use std::borrow::Cow;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::mem;

use itertools::Itertools;

mod impls;

use bitflags::bitflags;

#[cfg(feature = "structdoc-derive")]
pub use structdoc_derive::StructDoc;

pub type Text = Cow<'static, str>;

bitflags! {
    pub struct Flags: u8 {
        const FLATTEN  = 0b0001;
        const HIDE     = 0b0010;
        const OPTIONAL = 0b0100;
    }
}

bitflags! {
    #[derive(Default)]
    struct Processing: u8 {
        const SORT    = 0b0001;
        const HIDE    = 0b0010;
        const FLATTEN = 0b0100;
        const STRUCT  = 0b1000;
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Arity {
    One,
    ManyOrdered,
    ManyUnordered,
}

#[derive(Debug, Default, Eq, PartialEq, Ord, PartialOrd)]
struct Entry {
    caption: String,
    text: Vec<String>,
    flags: Vec<&'static str>,
    sub: Vec<Entry>,
    processing: Processing,
}

impl Entry {
    fn sort(&mut self) {
        for sub in &mut self.sub {
            sub.sort();
        }
        if self.processing.contains(Processing::SORT) {
            self.sub.sort();
        }
    }

    fn print(&self, fmt: &mut Formatter, indent: &mut String) -> FmtResult {
        let flags = if self.flags.is_empty() {
            String::new()
        } else {
            format!(" ({})", self.flags.iter().rev().join(", "))
        };
        let colon = if self.is_empty() { ' ' } else { ':' };
        writeln!(fmt, "{}{}{}{}", indent, self.caption, flags, colon)?;
        indent.push_str("  ");
        for line in &self.text {
            writeln!(fmt, "{}{}", indent, line)?;
        }
        indent.push_str("  ");
        for sub in &self.sub {
            sub.print(fmt, indent)?;
        }
        assert!(indent.pop().is_some());
        assert!(indent.pop().is_some());
        assert!(indent.pop().is_some());
        assert!(indent.pop().is_some());
        Ok(())
    }

    fn is_empty(&self) -> bool {
        self.caption.is_empty() && self.text.is_empty() && self.sub.is_empty()
    }
}

#[derive(Clone, Debug)]
pub struct Field {
    doc: Text,
    node: Node,
}

impl Field {
    pub fn new(inner: Documentation, doc: impl Into<Text>) -> Self {
        Field {
            doc: doc.into(),
            node: inner.0,
        }
    }

    fn entry(&self, name: String) -> Entry {
        let mut entry = self.node.entry();
        if !self.doc.is_empty() {
            entry.text.extend(self.doc.lines().map(str::to_owned));
        }
        entry.caption = name;
        entry
    }
}

#[derive(Clone, Debug)]
enum Node {
    Leaf,
    Wrapper {
        child: Box<Node>,
        arity: Arity,
        flags: Flags,
    },
    Map {
        key: Box<Node>,
        value: Box<Node>,
    },
    Struct(Vec<(Text, Field)>),
    Enum(Vec<(Text, Field)>),
}

impl Node {
    fn set_flag(&mut self, flag: Flags) {
        if let Node::Wrapper { ref mut flags, .. } = self {
            *flags |= flag;
        } else {
            let mut old = Node::Leaf;
            mem::swap(&mut old, self);
            *self = Node::Wrapper {
                child: Box::new(old),
                flags: flag,
                arity: Arity::One,
            };
        }
    }

    fn entry(&self) -> Entry {
        match self {
            Node::Leaf => Entry::default(),
            Node::Wrapper {
                child,
                flags,
                arity,
            } => {
                let mut child_entry = child.entry();
                match arity {
                    Arity::One => (),
                    Arity::ManyOrdered => child_entry.flags.push("Array"),
                    Arity::ManyUnordered => child_entry.flags.push("Set"),
                }
                if flags.contains(Flags::OPTIONAL) {
                    child_entry.flags.push("Optional");
                }
                if flags.contains(Flags::FLATTEN) && *arity == Arity::One {
                    child_entry.processing |= Processing::FLATTEN;
                }
                if flags.contains(Flags::HIDE) {
                    child_entry.processing |= Processing::HIDE;
                }
                child_entry
            }
            Node::Map { key, value } => {
                let mut entry = Entry::default();
                entry.text.push("Map:".to_owned());
                let mut key = key.entry();
                if !key.is_empty() {
                    key.caption = "Keys:".to_owned();
                    entry.sub.push(key);
                }
                let mut value = value.entry();
                if !value.is_empty() {
                    value.caption = "Values:".to_owned();
                    entry.sub.push(value);
                }
                entry
            }
            Node::Struct(fields) => {
                let mut sub = Vec::new();
                for (name, field) in fields {
                    let entry = field.entry(format!("Field {}", name));
                    if entry.processing.contains(Processing::HIDE) {
                        continue;
                    } else if entry.processing.contains(Processing::FLATTEN)
                        && entry.processing.contains(Processing::STRUCT)
                    {
                        sub.extend(entry.sub);
                    } else {
                        sub.push(entry);
                    }
                }
                Entry {
                    caption: String::new(),
                    text: Vec::new(),
                    flags: vec!["Struct"],
                    sub,
                    processing: Processing::SORT | Processing::STRUCT,
                }
            }
            Node::Enum(variants) => {
                let variants = variants
                    .iter()
                    .map(|(name, variant)| variant.entry(format!("Variant {}", name)));
                Entry {
                    caption: String::new(),
                    text: Vec::new(),
                    flags: vec!["Enum"],
                    sub: variants.collect(),
                    processing: Processing::SORT,
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Documentation(Node);

impl Documentation {
    pub fn leaf() -> Documentation {
        Documentation(Node::Leaf)
    }

    pub fn set_flag(&mut self, flag: Flags) {
        self.0.set_flag(flag);
    }

    pub fn with_arity(self, arity: Arity) -> Self {
        Documentation(Node::Wrapper {
            child: Box::new(self.0),
            arity,
            flags: Flags::empty(),
        })
    }
    pub fn map(key: Documentation, value: Documentation) -> Self {
        Documentation(Node::Map {
            key: Box::new(key.0),
            value: Box::new(value.0),
        })
    }
    pub fn struct_(fields: impl IntoIterator<Item = (impl Into<Text>, Field)>) -> Self {
        Documentation(Node::Struct(
            fields.into_iter().map(|(t, f)| (t.into(), f)).collect(),
        ))
    }
    pub fn enum_(variants: impl IntoIterator<Item = (impl Into<Text>, Field)>) -> Self {
        Documentation(Node::Enum(
            variants.into_iter().map(|(t, f)| (t.into(), f)).collect(),
        ))
    }
}

impl Display for Documentation {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        let mut entry = self.0.entry();
        entry.sort();
        entry.caption = "<root>".to_owned();
        let mut indent = String::new();
        entry.print(fmt, &mut indent)
    }
}

pub trait StructDoc {
    fn document() -> Documentation;
}
