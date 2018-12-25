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
        const SORT    = 0b0000_0001;
        const HIDE    = 0b0000_0010;
        const FLATTEN = 0b0000_0100;
        const STRUCT  = 0b0000_1000;
        const ENUM    = 0b0001_0000;
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Arity {
    One,
    ManyOrdered,
    ManyUnordered,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Tagging {
    Untagged,
    External,
    Internal { tag: String },
    Adjacent { tag: String, content: String },
}

#[derive(Debug, Default, Eq, PartialEq, Ord, PartialOrd)]
struct Entry {
    caption: String,
    text: Vec<String>,
    flags: Vec<Text>,
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
            let space = if self.caption.is_empty() { "" } else { " " };
            format!("{}({})", space, self.flags.iter().rev().join(", "))
        };
        let colon = if self.text.is_empty() && self.sub.is_empty() {
            ' '
        } else {
            ':'
        };
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

    fn entry(&self, prefix: &str, name: &str) -> Entry {
        let mut entry = self.node.entry();
        if !self.doc.is_empty() {
            entry.text.extend(self.doc.lines().map(str::to_owned));
        }
        entry.caption = format!("{}{}", prefix, name);
        entry
    }
}

#[derive(Clone, Debug)]
enum Node {
    Leaf(Text),
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
    Enum {
        variants: Vec<(Text, Field)>,
        tagging: Tagging,
    },
}

impl Node {
    fn set_flag(&mut self, flag: Flags) {
        if let Node::Wrapper { ref mut flags, .. } = self {
            *flags |= flag;
        } else {
            let mut old = Node::Leaf(Text::default());
            mem::swap(&mut old, self);
            *self = Node::Wrapper {
                child: Box::new(old),
                flags: flag,
                arity: Arity::One,
            };
        }
    }

    fn struct_from<'i, I>(fields: I) -> Entry
    where
        I: IntoIterator<Item = &'i (Text, Field)>,
    {
        let mut sub = Vec::new();
        for (name, field) in fields {
            let mut entry = field.entry("Field ", name);
            if entry.processing.contains(Processing::FLATTEN)
                && entry.processing.contains(Processing::ENUM)
            {
                entry.flags.push("Inlined to parent".into());
            }
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
            flags: vec!["Struct".into()],
            sub,
            processing: Processing::SORT | Processing::STRUCT,
        }
    }

    fn entry(&self) -> Entry {
        match self {
            Node::Leaf(ty) => {
                let flags = if ty.is_empty() {
                    Vec::new()
                } else {
                    vec![ty.clone()]
                };
                Entry {
                    flags,
                    ..Entry::default()
                }
            }
            Node::Wrapper {
                child,
                flags,
                arity,
            } => {
                let mut child_entry = child.entry();
                match arity {
                    Arity::One => (),
                    Arity::ManyOrdered => child_entry.flags.push("Array".into()),
                    Arity::ManyUnordered => child_entry.flags.push("Set".into()),
                }
                if flags.contains(Flags::OPTIONAL) {
                    child_entry.flags.push("Optional".into());
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
            Node::Struct(fields) => Self::struct_from(fields),
            Node::Enum { variants, tagging } => {
                let mut variants = variants
                    .iter()
                    .map(|(name, variant)| variant.entry("Variant ", name))
                    .collect::<Vec<_>>();
                let (ty, flags, cap) = match tagging {
                    Tagging::Untagged => {
                        for (num, variant) in variants.iter_mut().enumerate() {
                            variant.caption = format!("Variant #{}", num + 1);
                        }
                        (
                            "Anonymous alternatives (inline structs to parent level)",
                            Processing::empty(),
                            String::new(),
                        )
                    }
                    Tagging::External => ("One-of", Processing::SORT, String::new()),
                    Tagging::Internal { tag } => (
                        "Alternatives (inline other fields)",
                        Processing::SORT,
                        format!("Field {}", tag),
                    ),
                    Tagging::Adjacent { tag, content } => {
                        for (num, var) in variants.iter_mut().enumerate() {
                            let cap = var.caption.replacen("Variant ", "Constant ", 1);
                            let mut old_text = Vec::new();
                            mem::swap(&mut old_text, &mut var.text);
                            var.caption = format!("Field {}", content);
                            var.text = Vec::new();
                            let tag_field = Entry {
                                caption: cap,
                                text: Vec::new(),
                                flags: vec!["Variant selector".into()],
                                sub: Vec::new(),
                                processing: Processing::empty(),
                            };
                            let mut tmp = Entry::default();
                            mem::swap(&mut tmp, var);
                            *var = Entry {
                                caption: format!("Variant #{}", num + 1),
                                text: old_text,
                                flags: vec!["Struct".into()],
                                sub: vec![tag_field, tmp],
                                processing: Processing::STRUCT,
                            };
                        }
                        // TODO: What do we do with content?
                        ("Alternatives", Processing::SORT, tag.clone())
                    }
                };
                let inner = Entry {
                    caption: cap,
                    text: Vec::new(),
                    flags: vec![ty.into()],
                    sub: variants,
                    processing: flags | Processing::ENUM,
                };
                if inner.sub.iter().all(|sub| sub.sub.is_empty()) {
                    inner
                } else {
                    Entry {
                        caption: String::new(),
                        text: Vec::new(),
                        flags: vec!["Struct".into()],
                        sub: vec![inner],
                        processing: Processing::STRUCT,
                    }
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Documentation(Node);

impl Documentation {
    pub fn leaf_empty() -> Documentation {
        Documentation(Node::Leaf(Text::default()))
    }

    pub fn leaf(ty: impl Into<Text>) -> Documentation {
        Documentation(Node::Leaf(ty.into()))
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
    pub fn enum_(
        variants: impl IntoIterator<Item = (impl Into<Text>, Field)>,
        tagging: Tagging,
    ) -> Self {
        Documentation(Node::Enum {
            variants: variants.into_iter().map(|(t, f)| (t.into(), f)).collect(),
            tagging,
        })
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
