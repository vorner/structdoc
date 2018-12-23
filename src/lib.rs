use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::mem;

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

#[derive(Clone, Debug)]
pub enum Arity {
    One,
    ManyOrdered,
    ManyUnordered,
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
}

#[derive(Clone, Debug)]
enum Node {
    Leaf,
    Wrapper{
        child: Box<Node>,
        arity: Arity,
        flags: Flags,
    },
    Map {
        key: Box<Node>,
        value: Box<Node>,
    },
    Struct(HashMap<Text, Field>),
    Enum(HashMap<Text, Field>),
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
        Documentation(Node::Struct(fields.into_iter().map(|(t, f)| (t.into(), f)).collect()))
    }
    pub fn enum_(variants: impl IntoIterator<Item = (impl Into<Text>, Field)>) -> Self {
        Documentation(Node::Enum(variants.into_iter().map(|(t, f)| (t.into(), f)).collect()))
    }
}

impl Display for Documentation {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "Dummy")
    }
}

pub trait StructDoc {
    fn document() -> Documentation;
}
