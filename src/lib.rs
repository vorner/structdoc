use std::borrow::Cow;
use std::collections::HashMap;

mod impls;

use bitflags::bitflags;

pub type Text = Cow<'static, str>;

// TODO: Hide something of the public stuff?

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
    pub fn new(node: Node, doc: impl Into<Text>) -> Self {
        Field {
            doc: doc.into(),
            node,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Node {
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

#[derive(Clone, Debug)]
pub struct Documentation(Node);

impl Documentation {
    pub fn leaf() -> Documentation {
        Documentation(Node::Leaf)
    }
}

pub trait StructDoc {
    fn document() -> Documentation;
}
