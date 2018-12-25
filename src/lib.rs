#![doc(
    html_root_url = "https://docs.rs/structdoc/0.1.1/structdoc/",
    test(attr(deny(warnings)))
)]
#![forbid(unsafe_code)]
#![warn(missing_docs)]

//! Extract documentation out of types and make use of it at runtime.
//!
//! The [`StructDoc`] trait describes types which know their own documentation at runtime. It can
//! be derived (see the [`StructDoc`] documentation for deriving details). The [`Documentation`] is
//! a type holding the actual documentation.
//!
//! # Motivation
//!
//! Sometimes, an application needs some structured input from the user ‒ configuration, input
//! files, etc. Therefore, the format needs to be documented somehow. But doing so manually has
//! several disadvantages:
//!
//! * Manual documentation tends to be out of sync.
//! * It needs additional manual work.
//! * If parts of the structure come from different parts of the application or even different
//!   libraries, the documentation needs to either be collected from all these places or written
//!   manually at a different place (making the chance of forgetting to update it even higher).
//!
//! This crate tries to help with that ‒ it allows extracting doc strings and composing them
//! together to form the documentation automatically, using procedural derive. The structure is
//! guaranteed to match and the documentation strings are much more likely to be updated, as they
//! are close to the actual definitions being changed.
//!
//! It is able to use both its own and [`serde`]'s attributes, because [`serde`] is very commonly
//! used to read the structured data.
//!
//! # Examples
//!
//! ```rust
//! # #![allow(dead_code)]
//! use std::num::NonZeroU32;
//!
//! use serde_derive::Deserialize;
//! use structdoc::StructDoc;
//!
//! #[derive(Deserialize, StructDoc)]
//! struct Point {
//!     /// The horizontal position.
//!     x: i32,
//!
//!     /// The vertical position.
//!     y: i32,
//! }
//!
//! #[derive(Deserialize, StructDoc)]
//! struct Circle {
//!     // Will flatten both on the serde side and structdoc, effectively creating a structure with
//!     // 3 fields for both of them.
//!     #[serde(flatten)]
//!     center: Point,
//!
//!     /// The diameter of the circle.
//!     diameter: NonZeroU32,
//! }
//!
//! println!("{}", Circle::document());
//! ```
//!
//! # TODO
//!
//! This crate is young and has some missing things:
//!
//! * Probably some corner-cases are not handled properly. Also, not everything that can derive
//!   [`Deserialize`] can derive [`StructDoc`] yet.
//! * Some ability to manually traverse the documentation.
//! * Allow tweaking how the documentation is printed.
//! * Proper tests.
//! * Error handling during derive ‒ the error messages would need some improvements and some
//!   things are simply ignored. Furthermore, if you specify some nonsensical combination of
//!   attributes, you're as likely to get some garbage documentation out instead of error.
//! * There are plans to provide implementations for types from other crates, under feature flags.
//!
//! In other words, let this crate generate the documentation, but skim the result before shipping
//! to make sure it is correct and makes sense. Pull requests to fix bugs are indeed welcome.
//!
//! [`serde`]: https://serde.rs
//! [`Deserialize`]: https://docs.rs/serde/~1/serde/trait.Deserialize.html

use std::borrow::Cow;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::mem;

use itertools::Itertools;

mod impls;

use bitflags::bitflags;

#[cfg(feature = "structdoc-derive")]
pub use structdoc_derive::StructDoc;

/// Text representation.
///
/// Many things inside here can take either owned strings or string literals.
pub type Text = Cow<'static, str>;

bitflags! {
    /// Flags on nodes of [`Documentation`].
    ///
    /// Can be put onto a documentation node with [`Documentation::set_flag`].
    pub struct Flags: u8 {
        /// Flatten structure into a parent.
        ///
        /// For structure field inside a structure, this skips the one level and puts all the inner
        /// fields directly inside the outer struct.
        ///
        /// For enums inside structs, this suggests that the fields are merged inline the outer
        /// struct, but still keeps the separation inside the documentation.
        const FLATTEN  = 0b0001;

        /// This part of documentation should be hidden.
        const HIDE     = 0b0010;

        /// The presence of this field is optional.
        ///
        /// This may be caused either by it to reasonably contain a no-value (eg. `Option<T>`,
        /// `Vec<T>`) or by having a default value. Any possible default value should be described
        /// in the doc comment.
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

/// An arity of an container.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Arity {
    /// Contains one thing.
    ///
    /// Or, at most one, in case it is also optional.
    One,

    /// Multiple things of the same kind, preserving order.
    ManyOrdered,

    /// Multiple things of the same kind, without specified order.
    ManyUnordered,
}

/// A tagging of an enum.
///
/// Corresponds to the [serde enum representations](https://serde.rs/enum-representations.html).
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Tagging {
    #[allow(missing_docs)]
    Untagged,

    #[allow(missing_docs)]
    External,

    #[allow(missing_docs)]
    Internal { tag: String },

    #[allow(missing_docs)]
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

/// A documentation node with actual documentation text.
#[derive(Clone, Debug)]
pub struct Field {
    doc: Text,
    node: Node,
}

impl Field {
    /// Creates a field from (undocumented) documentation node and the documentation text.
    ///
    /// This is the proper way to add descriptions to struct fields and enum variants.
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
                    .filter(|entry| !entry.processing.contains(Processing::HIDE))
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

/// A representation of documentation.
///
/// This carries the internal representation (tree) of a documentation. Note that currently this
/// does not support cycles or referencing other branches.
///
/// This can be either queried by the [`StructDoc`] trait, or manually constructed (which might be
/// needed in a manual implementation of the trait).
///
/// # TODO
///
/// Currently, the documentation can be formatted both with the [`Debug`][std::fmt::Debug] and
/// [`Display`][std::fmt::Display] traits, but doesn't offer any kind of customization. In the
/// future it should be possible to both traverse the structure manually and to customize the way
/// the documentation is formatted.
#[derive(Clone, Debug)]
pub struct Documentation(Node);

impl Documentation {
    /// Creates a leaf node of the documentation, without any description.
    pub fn leaf_empty() -> Documentation {
        Documentation(Node::Leaf(Text::default()))
    }

    /// Creates a leaf node with the given type.
    ///
    /// Note that an empty `ty` is equivalent to the [`leaf_empty`][Documentation::leaf_empty].
    pub fn leaf(ty: impl Into<Text>) -> Documentation {
        Documentation(Node::Leaf(ty.into()))
    }

    /// Adds a flag to this documentation node.
    pub fn set_flag(&mut self, flag: Flags) {
        self.0.set_flag(flag);
    }

    /// Wraps a node into an array or a set.
    ///
    /// This describes a homogeneous collection.
    pub fn with_arity(self, arity: Arity) -> Self {
        Documentation(Node::Wrapper {
            child: Box::new(self.0),
            arity,
            flags: Flags::empty(),
        })
    }

    /// Builds a map.
    ///
    /// Joins documentation of keys and values into a map. Note that all the keys and all the
    /// values are of the same type ‒ for heterogeneous things, you might want structs or enums.
    pub fn map(key: Documentation, value: Documentation) -> Self {
        Documentation(Node::Map {
            key: Box::new(key.0),
            value: Box::new(value.0),
        })
    }

    /// Builds a struct.
    ///
    /// Builds a structure, provided a list of fields.
    ///
    /// The iterator should yield pairs of (name, field).
    pub fn struct_(fields: impl IntoIterator<Item = (impl Into<Text>, Field)>) -> Self {
        Documentation(Node::Struct(
            fields.into_iter().map(|(t, f)| (t.into(), f)).collect(),
        ))
    }

    /// Builds an enum.
    ///
    /// Builds an enum from provided list of fields. The fields may be either leaves (without
    /// things inside ‒ created with eg. [`leaf_empty`][Documentation::leaf_empty]), newtypes
    /// (other leaves) or structs. The iterator should yield pairs of (name, variant).
    ///
    /// See the [serde documentation about enum
    /// representations](https://serde.rs/enum-representations.html) for `tagging`.
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

/// Types that can provide their own documentation at runtime.
///
/// It is provided for basic types and containers in the standard library. It should be possible to
/// derive for most of the rest.
///
/// # Examples
///
/// ```
/// # #![allow(dead_code)]
/// use structdoc::StructDoc;
///
/// #[derive(StructDoc)]
/// struct Point {
///     /// The horizontal coordinate.
///     x: i32,
///
///     /// The vertical coordinate.
///     y: i32,
/// }
///
/// let documentation = format!("{}", Point::document());
/// let expected = r#"<root> (Struct):
///     Field x (Integer):
///       The horizontal coordinate.
///     Field y (Integer):
///       The vertical coordinate.
/// "#;
///
/// assert_eq!(expected, documentation);
/// ```
///
/// # Deriving the trait
///
/// If the `structdoc-derive` feature is enabled (it is by default), it is possible to derive the
/// trait on structs and enums. The text of documentation is extracted from the doc comments.
/// Furthermore, it allows tweaking the implementation by attributes.
///
/// Because the primary aim of this crate is to provide user documentation for things fed to the
/// application a lot of such things are handled by the [`serde`] crate, our derive can use both
/// its own attributes and `serde` ones where it makes sense.
///
/// ## Ignoring fields and variants
///
/// They can be ignored by placing either `#[doc(hidden)]`, `#[serde(skip)]`,
/// `#[serde(skip_deserialize)]` or `#[structdoc(skip)]` attributes on them.
///
/// ## Stubbing out implementations
///
/// If a field's type doesn't implement the trait or if recursing into it is not wanted (or maybe
/// because the data structure is cyclic), it can be prefixed with the `#[structdoc(leaf)]` or
/// `#[structdoc(leag = "Type")]` attribute. It'll provide trivial implementation without any
/// explanation and the provided type in parenthesis, if one is provided.
///
/// Alternatively, a function `fn() -> Documentation` can be plugged in using the
/// `#[structdoc(with = "path::to::the_fn")]`. That can return an arbitrary implementation.
///
/// ## Renaming things
///
/// The `rename` and `rename_all` attributes are available, both in `serde` and `structdoc`
/// variants. They have the same meaning as withing serde.
///
/// ## Flattening
///
/// The `#[serde(flatten)]` and `#[structdoc(flatten)]` flattens structures inline.
///
/// ## Enum representations
///
/// The serde (and `structdoc` alternatives) of [tag representation] attributes are available.
///
/// [`serde`]: https://crates.io/crates/serde
/// [`tag representation]: https://serde.rs/container-attrs.html#tag
pub trait StructDoc {
    /// Returns the documentation for the type.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use structdoc::StructDoc;
    ///
    /// println!("Documentation: {}", Vec::<Option<String>>::document());
    /// ```
    fn document() -> Documentation;
}
