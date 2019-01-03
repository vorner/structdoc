// We don't actually use the structures below, just document them, which makes the compiler a bit
// unhappy.
#![allow(dead_code)]
use serde_derive::Deserialize;
use structdoc::{Documentation, StructDoc};

#[derive(StructDoc)]
struct A<T>(T);

#[derive(StructDoc, Deserialize)]
#[structdoc(rename_all = "SCREAMING-KEBAB-CASE")]
struct Stuff<T>
where
    T: Clone,
{
    /// How many times to say hello
    #[structdoc(rename = "hello")]
    #[serde(default)]
    a: i32,

    /// Some extra chatter.
    ///
    /// Appended to the stuff.
    #[serde(default)]
    b: Vec<String>,

    #[structdoc(skip, leaf)]
    und: Undocumented,

    /// Select one of these, please
    selection: Selection,

    #[serde(flatten)]
    sub: T,

    #[structdoc(with = "gen_doc")]
    flag: bool,
}

#[derive(Deserialize)]
enum Undocumented {
    A,
}

#[derive(Deserialize, StructDoc)]
#[serde(tag = "type", rename_all = "camelCase")]
enum Selection {
    A {
        a: i32,
        b: i32,
    },
    X {
        x: String,
    },
    #[structdoc(leaf)]
    AnotherThing(Undocumented),
    FooBar(String),
    #[serde(skip)]
    Other,
}

fn gen_doc() -> Documentation {
    Documentation::leaf("Manual implementation")
}

#[derive(Clone, StructDoc, Deserialize)]
struct Inner {
    /// A bool
    c: bool,
}

fn main() {
    let documentation = Stuff::<Inner>::document();
    println!("{:?}", documentation);
    println!("{}", documentation);
}
