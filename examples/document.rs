use serde_derive::Deserialize;
use structdoc::StructDoc;

#[derive(Deserialize)]
enum Undocumented {
    A,
}

#[derive(Deserialize, StructDoc)]
#[serde(untagged, rename_all = "camelCase")]
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
    Other,
}

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
    b: Vec<String>,

    #[structdoc(skip, leaf)]
    und: Undocumented,

    /// Select one of these, please
    selection: Selection,

    #[serde(flatten)]
    sub: T,
}

#[derive(Clone, StructDoc, Deserialize)]
struct Inner {
    /// A bool
    c: bool,
}

fn main() {
    println!("{:?}", Stuff::<Inner>::document());
}
