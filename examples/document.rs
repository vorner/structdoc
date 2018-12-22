use serde_derive::Deserialize;
use structdoc::StructDoc;

#[derive(Deserialize)]
enum Undocumented {
    A,
}

#[derive(StructDoc, Deserialize)]
#[structdoc(rename_all = "SCREAMING-KEBAB-CASE")]
struct Stuff {
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
}

fn main() {
    println!("{:?}", Stuff::document());
}
