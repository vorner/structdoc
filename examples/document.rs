use structdoc::StructDoc;

#[derive(StructDoc)]
struct Stuff {
    #[structdoc(something)]
    a: i32,
    b: Vec<String>,
}

fn main() {
    println!("{:?}", Stuff::document());
}
