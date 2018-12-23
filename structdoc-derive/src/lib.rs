extern crate proc_macro;

use std::iter;

use either::Either;
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::quote;
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{
    Attribute, Data, DataStruct, DeriveInput, Field, Fields, Ident, Lit, Meta, MetaList,
    MetaNameValue, NestedMeta,
};

#[derive(Eq, PartialEq)]
enum RenameMode {
    Lower,
    Upper,
    Pascal,
    Camel,
    Snake,
    ScreamingSnake,
    Kebab,
    ScreamingKebab,
}

impl RenameMode {
    fn apply(&self, s: &str) -> String {
        use self::RenameMode::*;
        use heck::*;
        match self {
            Lower => s.to_ascii_lowercase(),
            Upper => s.to_ascii_uppercase(),
            // Note that serde's and heck's definitions differ a bit (we have serde's in our API)
            Pascal => s.to_camel_case(),
            Camel => s.to_mixed_case(),
            Snake => s.to_snake_case(),
            ScreamingSnake => s.to_snake_case().to_ascii_uppercase(),
            Kebab => s.to_kebab_case(),
            ScreamingKebab => s.to_kebab_case().to_ascii_uppercase(),
        }
    }
}

impl From<&str> for RenameMode {
    fn from(s: &str) -> RenameMode {
        use self::RenameMode::*;
        match s {
            "lowercase" => Lower,
            "UPPERCASE" => Upper,
            "PascalCase" => Pascal,
            "camelCase" => Camel,
            "snake_case" => Snake,
            "SCREAMING_SNAKE_CASE" => ScreamingSnake,
            "kebab-case" => Kebab,
            "SCREAMING-KEBAB-CASE" => ScreamingKebab,
            s => panic!("Unknown rename-all value {}", s),
        }
    }
}

#[derive(Eq, PartialEq)]
enum Attr {
    Hidden,
    Flatten,
    Leaf,
    Default,
    Doc(String),
    RenameAll(RenameMode),
    Rename(String),
    // TODO: Tagging of enums
}

fn parse_word(outer: &Ident, inner: &Ident) -> Option<Attr> {
    match (outer.to_string().as_ref(), inner.to_string().as_ref()) {
        ("doc", "hidden") |
        ("serde", "skip") |
        ("serde", "skip_deserializing") |
        ("structdoc", "skip") => Some(Attr::Hidden),
        ("serde", "flatten") |
        ("structdoc", "flatten") => Some(Attr::Flatten),
        ("serde", "default") |
        ("structdoc", "default") => Some(Attr::Default),
        ("structdoc", "leaf") => Some(Attr::Leaf),
        ("structdoc", attr) => panic!("Unknown structdoc attribute {}", attr),
        // TODO: serde-untagged
        // TODO: Does serde-transparent mean anything to us?
        // Serde or rustc will validate doc and serde attributes, we don't hope to know them all.
        _ => None,
    }
}

fn parse_name_value(outer: &Ident, inner: &Ident, value: &Lit) -> Option<Attr> {
    match (outer.to_string().as_ref(), inner.to_string().as_ref(), value) {
        ("serde", "rename_all", Lit::Str(s)) |
        ("structdoc", "rename_all", Lit::Str(s)) => {
            Some(Attr::RenameAll(RenameMode::from(&s.value() as &str)))
        }
        ("serde", "rename_all", _) |
        ("structdoc", "rename_all", _) => panic!("rename-all expects string"),
        ("serde", "rename", Lit::Str(s)) |
        ("structdoc", "rename", Lit::Str(s)) => Some(Attr::Rename(s.value())),
        ("structdoc", name, _) => panic!("Unknown strucdoc attribute {}", name),
        _ => None,
    }
}

fn parse_nested_meta(
    ident: Ident,
    nested: impl IntoIterator<Item = NestedMeta>,
) -> impl Iterator<Item = Attr> {
    nested.into_iter().filter_map(move |nm| match nm {
        NestedMeta::Meta(Meta::Word(word)) => parse_word(&ident, &word),
        NestedMeta::Meta(Meta::NameValue(MetaNameValue { ident: name, lit: value, .. })) => {
            parse_name_value(&ident, &name, &value)
        }
        _ => panic!("Confused by attribute syntax"),
    })
}

fn parse_attrs(attrs: &[Attribute]) -> Vec<Attr> {
    attrs
        .iter()
        // Filter the doc, structdoc and serde attributes
        .filter(|attr| {
            attr.path.is_ident("structdoc")
                || attr.path.is_ident("doc")
                || attr.path.is_ident("serde")
        })
        // Interpret each as meta (all of them should be fine)
        .map(|attr| attr.interpret_meta().expect("Unparsable attribute"))
        .flat_map(|meta| match meta {
            Meta::List(MetaList { ident, nested, .. }) => {
                Either::Left(parse_nested_meta(ident, nested))
            }
            Meta::NameValue(MetaNameValue { ident, lit, .. }) => {
                assert_eq!(ident, "doc", "Broken attribute");
                if let Lit::Str(string) = lit {
                    Either::Right(iter::once(Attr::Doc(string.value())))
                } else {
                    panic!("Invalid doc text (must be string)");
                }
            }
            _ => panic!("Wrong attribute"),
        })
        .collect()
}

fn mangle_name(name: &Ident, container_attrs: &[Attr], field_attrs: &[Attr]) -> String {
    for attr in field_attrs {
        if let Attr::Rename(name) = attr {
            return name.clone();
        }
    }
    for attr in container_attrs {
        if let Attr::RenameAll(mode) = attr {
            return mode.apply(&name.to_string());
        }
    }
    name.to_string()
}

fn get_doc(attrs: &[Attr]) -> String {
    let lines = iter::once(&Attr::Doc(String::new()))
        .chain(attrs)
        .filter_map(|a| {
            if let Attr::Doc(d) = a {
                Some(d)
            } else {
                None
            }
        })
        .join("\n");
    unindent::unindent(&lines)
}

fn get_mods(attrs: &[Attr]) -> TokenStream {
    let mut mods = TokenStream::new();
    if attrs.contains(&Attr::Default) {
        mods.extend(quote!(field.set_flag(::structdoc::Flags::OPTIONAL);));
    }
    if attrs.contains(&Attr::Flatten) {
        mods.extend(quote!(field.set_flag(::structdoc::Flags::FLATTEN);));
    }
    if attrs.contains(&Attr::Hidden) {
        mods.extend(quote!(field.set_flag(::structdoc::Flags::HIDE);));
    }
    mods
}

fn derive_struct(
    name: &Ident,
    fields: &Punctuated<Field, Comma>,
    attrs: &[Attribute],
) -> TokenStream {
    let struct_attrs = parse_attrs(attrs);
    // TODO: Validate the attributes make sense here
    // TODO: Generics
    let insert_fields = fields.iter().map(|field| {
        let ident = field
            .ident
            .as_ref()
            .expect("A struct with anonymous field?!");
        let field_attrs = parse_attrs(&field.attrs);
        let name = mangle_name(ident, &struct_attrs, &field_attrs);
        let ty = &field.ty;
        let doc = get_doc(&field_attrs);
        let mods = get_mods(&field_attrs);
        // We don't dive into hiddens, we just list them here. They don't need to have the
        // implementation.
        let is_leaf = field_attrs.contains(&Attr::Leaf) || field_attrs.contains(&Attr::Hidden);
        let field_document = if is_leaf {
            quote!(::structdoc::Node::Leaf)
        } else {
            quote!(<#ty as ::structdoc::StructDoc>::document().0)
        };

        quote! {
            let mut field = #field_document;
            #mods
            let field = ::structdoc::Field::new(field, #doc);
            fields.insert(#name.into(), field);
        }
    });

    quote! {
        impl ::structdoc::StructDoc for #name {
            fn document() -> ::structdoc::Documentation {
                let mut fields = ::std::collections::HashMap::new();
                #(#insert_fields)*
                ::structdoc::Documentation(::structdoc::Node::Struct(fields))
            }
        }
    }
}

// Note: We declare the structdoc attribute. But we also parasite on serde attribute if present.
#[proc_macro_derive(StructDoc, attributes(structdoc))]
pub fn structdoc_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: DeriveInput = syn::parse(input).unwrap();
    let name = &input.ident;
    match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => derive_struct(name, &fields.named, &input.attrs),
        _ => unimplemented!("Only named structs and enums for now :-("),
    }
    .into()
}
