extern crate proc_macro;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Attribute, Data, DataStruct, DeriveInput, Ident, Field, Fields};
use syn::punctuated::Punctuated;
use syn::token::Comma;

fn derive_struct(name: &Ident, fields: &Punctuated<Field, Comma>, attrs: &[Attribute]) -> TokenStream {
    // TODO: Generics
    let insert_fields = fields.iter().map(|field| {
        let ident = field.ident.as_ref().expect("A struct with anonymous field?!");
        let ident = format!("{}", ident);
        let ty = &field.ty;

        quote! {
            let field = ::structdoc::Field::new(
                <#ty as ::structdoc::StructDoc>::document().0,
                "TODO",
            );
            fields.insert(#ident.into(), field);
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
        Data::Struct(DataStruct { fields: Fields::Named(fields), .. }) => {
            derive_struct(name, &fields.named, &input.attrs)
        },
        _ => unimplemented!("Only named structs and enums for now :-("),
    }.into()
}
