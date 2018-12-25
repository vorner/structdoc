extern crate proc_macro;

use std::iter;

use either::Either;
use itertools::Itertools;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::fold::{self, Fold};
use syn::punctuated::Punctuated;
use syn::token::{Colon2, Comma};
use syn::{
    Attribute, Data, DataEnum, DataStruct, DeriveInput, Field, Fields, Generics, Ident,
    LifetimeDef, Lit, Meta, MetaList, MetaNameValue, NestedMeta, Path, PathArguments, PathSegment,
    TraitBound, TraitBoundModifier, TypeParam, TypeParamBound, Variant,
};

#[derive(Clone, Eq, PartialEq)]
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

#[derive(Clone, Eq, PartialEq)]
enum Tag {
    Untagged,
    Internal { tag: String },
}

#[derive(Clone, Eq, PartialEq)]
enum Attr {
    Hidden,
    Flatten,
    Leaf(String),
    Default,
    Doc(String),
    RenameAll(RenameMode),
    Rename(String),
    Tag(Tag),
    TagContent(String),
    // TODO: with = function
    // TODO: Tagging of enums
}

fn parse_word(outer: &Ident, inner: &Ident) -> Option<Attr> {
    match (outer.to_string().as_ref(), inner.to_string().as_ref()) {
        ("doc", "hidden")
        | ("serde", "skip")
        | ("serde", "skip_deserializing")
        | ("structdoc", "skip") => Some(Attr::Hidden),
        ("serde", "flatten") | ("structdoc", "flatten") => Some(Attr::Flatten),
        ("serde", "default") | ("structdoc", "default") => Some(Attr::Default),
        ("structdoc", "leaf") => Some(Attr::Leaf(String::new())),
        ("serde", "untagged") | ("structdoc", "untagged") => Some(Attr::Tag(Tag::Untagged)),
        ("structdoc", attr) => panic!("Unknown structdoc attribute {}", attr),
        // TODO: serde-untagged
        // TODO: Does serde-transparent mean anything to us?
        // Serde or rustc will validate doc and serde attributes, we don't hope to know them all.
        _ => None,
    }
}

fn parse_name_value(outer: &Ident, inner: &Ident, value: &Lit) -> Option<Attr> {
    match (
        outer.to_string().as_ref(),
        inner.to_string().as_ref(),
        value,
    ) {
        ("serde", "rename_all", Lit::Str(s)) | ("structdoc", "rename_all", Lit::Str(s)) => {
            Some(Attr::RenameAll(RenameMode::from(&s.value() as &str)))
        }
        ("serde", "rename_all", _) | ("structdoc", "rename_all", _) => {
            panic!("rename-all expects string")
        }
        ("serde", "rename", Lit::Str(s)) | ("structdoc", "rename", Lit::Str(s)) => {
            Some(Attr::Rename(s.value()))
        }
        ("serde", "rename", _) | ("structdoc", "rename", _) => panic!("rename expects string"),
        ("structdoc", "leaf", Lit::Str(s)) => Some(Attr::Leaf(s.value())),
        ("structdoc", "leaf", _) => panic!("leaf expects string"),
        ("serde", "tag", Lit::Str(s)) | ("structdoc", "tag", Lit::Str(s)) => {
            Some(Attr::Tag(Tag::Internal { tag: s.value() }))
        }
        ("serde", "tag", _) | ("structdoc", "tag", _) => panic!("tag expects string"),
        ("serde", "content", Lit::Str(s)) | ("structdoc", "content", Lit::Str(s)) => {
            Some(Attr::TagContent(s.value()))
        }
        ("serde", "content", _) | ("structdoc", "content", _) => panic!("content expects string"),
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
        NestedMeta::Meta(Meta::NameValue(MetaNameValue {
            ident: name,
            lit: value,
            ..
        })) => parse_name_value(&ident, &name, &value),
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
        .filter_map(|a| if let Attr::Doc(d) = a { Some(d) } else { None })
        .join("\n");
    unindent::unindent(&lines)
}

fn get_mods(what: &Ident, attrs: &[Attr]) -> TokenStream {
    let mut mods = TokenStream::new();
    if attrs.contains(&Attr::Default) {
        mods.extend(quote!(#what.set_flag(::structdoc::Flags::OPTIONAL);));
    }
    if attrs.contains(&Attr::Flatten) {
        mods.extend(quote!(#what.set_flag(::structdoc::Flags::FLATTEN);));
    }
    if attrs.contains(&Attr::Hidden) {
        mods.extend(quote!(#what.set_flag(::structdoc::Flags::HIDE);));
    }
    mods
}

fn leaf(ty: &str) -> TokenStream {
    quote!(::structdoc::Documentation::leaf(#ty))
}

fn find_leaf(attrs: &[Attr]) -> Option<&str> {
    for attr in attrs {
        if let Attr::Leaf(s) = attr {
            return Some(s);
        }
    }
    None
}

fn named_field(field: &Field, container_attrs: &[Attr]) -> TokenStream {
    let ident = field
        .ident
        .as_ref()
        .expect("A struct with anonymous field?!");
    let field_attrs = parse_attrs(&field.attrs);
    let name = mangle_name(ident, &container_attrs, &field_attrs);
    let ty = &field.ty;
    let doc = get_doc(&field_attrs);
    let mods = get_mods(&Ident::new("field", Span::call_site()), &field_attrs);
    // We don't dive into hiddens, we just list them here. They don't need to have the
    // implementation.
    let found_leaf = find_leaf(&field_attrs);
    let is_leaf = found_leaf.is_some() || field_attrs.contains(&Attr::Hidden);
    let field_document = if is_leaf {
        leaf(found_leaf.unwrap_or_default())
    } else {
        quote!(<#ty as ::structdoc::StructDoc>::document())
    };

    quote! {
        let mut field = #field_document;
        #mods
        let field = ::structdoc::Field::new(field, #doc);
        fields.push((#name.into(), field));
    }
}

fn derive_struct(fields: &Punctuated<Field, Comma>, attrs: &[Attribute]) -> TokenStream {
    let struct_attrs = parse_attrs(attrs);
    // TODO: Validate the attributes make sense here
    let insert_fields = fields.iter().map(|field| named_field(field, &struct_attrs));

    quote! {
        let mut fields = ::std::vec::Vec::<(&str, ::structdoc::Field)>::new();
        #(#insert_fields)*
        ::structdoc::Documentation::struct_(fields)
    }
}

fn find_tag(attrs: &[Attr]) -> Option<&Tag> {
    for attr in attrs {
        if let Attr::Tag(tag) = attr {
            return Some(tag);
        }
    }
    None
}

fn find_tag_content(attrs: &[Attr]) -> Option<&str> {
    for attr in attrs {
        if let Attr::TagContent(s) = attr {
            return Some(s);
        }
    }
    None
}

fn derive_enum(variants: &Punctuated<Variant, Comma>, attrs: &[Attribute]) -> TokenStream {
    let enum_attrs = parse_attrs(attrs);
    let insert_varianst = variants.iter().map(|variant| {
        let variant_attrs = parse_attrs(&variant.attrs);
        let name = mangle_name(&variant.ident, &enum_attrs, &variant_attrs);
        let doc = get_doc(&variant_attrs);
        let mods = get_mods(&Ident::new("variant", Span::call_site()), &variant_attrs);
        let found_leaf = find_leaf(&variant_attrs);
        let is_leaf = found_leaf.is_some() || variant_attrs.contains(&Attr::Hidden);
        let constructor = if is_leaf {
            leaf(found_leaf.unwrap_or_default())
        } else {
            match &variant.fields {
                Fields::Unit => leaf(""),
                Fields::Named(fields) => {
                    let mut attrs = Vec::new();
                    attrs.extend(variant_attrs);
                    attrs.extend(enum_attrs.clone());
                    let insert_fields = fields.named.iter().map(|field| named_field(field, &attrs));
                    quote! {
                        {
                            let mut fields =
                                ::std::vec::Vec::<(&str, ::structdoc::Field)>::new();
                            #(#insert_fields)*
                            ::structdoc::Documentation::struct_(fields)
                        }
                    }
                }
                Fields::Unnamed(fields) if fields.unnamed.is_empty() => leaf(""),
                Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                    let ty = &fields.unnamed[0].ty;
                    quote!(<#ty as ::structdoc::StructDoc>::document())
                }
                Fields::Unnamed(fields) => {
                    panic!(
                        "Don't know what to do with tuple variant with {} fields",
                        fields.unnamed.len(),
                    );
                }
            }
        };
        quote! {
            let mut variant = #constructor;
            #mods
            let variant = ::structdoc::Field::new(variant, #doc);
            variants.push((#name.into(), variant));
        }
    });

    let tag = match (find_tag(&enum_attrs), find_tag_content(&enum_attrs)) {
        (None, _) => quote!(External),
        (Some(Tag::Internal { tag }), Some(content)) => {
            quote!(Adjacent { tag: #tag.to_owned(), content: #content.to_owned() })
        }
        (Some(Tag::Internal { tag }), _) => quote!(Internal { tag: #tag.to_owned() }),
        (Some(Tag::Untagged), _) => quote!(Untagged),
    };

    quote! {
        let mut variants = ::std::vec::Vec::<(&str, ::structdoc::Field)>::new();
        #(#insert_varianst)*
        ::structdoc::Documentation::enum_(variants, ::structdoc::Tagging::#tag)
    }
}

fn filter_generic_bounds(generics: &Generics) -> Generics {
    struct Filter;

    // TODO: Attribute to allow not adding the generic bound

    impl Fold for Filter {
        fn fold_type_param(&mut self, mut p: TypeParam) -> TypeParam {
            p.eq_token.take();
            p.default.take();
            let mut segments = Punctuated::<PathSegment, Colon2>::new();
            segments.push(PathSegment {
                ident: Ident::new("structdoc", Span::call_site()),
                arguments: PathArguments::None,
            });
            segments.push(PathSegment {
                ident: Ident::new("StructDoc", Span::call_site()),
                arguments: PathArguments::None,
            });
            p.bounds.push(TypeParamBound::Trait(TraitBound {
                paren_token: None,
                modifier: TraitBoundModifier::None,
                lifetimes: None,
                path: Path {
                    leading_colon: Some(Colon2::default()),
                    segments,
                },
            }));
            p
        }
    }

    fold::fold_generics(&mut Filter, generics.clone())
}

fn filter_generic_types(generics: &Generics) -> Generics {
    struct Filter;

    impl Fold for Filter {
        fn fold_type_param(&mut self, mut p: TypeParam) -> TypeParam {
            p.eq_token.take();
            p.colon_token.take();
            p.default.take();
            p.attrs.clear();
            p.bounds = Punctuated::new();
            p
        }
        fn fold_lifetime_def(&mut self, mut d: LifetimeDef) -> LifetimeDef {
            d.attrs.clear();
            d.colon_token.take();
            d.bounds = Punctuated::new();
            d
        }
    }

    fold::fold_generics(&mut Filter, generics.clone())
}

fn derive_transparent(field: &Field) -> TokenStream {
    let ty = &field.ty;
    quote!(<#ty as ::structdoc::StructDoc>::document())
}

// Note: We declare the structdoc attribute. But we also parasite on serde attribute if present.
#[proc_macro_derive(StructDoc, attributes(structdoc))]
pub fn structdoc_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: DeriveInput = syn::parse(input).unwrap();
    let name = &input.ident;
    let generic_bounds = filter_generic_bounds(&input.generics);
    let generic_types = filter_generic_types(&input.generics);
    let where_clause = input.generics.where_clause;
    let inner = match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => derive_struct(&fields.named, &input.attrs),
        Data::Struct(DataStruct {
            fields: Fields::Unnamed(ref fields),
            ..
        }) if fields.unnamed.len() == 1 => derive_transparent(&fields.unnamed[0]),
        Data::Enum(DataEnum { variants, .. }) => derive_enum(&variants, &input.attrs),
        _ => unimplemented!("Only named structs and enums for now :-("),
    };
    (quote! {
        impl #generic_bounds ::structdoc::StructDoc for #name #generic_types
        #where_clause
        {
            fn document() -> ::structdoc::Documentation {
                #inner
            }
        }
    })
    .into()
}
