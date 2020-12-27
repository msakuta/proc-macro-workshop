use core::panic;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::parse::Parse;
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, AngleBracketedGenericArguments, Data,
    DeriveInput, Field, Fields, FieldsNamed, GenericArgument, Ident, Path, PathArguments,
    PathSegment, Token, Type, TypePath,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let builder_name = Ident::new(&(input.ident.to_string() + "Builder"), Span::call_site());
    let name = input.ident;
    // let name = Ident::new(&(input.ident.to_string() + "Ident"), Span::call_site());

    let data = if let Data::Struct(data) = input.data {
        data
    } else {
        panic!();
    };

    let named_fields = if let Fields::Named(named_fields) = data.fields {
        named_fields
    } else {
        panic!();
    };

    // let named_fields = FieldsNamed{ brace_token: syn::token::Brace::default(), named: Punctuated::<Field, Comma>::new()};

    let field_names = named_fields
        .named
        .iter()
        .filter_map(|field| field.ident.clone())
        .collect::<Vec<_>>();

    fn is_option(ty: &Type) -> bool {
        if let Type::Path(tp) = ty {
            if let Some(first) = tp.path.segments.first() {
                if first.ident == "Option" {
                    return true;
                }
            }
        }
        false
    }

    fn unwrap_option(ty: &Type) -> Option<Type> {
        if let Type::Path(tp) = ty {
            if let Some(first) = tp.path.segments.first() {
                if first.ident == "Option" {
                    if let PathArguments::AngleBracketed(args) = &first.arguments {
                        if let Some(&GenericArgument::Type(ty)) = args.args.first().as_ref() {
                            return Some(ty.clone());
                        }
                    }
                }
            }
        }
        None
    }

    let mut optionals = vec![];
    let mut nonoptionals = vec![];

    let field_defs = named_fields
        .named
        .iter()
        .map(|field| Field {
            ty: if is_option(&field.ty) {
                optionals.push(field.clone());
                field.ty.clone()
            } else {
                nonoptionals.push(field.clone());
                Type::Path(TypePath {
                    qself: None,
                    path: Path {
                        leading_colon: None,
                        segments: {
                            let mut punct = Punctuated::new();
                            punct.push(PathSegment {
                                ident: Ident::new("Option", Span::call_site()),
                                arguments: PathArguments::AngleBracketed(
                                    AngleBracketedGenericArguments {
                                        colon2_token: None,
                                        lt_token: Token![<](Span::call_site()),
                                        args: {
                                            let mut punct2 = Punctuated::new();
                                            punct2.push(GenericArgument::Type(field.ty.clone()));
                                            punct2
                                        },
                                        gt_token: Token![>](Span::call_site()),
                                    },
                                ),
                            });
                            punct
                        },
                    },
                })
            },
            ..field.clone()
        })
        .collect::<Punctuated<Field, Token![,]>>();
    // let field_defs = quote!{ #( #field_names: Option<#types>, )* };

    let nonoptional_names = nonoptionals
        .iter()
        .map(|field| field.ident.clone())
        .collect::<Vec<_>>();

    let nonoptional_types = nonoptionals
        .iter()
        .map(|field| field.ty.clone())
        .collect::<Vec<_>>();

    let optional_names = optionals
        .iter()
        .map(|field| field.ident.clone())
        .collect::<Vec<_>>();

    let optional_types = optionals
        .iter()
        .map(|field| unwrap_option(&field.ty).unwrap())
        .collect::<Vec<_>>();
    eprintln!("{:?}", field_names);

    // for name in named {
    //     name.
    // }

    let tokens = quote! {
        pub struct #builder_name {
            #field_defs
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #( #field_names: None, )*
                }
            }
        }

        trait Error: std::fmt::Debug {

        }

        #[derive(Debug)]
        struct FieldError(String);

        impl FieldError{
            fn new() -> Self{
                Self("".to_string())
            }
        }

        impl Error for FieldError{

        }

        impl #builder_name {
            #(fn #nonoptional_names(&mut self, v: #nonoptional_types) -> &mut Self {
                self.#nonoptional_names = Some(v);
                self
            })*

            #(fn #optional_names(&mut self, v: #optional_types) -> &mut Self {
                self.#optional_names = Some(v);
                self
            })*

            fn build(&mut self) -> Result<#name, Box<dyn Error>> {
                Ok(#name {
                    #(#nonoptional_names: self.#nonoptional_names.take().ok_or_else(|| Box::new(FieldError::new()) as Box<dyn Error>)?, )*
                    #(#optional_names: self.#optional_names.take(), )*
                })
            }
        }
    };

    tokens.into()
}
