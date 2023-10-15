extern crate proc_macro;
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{parse_macro_input, BinOp, Expr, ExprBinary, Fields, ItemEnum, Type};

#[derive(Clone)]
enum Variant {
    Leaf(Ident, Ident, Expr),
    Branch(Ident, Ident, Type),
}

#[proc_macro_attribute]
pub fn operation(args: TokenStream, input: TokenStream) -> TokenStream {
    let target = parse_macro_input!(input as ItemEnum);

    if !args.is_empty() {
        panic!("Expected no args")
    }

    let metas = target.attrs;
    let name = target.ident;

    let variants: Vec<_> = target
        .variants
        .into_iter()
        .map(|variant| {
            if let Some((_, discrim)) = variant.discriminant {
                Variant::Leaf(name.clone(), variant.ident, discrim)
            } else if let Fields::Unnamed(fields) = variant.fields {
                let unnamed = fields.unnamed;
                if unnamed.len() == 1 {
                    let first = unnamed.into_iter().next().unwrap();
                    Variant::Branch(name.clone(), variant.ident, first.ty)
                } else {
                    panic!("must have one field only")
                }
            } else {
                panic!("not valid")
            }
        })
        .collect();

    let definitions: Vec<_> = variants
        .clone()
        .into_iter()
        .map(|variant| match variant {
            Variant::Leaf(_, name, _) => quote! { #name },
            Variant::Branch(_, name, field) => quote! { #name(#field) },
        })
        .collect();

    let (leaves, branches): (Vec<_>, Vec<_>) = variants
        .into_iter()
        .map(|variant| match variant {
            Variant::Leaf(a, b, c) => (Some((a, b, c)), None),
            Variant::Branch(a, b, c) => (None, Some((a, b, c))),
        })
        .unzip();

    let patterns: Vec<_> = leaves
        .clone()
        .into_iter()
        .flatten()
        .map(|(parent, name, pat)| {
            quote! {
                #pat => Ok(#parent::#name)
            }
        })
        .collect();

    let antipatterns: Vec<_> = leaves
        .into_iter()
        .flatten()
        .map(|(parent, name, pat)| {
            // the first choice is the canonical form
            let choice = if let Expr::Binary(ExprBinary {
                left,
                op: BinOp::BitOr(_),
                ..
            }) = &pat
            {
                *left.clone()
            } else {
                pat
            };
            quote! {
                #parent::#name => (#choice).to_string()
            }
        })
        .collect();

    // assumes string argument is always `s`
    let nodes: Vec<_> = branches
        .clone()
        .into_iter()
        .flatten()
        .map(|(parent, name, ty)| {
            // Early return avoids having to match too many strings
            quote! {
                if let Ok(op) = <#ty as FromStr>::from_str(s) {
                    return Ok(#parent::#name(op));
                }
            }
        })
        .collect();

    let antinodes: Vec<_> = branches
        .into_iter()
        .flatten()
        .map(|(parent, name, ty)| {
            quote! {
                #parent::#name(op) => <#ty as ToString>::to_string(op)
            }
        })
        .collect();

    quote! {
        #(#metas)*
        enum #name {
            #(#definitions,)*
        }

        impl FromStr for #name {
            type Err = ();

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    #(#patterns,)*
                    s => {
                        #(#nodes)*
                        _ = s;
                        Err(())
                    }
                }
            }
        }

        impl ToString for #name {
            fn to_string(&self) -> String {
                match self {
                    #(#antipatterns,)*
                    #(#antinodes,)*
                }
            }
        }
    }
    .into()
}
