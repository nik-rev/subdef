//! [![crates.io](https://img.shields.io/crates/v/subdef?style=flat-square&logo=rust)](https://crates.io/crates/subdef)
//! [![docs.rs](https://img.shields.io/badge/docs.rs-subdef-blue?style=flat-square&logo=docs.rs)](https://docs.rs/subdef)
//! ![license](https://img.shields.io/badge/license-Apache--2.0_OR_MIT-blue?style=flat-square)
//! ![msrv](https://img.shields.io/badge/msrv-1.65-blue?style=flat-square&logo=rust)
//! [![github](https://img.shields.io/github/stars/nik-rev/subdef)](https://github.com/nik-rev/subdef)
//!
//! This crate provides a procedural macro `#[subdef]` - it simplifies the creation of nested
//! structures, reduces boilerplate and helps keep logic in a single place.
//!
//! ```toml
//! [dependencies]
//! subdef = "0.1"
//! ```
//!
//! This crate is a successor to [`nestify`](https://crates.io/crates/nestify).
//!
//! The main distinguishing feature is that items marked with `#[subdef]` can be
//! entirely formatted by `rustfmt` - as the syntax parses as 100% valid Rust.
//!
//! # Usage
//!
//! Apply `#[subdef]` to your type to be able to define inline types in individual fields
//!
//! ```rust
//! #[subdef]
//! struct UserProfile {
//!     name: String,
//!     address: [_; {
//!         struct Address {
//!             street: String,
//!             city: String
//!         }
//!     }],
//!     friends: [Vec<_>; {
//!         struct Friend {
//!             name: String
//!         }
//!     }],
//!     status: [_; {
//!         enum Status {
//!             Online,
//!             Offline,
//!             Idle
//!         }
//!     }]
//! }
//! ```
//!
//! Expansion:
//!
//! ```rust
//! struct UserProfile {
//!     name: String,
//!     address: Address,
//!     friends: Vec<Friend>,
//!     status: Status,
//! }
//! struct Address {
//!     street: String,
//!     city: String,
//! }
//! struct Friend {
//!     name: String,
//! }
//! enum Status {
//!     Online,
//!     Offline,
//!     Idle,
//! }
//! ```
//!
//! ## Details
//!
//! Fields on types marked with `#[subdef]` can have the type `[Type; { Item }]` where `Type` is the actual
//! type of the field, and `Item` is the `struct` or `enum`.
//!
//! The `Type` can contain `_`, which infers to the name of the `Item`. In the above example:
//!
//! - The `address` field contains `_`, which infers to be `Address`.
//! - The `friends` field contains `_`, which infers to be `Friend`,
//!   so `Vec<_>` is inferred to `Vec<Friend>`
//!
//! You can apply `#[subdef]` to enums:
//!
//! ```rust
//!  #[subdef]
//!  pub enum One {
//!      Two([_, { pub struct Two; }])
//!  }
//! ```
//!
//! Inline types can contain fields that have inline types themselves:
//!
//! ```rust
//! #[subdef]
//! struct One {
//!     two: [_; {
//!         struct Two {
//!             three: [_; {
//!                 struct Three;
//!             }]
//!         }
//!     }]
//! }
//! ```
//!
//! # Propagate attributes
//!
//! The `derive` attribute is propagated to all nested items. For example, this:
//!
//! ```rust
//! #[subdef]
//! #[derive(Serialize, Deserialize)]
//! struct SystemReport {
//!     report_id: Uuid,
//!     kind: [_; {
//!         pub enum ReportKind {
//!             Initial,
//!             Heartbeat,
//!             Shutdown,
//!         }
//!     }],
//!     application_config: [_; {
//!         struct ApplicationConfig {
//!             version: String,
//!             container_runtime: String,
//!
//!             flags: [_; {
//!                 struct Flags {
//!                     is_admin: bool,
//!                     is_preview_mode: bool,
//!                     telemetry_enabled: bool,
//!                 }
//!             }],
//!             components: [Vec<_>; {
//!                 struct Component {
//!                     name: String,
//!                     version: String,
//!                     maintainer: Option<String>,
//!                     target_platform: String,
//!                 }
//!             }],
//!         }
//!     }],
//! }
//! ```
//!
//! Expands to this, with fields omitted:
//!
//! ```rust
//! #[derive(Serialize, Deserialize)]
//! struct SystemReport { ... }
//!
//! #[derive(Serialize, Deserialize)]
//! pub enum ReportKind { ... }
//!
//! #[derive(Serialize, Deserialize)]
//! struct Flags { ... }
//!
//! #[derive(Serialize, Deserialize)]
//! struct Component { ... }
//!
//! #[derive(Serialize, Deserialize)]
//! struct ApplicationConfig { ... }
//! ```
//!
//! ## Propagate other attributes
//!
//! The `derive` is propagated by default for convenience, but you can propagate any attribute
//! by passing it as an argument to `subdef`:
//!
//! ```rust
//! #[subdef(cfg(not(windows)), object)]
//! #[derive(Serialize, Deserialize)]
//! struct SystemReport { ... }
//! ```
//!
//! The above expands to this:
//!
//! ```rust
//! #[subdef(cfg(not(windows)), object)]
//! #[derive(Serialize, Deserialize)]
//! struct SystemReport { ... }
//!
//! #[subdef(cfg(not(windows)), object)]
//! #[derive(Serialize, Deserialize)]
//! pub enum ReportKind { ... }
//!
//! #[subdef(cfg(not(windows)), object)]
//! #[derive(Serialize, Deserialize)]
//! struct Flags { ... }
//!
//! #[subdef(cfg(not(windows)), object)]
//! #[derive(Serialize, Deserialize)]
//! struct Component { ... }
//!
//! #[subdef(cfg(not(windows)), object)]
//! #[derive(Serialize, Deserialize)]
//! struct ApplicationConfig { ... }
//! ```
//!
//! ## Disable propagation (advanced)
//!
//! You can attach labels to each attribute:
//!
//! ```rust
//! #[subdef(
//!     label1 = cfg(not(windows)),
//!     label2 = object
//! )]
//! #[derive(Serialize, Deserialize)]
//! struct SystemReport { ... }
//! ```
//!
//! You can apply these attributes to the top-level, or any of the nested types:
//!
//! - `#[subdef(skip(label1, label2))]` to skip applying the attribute to the type
//! - `#[subdef(skip_recursively(label1, label2))]` to recursively skip applying the attribute to the type
//! - `#[subdef(apply(label1, label2))]` to apply the attribute, overriding any previous `#[subdef(skip_recursively)]`
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Error, Expr, Field, Ident, Item, ItemStruct, Type, TypeArray, spanned::Spanned,
    visit_mut::VisitMut,
};

#[proc_macro_attribute]
pub fn subdef(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut input = syn::parse_macro_input!(input as Item);
    let mut errors = Vec::new();
    let mut expanded_adts = Vec::new();

    expand_adt(&mut input, &mut expanded_adts, &mut errors);

    let errors = errors
        .into_iter()
        .reduce(|mut errors, error| {
            errors.combine(error);
            errors
        })
        .map(Error::into_compile_error);

    quote! {
        #errors
        #input
        #(#expanded_adts)*
    }
    .into()
}

/// Expands the ADT, generating the true value of all the fields.
/// Any inline-adts are lifted to the outer scope. This process is recursive,
/// as inline adts may themselves contain fields that contain inline adts.
fn expand_adt(adt: &mut Item, expanded_adts: &mut Vec<Item>, errors: &mut Vec<Error>) {
    match adt {
        Item::Struct(data_struct) => {
            for field in &mut data_struct.fields {
                expand_field(field, expanded_adts, errors);
            }
        }
        Item::Enum(data_enum) => {
            for variant in &mut data_enum.variants {
                for field in &mut variant.fields {
                    expand_field(field, expanded_adts, errors);
                }
            }
        }
        Item::Union(data_union) => {
            for field in &mut data_union.fields.named {
                expand_field(field, expanded_adts, errors);
            }
        }
        item => errors.push(Error::new(
            item.span(),
            "expected `struct`, `enum` or `union`",
        )),
    }
}

/// Expands a field into the true type. If this field contains a nested adt, this adt
/// is added to the `output`
fn expand_field(field: &mut Field, expanded_adts: &mut Vec<Item>, errors: &mut Vec<Error>) {
    match &mut field.ty {
        Type::Array(TypeArray {
            elem: field_ty,
            len: Expr::Block(block),
            ..
        }) if block.attrs.is_empty() && block.label.is_none() && block.block.stmts.len() == 1 => {
            if let syn::Stmt::Item(
                Item::Struct(ItemStruct { ident, .. })
                | Item::Enum(syn::ItemEnum { ident, .. })
                | Item::Union(syn::ItemUnion { ident, .. }),
            ) = block.block.stmts.first().expect(".len() == 1")
            {
                // `_` is replaced with `ident`
                ReplaceTyInferWithIdent(ident.clone()).visit_type_mut(field_ty);

                let Some(syn::Stmt::Item(item)) = block.block.stmts.iter_mut().next() else {
                    unreachable!("see match condition")
                };

                // expand fields of this item, if they themselves contain nested adts
                expand_adt(item, expanded_adts, errors);

                let item = std::mem::replace(item, syn::Item::Verbatim(TokenStream::new().into()));

                field.ty = *field_ty.clone();

                // the inline structs/enums in all fields is added to the flat list
                // that we output after the main item
                expanded_adts.push(item);
            }
        }
        // Any other type, could also be array but with another expression, for example:
        //
        // field: [usize, { const fn foo() -> i32 { 4 }; foo() + 2 }]
        //
        // We don't want to touch that.
        _ => (),
    };
}

/// Replace all `_` types with the `ident`
struct ReplaceTyInferWithIdent(Ident);

impl syn::visit_mut::VisitMut for ReplaceTyInferWithIdent {
    fn visit_type_mut(&mut self, ty: &mut crate::Type) {
        syn::visit_mut::visit_type_mut(self, ty);

        if let Type::Infer(infer) = ty {
            let mut ident = self.0.clone();
            ident.set_span(infer.span());
            *ty = Type::Path(syn::TypePath {
                qself: None,
                path: ident.into(),
            });
        };
    }
}
