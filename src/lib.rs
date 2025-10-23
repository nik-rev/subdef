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
//! The syntax is a little strange, but that's a small price to pay for the convenience
//! of automatic formatting by `rustfmt`!
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
//! - `#[subdef(apply_recursively(label1, label2))]` to recursively apply the attribute, overriding any previous `#[subdef(skip_recursively)]`
//!
//! The label for the `#[derive]` attribute is `derive`.
use std::collections::{HashMap, HashSet};

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Attribute, Error, Expr, Field, Ident, Item, ItemStruct, Token, Type, TypeArray, parenthesized,
    parse::{Parse, ParseBuffer, ParseStream},
    parse_quote,
    punctuated::Punctuated,
    spanned::Spanned,
    visit_mut::VisitMut,
};

#[proc_macro_attribute]
pub fn subdef(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut adt = syn::parse_macro_input!(input as Item);

    // Errors, to report all at once for maximum error recovery
    // from rust-analyzer
    let mut errors = Vec::new();
    // The top-level list of ADTs that we will output
    let mut expanded_adts = Vec::new();
    // These attributes can never be disabled. They don't have a label.
    let mut always_applicable_attrs = Vec::new();
    // Attributes with a label.
    let mut labelled_attrs = HashMap::new();
    // Attributes that apply to the current
    let mut applicable_labels = HashSet::new();

    expand_adt(
        &mut adt,
        &mut expanded_adts,
        &mut errors,
        &mut always_applicable_attrs,
        &mut labelled_attrs,
        &mut applicable_labels,
    );

    // Report all errors at once, but still give something for rust-analyzer to handle
    let errors = errors
        .into_iter()
        .reduce(|mut errors, error| {
            errors.combine(error);
            errors
        })
        .map(Error::into_compile_error);

    quote! {
        #errors
        #adt
        #(#expanded_adts)*
    }
    .into()
}

/// Expands the ADT, generating the true value of all the fields.
/// Any inline-adts are lifted to the outer scope. This process is recursive,
/// as inline adts may themselves contain fields that contain inline adts.
fn expand_adt(
    adt: &mut Item,
    expanded_adts: &mut Vec<Item>,
    errors: &mut Vec<Error>,
    always_applicable_attrs: &mut Vec<proc_macro2::TokenStream>,
    labelled_attrs: &mut HashMap<String, proc_macro2::TokenStream>,
    applicable_labels: &mut HashSet<String>,
) {
    let (attrs, fields): (_, Box<dyn Iterator<Item = &mut Field>>) = match adt {
        Item::Struct(adt) => (&mut adt.attrs, Box::new(adt.fields.iter_mut())),
        Item::Enum(adt) => (
            &mut adt.attrs,
            Box::new(
                adt.variants
                    .iter_mut()
                    .flat_map(|variant| variant.fields.iter_mut()),
            ),
        ),
        Item::Union(adt) => (&mut adt.attrs, Box::new(adt.fields.named.iter_mut())),
        item => {
            errors.push(Error::new(
                item.span(),
                "expected `struct`, `enum`, or `union`",
            ));
            return;
        }
    };

    // Expand `#[subdef]` attributes, applying all attributes
    // that should be applied to this ADT, and recording any new
    // attributes for future nested ADTs
    expand_subdef_attrs(
        attrs,
        always_applicable_attrs,
        labelled_attrs,
        applicable_labels,
        errors,
    );

    // Expand each type to its true field, and generate
    // the actual ADT, as well as expanding any inline ADTs
    // that this ADT contains in its fields. Recursive.
    for field in fields {
        expand_field(
            field,
            expanded_adts,
            errors,
            always_applicable_attrs,
            labelled_attrs,
            applicable_labels,
        );
    }
}

/// Expands a field into the true type. If this field contains a nested adt, this adt
/// is added to the `output`
fn expand_field(
    field: &mut Field,
    expanded_adts: &mut Vec<Item>,
    errors: &mut Vec<Error>,
    always_applicable_attrs: &mut Vec<proc_macro2::TokenStream>,
    labelled_attrs: &mut HashMap<String, proc_macro2::TokenStream>,
    applicable_labels: &mut HashSet<String>,
) {
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

                // expand fields of this adt, if it itself contains nested adts
                expand_adt(
                    item,
                    expanded_adts,
                    errors,
                    always_applicable_attrs,
                    labelled_attrs,
                    applicable_labels,
                );

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

/// Expand all `#[subdef]` attributes, which contain a list of `AttrSubdefSingle`
fn expand_subdef_attrs(
    adt_attrs: &mut Vec<Attribute>,
    always_applicable_attrs: &mut Vec<proc_macro2::TokenStream>,
    labelled_attrs: &mut HashMap<String, proc_macro2::TokenStream>,
    applicable_labels: &mut HashSet<String>,
    errors: &mut Vec<Error>,
) {
    // Skip these labels for this ADT, but not nested ADTs
    let mut skip_just_this_time = HashSet::new();
    // Apply these labels for this ADT, but not nested ADTs
    let mut apply_just_this_time = HashSet::new();

    // Remove all `#[subdef(..)]` attributes that there are, and iterate
    // over the removed elements
    for attr in adt_attrs.extract_if(.., |attr| attr.path().is_ident("subdef")) {
        let subdefs = match attr
            .parse_args_with(Punctuated::<AttrSubdefSingle, Token![,]>::parse_terminated)
        {
            Ok(subdefs) => subdefs,
            Err(err) => {
                errors.push(err);
                continue;
            }
        };

        for subdef in subdefs {
            match subdef {
                AttrSubdefSingle::Attr { attr } => {
                    always_applicable_attrs.push(attr);
                }
                AttrSubdefSingle::AttrLabel { label, attr } => {
                    labelled_attrs.insert(label.to_string(), attr);
                }
                AttrSubdefSingle::Skip(labels) => {
                    for label in labels {
                        skip_just_this_time.insert(label.to_string());
                    }
                }
                AttrSubdefSingle::SkipRecursively(labels) => {
                    for label in labels {
                        applicable_labels.remove(&label.to_string());
                    }
                }
                AttrSubdefSingle::Apply(labels) => {
                    for label in labels {
                        apply_just_this_time.insert(label.to_string());
                    }
                }
                AttrSubdefSingle::ApplyRecursively(labels) => {
                    for label in labels {
                        applicable_labels.insert(label.to_string());
                    }
                }
            }
        }
    }

    // Labels that apply to this ADT
    let applicable_labels = applicable_labels
        .intersection(&apply_just_this_time)
        .cloned()
        .collect::<HashSet<_>>();
    let applicable_labels = applicable_labels
        .difference(&skip_just_this_time)
        .collect::<HashSet<_>>();

    // All attributes we'll apply to the generated ADT
    let attrs = always_applicable_attrs
        .iter()
        .chain(
            labelled_attrs
                .iter()
                .filter_map(|(label, attr)| applicable_labels.contains(label).then_some(attr)),
        )
        .cloned();

    // Add attributes to the generated ADT
    adt_attrs.extend(attrs.map(|attr| parse_quote!(#[#attr])));
}

/// A single attribute
///
/// ```rust
/// #[subdef(attr(whatever), label = attr(whatever), skip(label))]
/// //       ^^^^^^^^^^^^^^
/// //                       ^^^^^^^^^^^^^^^^^^^^^^
/// //                                               ^^^^^^^^^^^^
/// ```
///
/// Each of the above `^^^` is this type
enum AttrSubdefSingle {
    /// A stream corresponding to any attribute at all
    ///
    /// `attr(whatever)`
    Attr { attr: proc_macro2::TokenStream },
    /// A label associated with any attribute
    ///
    /// `label = attr(whatever)`
    AttrLabel {
        label: Ident,
        attr: proc_macro2::TokenStream,
    },
    /// `skip(label1, label2)`
    Skip(Punctuated<Ident, Token![,]>),
    /// `skip_recursively(label1, label2)`
    SkipRecursively(Punctuated<Ident, Token![,]>),
    /// `apply(label1, label2)`
    Apply(Punctuated<Ident, Token![,]>),
    /// `apply_recursively(label1, label2)`
    ApplyRecursively(Punctuated<Ident, Token![,]>),
}

impl Parse for AttrSubdefSingle {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let single = if input.parse::<Option<kw::skip>>().is_ok() {
            Self::Skip
        } else if input.parse::<Option<kw::skip_recursively>>().is_ok() {
            Self::SkipRecursively
        } else if input.parse::<Option<kw::apply>>().is_ok() {
            Self::Apply
        } else if input.parse::<Option<kw::apply_recursively>>().is_ok() {
            Self::ApplyRecursively
        } else if input.peek2(Token![=]) {
            let label = input.parse::<Ident>()?;
            input.parse::<Token![=]>()?;
            let attr = parse_until_comma(input)?;
            input.parse::<Option<Token![,]>>()?;
            return Ok(Self::AttrLabel { label, attr });
        } else {
            let attr = parse_until_comma(input)?;
            input.parse::<Option<Token![,]>>()?;
            return Ok(Self::Attr { attr });
        };
        let labels;
        parenthesized!(labels in input);
        Ok(single(labels.parse_terminated(Ident::parse, Token![,])?))
    }
}

/// Parse everything into a `TokenStream` until we hit a comma. The comma is not included.
fn parse_until_comma(input: &ParseBuffer) -> syn::Result<proc_macro2::TokenStream> {
    let mut attr = proc_macro2::TokenStream::new();
    while !input.peek(Token![,]) && !input.is_empty() {
        let tt: proc_macro2::TokenTree = input.parse()?;
        attr.extend([tt]);
    }
    Ok(attr)
}

mod kw {
    syn::custom_keyword!(skip);
    syn::custom_keyword!(skip_recursively);
    syn::custom_keyword!(apply);
    syn::custom_keyword!(apply_recursively);
}
