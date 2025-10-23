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

        if let Type::Infer(_) = ty {
            *ty = Type::Path(syn::TypePath {
                qself: None,
                path: self.0.clone().into(),
            });
        };
    }
}
