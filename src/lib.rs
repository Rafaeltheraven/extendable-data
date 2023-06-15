pub use extendable_data_helpers::extendable_data;
use syn::token::{Comma, Lt, Gt, Where};
use syn::{self, DeriveInput, Generics, GenericParam, WhereClause, WherePredicate};
use syn::punctuated::{Punctuated};
use syn::Fields::*;
use proc_macro2::{TokenStream, Ident, Span};
use quote::quote;

fn combine_wheres(where_a: WhereClause, where_b: WhereClause) -> WhereClause {
    let pred_a = where_a.predicates.into_iter();
    let pred_b = where_b.predicates.into_iter();
    let combined: Punctuated<WherePredicate, Comma> = Punctuated::from_iter(pred_a.chain(pred_b));
    WhereClause { 
        where_token: Where::default(), 
        predicates: combined,
    }
}

fn combine_generics(input_a: Generics, input_b: Generics) -> Generics {
    let params_a = input_a.params.into_iter();
    let params_b = input_b.params.into_iter();
    let combined = params_a.chain(params_b);
    let params_c: Punctuated<GenericParam, Comma> = Punctuated::from_iter(combined);
    let where_c: Option<WhereClause> = match (input_a.where_clause, input_b.where_clause) {
        (None, None) => None,
        (None, Some(where_b)) => Some(where_b),
        (Some(where_a), None) => Some(where_a),
        (Some(where_a), Some(where_b)) => Some(combine_wheres(where_a, where_b)),
    };
    Generics { 
        lt_token: Some(Lt::default()), 
        params: params_c, 
        gt_token: Some(Gt::default()), 
        where_clause: where_c
    }
}

fn combine_enums<'input>(enum_a: &'input syn::DataEnum, enum_b: &'input syn::DataEnum) -> TokenStream {
    let data = enum_a.variants.iter().chain(enum_b.variants.iter());
    TokenStream::from(quote!({
        #(#data),*
    }))
}

fn combine_structs<'input>(struct_a: &'input syn::DataStruct, struct_b: &'input syn::DataStruct) -> TokenStream {
    match (&struct_a.fields, &struct_b.fields) {
        (Named(fields_a), Named(fields_b)) => {
            let combined = fields_a.named.pairs().chain(fields_b.named.pairs());
            quote!({
                #(#combined),*
            })
        },
        (Unnamed(fields_a), Unnamed(fields_b)) => {
            let combined = fields_a.unnamed.iter().chain(fields_b.unnamed.iter());
            quote! {
                (#(#combined),*);
            }
        },
        (Unit, Named(fields)) | (Named(fields), Unit) => {
            quote!(#fields)
        },
        (Unit, Unnamed(fields)) | (Unnamed(fields), Unit) => {
            quote!(#fields;)
        }
        (Unit, Unit) => quote!(;),
        _ => panic!("Can not combine provided structs as it makes no logical sense.")
    }
}

fn combine_unions<'input>(union_a: &'input syn::DataUnion, union_b: &'input syn::DataUnion) -> TokenStream {
    let data = union_a.fields.named.pairs().chain(union_b.fields.named.pairs());
    TokenStream::from(quote!({
        #(#data),*
    }))
}

fn construct_stream<'input>(
        data: TokenStream, 
        data_token: Ident, 
        visibility: &syn::Visibility, 
        gens: Generics, 
        name: syn::Ident, 
        attrs: Vec<syn::Attribute>
    ) -> TokenStream {
    TokenStream::from(quote! {
        #(#attrs)*
        #visibility #data_token #name #gens #data
    })
} 

/// Combines two enums into a single enum using TokenStreams.
///
/// Used internally by extendable-enums to construct a tokenstream which has the contents
/// of two enums glued together. The name of the enum in input_b will be the name of the "new" enum.
///
/// Technically this could probably be extended to structs as well, but I haven't looked into it.
pub fn combine_data(input_a: TokenStream, input_b: TokenStream) -> TokenStream {
    let ast_a = syn::parse2::<DeriveInput>(input_a).unwrap();
    let ast_b = syn::parse2::<DeriveInput>(input_b).unwrap();
    let vis_b = &ast_b.vis;
    let generics = combine_generics(ast_a.generics, ast_b.generics);
    let attrs = ast_a.attrs.into_iter().chain(ast_b.attrs.into_iter()).collect();
    let (data, data_token) = match (&ast_a.data, &ast_b.data) {
        (syn::Data::Enum(enum_a), syn::Data::Enum(enum_b)) => (combine_enums(&enum_a, &enum_b), "enum"),
        (syn::Data::Struct(struct_a), syn::Data::Struct(struct_b)) => (combine_structs(&struct_a, &struct_b), "struct"),
        (syn::Data::Union(union_a), syn::Data::Union(union_b)) => (combine_unions(&union_a, &union_b), "union"),
        _ => panic!("Can only combine 2 of the same type of data structure!")
    };
    construct_stream(data, Ident::new(data_token, Span::call_site()), vis_b, generics, ast_b.ident, attrs)
}

#[cfg(test)]
mod tests {

    use super::combine_data;
    use quote::quote;
    use syn::{DeriveInput};
    use proc_macro2::TokenStream;

    fn assert_streams(result: TokenStream, expected: TokenStream) {
        assert_eq!(syn::parse2::<DeriveInput>(result).unwrap(), syn::parse2::<DeriveInput>(expected).unwrap());
    }

    #[test]
    fn test_combine_enums() {
        let enum_a = quote! {
            enum A {
                One(Thing),
                Two {
                    SubOne: i32,
                    SubTwo: Another
                },
                Three 
            }
        };
        let enum_b = quote! {
            enum B {
                Four(Thing, Thing, Thing),
                Five,
                Six
            }
        };
        let expected = quote! {
            enum B {
                One(Thing),
                Two {
                    SubOne: i32,
                    SubTwo: Another
                },
                Three,
                Four(Thing, Thing, Thing),
                Five,
                Six
            }
        };
        let result = combine_data(enum_a, enum_b);
        assert_streams(result, expected);
    }

    #[test]
    fn test_combine_named_structs() {
        let struct_a = quote! {
            struct A {
                one: i32
            }
        };
        let struct_b = quote! {
            struct B {
                two: SomeType
            }
        };
        let expected = quote! {
            struct B {
                one: i32,
                two: SomeType
            }
        };
        let result = combine_data(struct_a, struct_b);
        assert_streams(result, expected);
    }

    #[test]
    fn test_combine_unnamed_structs() {
        let struct_a = quote! {
            struct A(i32, SomeType);
        };
        let struct_b = quote! {
            struct B(i64, Another);
        };
        let expected = quote! {
            struct B(i32, SomeType, i64, Another);
        };
        let result = combine_data(struct_a, struct_b);
        assert_streams(result, expected);
    }

    #[test]
    fn test_combine_unit_structs() {
        let struct_a = quote! {
            struct A;
        };
        let struct_b = quote! {
            struct B;
        };
        let struct_c = quote! {
            struct C {
                one: i32
            }
        };
        let struct_d = quote! {
            struct D(i32, i32);
        };
        let expected_unit = quote! {
            struct B;
        };
        let expected_named_one = quote! {
            struct C {
                one: i32
            }
        };
        let expected_named_two = quote! {
            struct A {
                one: i32
            }
        };
        let expected_unnamed_one = quote! {
            struct D(i32, i32);
        };
        let expected_unnamed_two = quote! {
            struct A(i32, i32);
        };

        // Don't care for testing, just clone
        let result_unit = combine_data(struct_a.clone(), struct_b);
        let result_named_one = combine_data(struct_a.clone(), struct_c.clone());
        let result_named_two = combine_data(struct_c, struct_a.clone());
        let result_unnamed_one = combine_data(struct_a.clone(), struct_d.clone());
        let result_unnamed_two = combine_data(struct_d, struct_a);

        assert_streams(result_unit, expected_unit);
        assert_streams(result_named_one, expected_named_one);
        assert_streams(result_named_two, expected_named_two);
        assert_streams(result_unnamed_one, expected_unnamed_one);
        assert_streams(result_unnamed_two, expected_unnamed_two);
    }

    #[test]
    #[should_panic]
    fn test_invalid_combine() {
        let input_a = quote! {
            struct A;
        };
        let input_b = quote! {
            enum B {
                Thing
            }
        };
        combine_data(input_a, input_b);

    }

    #[test]
    #[should_panic]
    fn test_invalid_combine_structs() {
        let input_a = quote! {
            struct A(i32, i32);
        };
        let input_b = quote! {
            struct B {
                one: i32
            }
        };
        combine_data(input_a, input_b);
    }

    #[test]
    fn test_combine_visibility() {
        let input_a = quote! {
            enum A {
                One
            }
        };
        let input_b = quote! {
            pub enum B {
                Two
            }
        };
        let expected = quote! {
            pub enum B {
                One,
                Two
            }
        };
        let result = combine_data(input_a, input_b);
        assert_streams(result, expected);
    }

    #[test]
    fn test_combine_attributes() {
        let input_a = quote! {
            #[some_attr]
            enum A {
                One
            }
        };
        let input_b = quote! {
            #[another(attr)]
            enum B {
                #[on_attr]
                Two
            }
        };
        let expected = quote! {
            #[some_attr]
            #[another(attr)]
            enum B {
                One,

                #[on_attr]
                Two
            }
        };
        let result = combine_data(input_a, input_b);
        assert_streams(result, expected);
        
        let input_a = quote! {
            #[some_attr]
            struct A;
        };
        let input_b = quote! {
            #[another_attr]
            struct B {
                one: i32
            }
        };
        let expected = quote! {
            #[some_attr]
            #[another_attr]
            struct B {
                one: i32
            }
        };
        let result = combine_data(input_a, input_b);
        assert_streams(result, expected);

    }

    #[test]
    fn test_combine_generics() {
        let input_a = quote! {
            enum A<'life, T> {
                One(Thing<'life, T>)
            }
        };
        let input_b = quote! {
            enum B<'efil, U> {
                Two(Thing<'efil, U>)
            }
        };
        let expected = quote! {
            enum B<'life, 'efil, T, U> {
                One(Thing<'life, T>),
                Two(Thing<'efil, U>)
            }
        };
        let result = combine_data(input_a, input_b);
        assert_streams(result, expected);
    }
}