
use std::collections::{HashMap, HashSet};
use std::{mem};
use std::ops::{IndexMut};
pub use extendable_data_helpers::extendable_data;
use syn::meta::ParseNestedMeta;
use syn::spanned::Spanned;
use syn::token::{Comma, Lt, Gt, Where, Brace, Paren};
use syn::{self, DeriveInput, Generics, GenericParam, WhereClause, WherePredicate, Variant, Attribute, Path, Fields, FieldsNamed, FieldsUnnamed};
use syn::parse::{Result};
use syn::punctuated::{Punctuated};
use syn::Fields::*;
use proc_macro2::{TokenStream, Ident, Span};
use quote::{quote, ToTokens};


fn overwrite_optionals<T>(option_a: Option<T>, option_b: Option<T>) -> Option<T> {
    match (&option_a, &option_b) {
        (Some(_), None) => option_a,
        _ => option_b
    }
}

fn path_into_string(path: Path) -> String {
    path.into_token_stream().to_string()
}

fn path_to_string(path: &Path) -> String {
    path.to_token_stream().to_string()
}

fn meta_to_string(meta: &syn::Meta) -> String {
    match meta {
        syn::Meta::Path(p) => path_to_string(p),
        syn::Meta::List(m) => path_to_string(&m.path),
        syn::Meta::NameValue(m) => path_to_string(&m.path)
    }
}

trait Len {
    fn len(&self) -> usize;
}

impl<T> Len for Vec<T> {
    fn len(&self) -> usize {
        self.len()
    }
}

impl<T, P> Len for Punctuated<T, P> {
    fn len(&self) -> usize {
        self.len()
    }
}

fn combine_iters<T, U, F1, F2>(iter_a: T, iter_b: T, to_str: F1, handle_conflict: F2, args: &Args) -> Result<T>
where
    T: IntoIterator<Item = U> + Default + Extend<U> + IndexMut<usize, Output = U> + Len,
    F1: Fn(&U) -> String,
    F2: Fn(&mut T, usize, U, &Args) -> Result<()>
{
    let mut seen: HashMap<String, usize> = HashMap::with_capacity(iter_a.len()); // Will always need exactly this much
    let mut combined: T = <T as std::default::Default>::default();
    let mut i = 0;
    for a in iter_a.into_iter() {
        let repr = to_str(&a);
        if !args.filter.contains(&repr) {
            seen.insert(repr, i);
            combined.extend([a]);
            i += 1;
        }
    }
    for b in iter_b.into_iter() {
        if let Some(i) = seen.remove(&to_str(&b)) {
            handle_conflict(&mut combined, i, b, args)?;
        } else {
            combined.extend([b]);
        }
    }
    Ok(combined)
}

fn handle_conflicts_basic<T: IntoIterator<Item = U> + Default + Extend<U> + IndexMut<usize, Output = U> + Len, U>(list: &mut T, index: usize, data: U, _args: &Args) -> Result<()> {
    list[index] = data;
    Ok(())
}

fn combine_attrs(attr_a: Vec<Attribute>, attr_b: Vec<Attribute>, args: &Args) -> Result<Vec<Attribute>> {
    combine_iters(attr_a, attr_b, |x| meta_to_string(&x.meta), handle_conflicts_basic, args)
}

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

fn combine_fields_named(fields_a: FieldsNamed, fields_b: FieldsNamed, args: &Args) -> Result<FieldsNamed> {
    let named = combine_iters(fields_a.named, fields_b.named, |x| x.ident.as_ref().unwrap().to_string(), handle_conflicts_basic, args)?;
    Ok(FieldsNamed {
        brace_token: Brace::default(),
        named
    })
}

fn combine_fields(fields_a: Fields, fields_b: Fields, args: &Args, merging: bool) -> Result<Fields> {
    let b_span = fields_b.span();
    match (fields_a, fields_b) {
        (_, f) if !merging => Ok(f),
        (Named(fields_a), Named(fields_b)) => {
            let resp = combine_fields_named(fields_a, fields_b, args)?;
            Ok(Named(resp))
        },
        (Unnamed(fields_a), Unnamed(fields_b)) => {
            let unnamed = combine_iters(fields_a.unnamed, fields_b.unnamed, |x| x.ty.to_token_stream().to_string(), handle_conflicts_basic, args)?;
            Ok(Unnamed(FieldsUnnamed {
                paren_token: Paren::default(),
                unnamed
            }))
        },
        (Unit, f) | (f, Unit) => Ok(f),
        _ => Err(syn::Error::new(b_span, "Can not combine provided structs. Either make sure they are the same type, or filter out the offending struct."))
    }
}

fn combine_enum_variants(variants_a: Punctuated<Variant, Comma>, variants_b: Punctuated<Variant, Comma>, args: Args) -> Result<Punctuated<Variant, Comma>> {
    fn handle_merge_conflict(combined: &mut Punctuated<Variant, Comma>, i: usize, b: Variant, args: &Args) -> Result<()> {
        let a = mem::replace(&mut combined[i], b);
        combined[i].attrs = combine_attrs(a.attrs, mem::take(&mut combined[i].attrs), args)?;
        combined[i].fields = combine_fields(a.fields, mem::replace(&mut combined[i].fields, Unit), args, args.merge)?;
        combined[i].discriminant = overwrite_optionals(a.discriminant, mem::take(&mut combined[i].discriminant));
        Ok(())
    }
    let handle_conflicts = if args.merge {
        handle_merge_conflict
    } else {
        handle_conflicts_basic
    };
    combine_iters(variants_a, variants_b, |x| x.ident.to_string(), handle_conflicts, &args)
}

fn combine_enums(enum_a: syn::DataEnum, enum_b: syn::DataEnum, args: Args) -> Result<(TokenStream, &'static str)> {
    let variants = combine_enum_variants(enum_a.variants, enum_b.variants, args)?;
    let tokens = quote!({
        #variants
    });
    Ok((tokens, "enum"))
}

fn combine_structs(struct_a: syn::DataStruct, struct_b: syn::DataStruct, args: Args) -> Result<(TokenStream, &'static str)> {
    let fields = combine_fields(struct_a.fields, struct_b.fields, &args, true)?;
    let tokens = match fields {
        Named(fields) => quote!(#fields),
        Unnamed(fields) => quote!(#fields;),
        Unit => quote!(;)
    };
    Ok((tokens, "struct"))
}

fn combine_unions(union_a: syn::DataUnion, union_b: syn::DataUnion, args: Args) -> Result<(TokenStream, &'static str)> {
    let fields = combine_fields_named(union_a.fields, union_b.fields, &args)?;
    let tokens = quote!({#fields});
    Ok((tokens, "union"))
}

fn construct_stream (
        data: TokenStream, 
        data_token: Ident, 
        visibility: syn::Visibility, 
        gens: Generics, 
        name: syn::Ident, 
        attrs: Vec<syn::Attribute>
    ) -> TokenStream {
    quote! {
        #(#attrs)*
        #visibility #data_token #name #gens #data
    }
}

#[derive(Default)]
struct Args {
    filter: HashSet<String>,
    merge: bool
}

impl Args {
    fn parse(&mut self, meta: ParseNestedMeta) -> Result<()> {
        if meta.path.is_ident("filter") {
            meta.parse_nested_meta(|meta| {
                let ident: String = path_into_string(meta.path);
                self.filter.insert(ident);
                Ok(())
            })
        } else if meta.path.is_ident("merge_on_conflict") {
            self.merge = true;
            Ok(())
        } else {
            Err(meta.error("Unsupported Argument"))
        }
    }
}

/// Combines two enums into a single enum using TokenStreams.
///
/// Used internally by extendable-enums to construct a tokenstream which has the contents
/// of two enums glued together. The name of the enum in input_b will be the name of the "new" enum.
///
/// Technically this could probably be extended to structs as well, but I haven't looked into it.
pub fn combine_data(input_a: TokenStream, input_b: TokenStream, args_input: Option<TokenStream>) -> TokenStream {
    let ast_a = match syn::parse2::<DeriveInput>(input_a) {
        Ok(a) => a,
        Err(e) => return e.to_compile_error()
    };
    let ast_b = match syn::parse2::<DeriveInput>(input_b) {
        Ok(b) => b,
        Err(e) => return e.to_compile_error()
    };
    let mut args = Args::default();
    if let Some(a) = args_input {
        let arg_parser = syn::meta::parser(|meta| args.parse(meta));
        if let Err(e) = syn::parse::Parser::parse2(arg_parser, a) {
            return e.to_compile_error();
        }
    }
    let b_span = ast_b.span();
    let generics = combine_generics(ast_a.generics, ast_b.generics);
    let attrs = match combine_attrs(ast_a.attrs, ast_b.attrs, &args) {
        Ok(attrs) => attrs,
        Err(e) => return e.to_compile_error()
    };
    let resp = match (ast_a.data, ast_b.data) {
        (syn::Data::Enum(enum_a), syn::Data::Enum(enum_b)) => combine_enums(enum_a, enum_b, args),
        (syn::Data::Struct(struct_a), syn::Data::Struct(struct_b)) => combine_structs(struct_a, struct_b, args),
        (syn::Data::Union(union_a), syn::Data::Union(union_b)) => combine_unions(union_a, union_b, args),
        _ => Err(syn::Error::new(b_span, "Can only combine 2 of the same type of data structure!",))
    };
    match resp {
        Ok((data, data_token)) => {
            let vis_b = ast_b.vis;
            construct_stream(data, Ident::new(data_token, Span::call_site()), vis_b, generics, ast_b.ident, attrs)
        },
        Err(e) => e.to_compile_error()
    }
}

#[cfg(test)]
mod tests {

    use super::combine_data;
    use quote::quote;
    use syn::{DeriveInput};
    use proc_macro2::TokenStream;
    use assert_tokenstreams_eq::assert_tokenstreams_eq;

    fn assert_streams(result: TokenStream, expected: TokenStream) {
        assert_eq!(syn::parse2::<DeriveInput>(result).unwrap(), syn::parse2::<DeriveInput>(expected).unwrap());
    }

    fn assert_compiler_error(result: TokenStream, msg: &str) {
        let expected = quote!(::core::compile_error! { #msg });
        assert_tokenstreams_eq!(&result, &expected);
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
        let result = combine_data(enum_a, enum_b, None);
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
        let result = combine_data(struct_a, struct_b, None);
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
        let result = combine_data(struct_a, struct_b, None);
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
        let result_unit = combine_data(struct_a.clone(), struct_b, None);
        let result_named_one = combine_data(struct_a.clone(), struct_c.clone(), None);
        let result_named_two = combine_data(struct_c, struct_a.clone(), None);
        let result_unnamed_one = combine_data(struct_a.clone(), struct_d.clone(), None);
        let result_unnamed_two = combine_data(struct_d, struct_a, None);

        assert_streams(result_unit, expected_unit);
        assert_streams(result_named_one, expected_named_one);
        assert_streams(result_named_two, expected_named_two);
        assert_streams(result_unnamed_one, expected_unnamed_one);
        assert_streams(result_unnamed_two, expected_unnamed_two);
    }

    #[test]
    fn test_invalid_combine() {
        let input_a = quote! {
            struct A;
        };
        let input_b = quote! {
            enum B {
                Thing
            }
        };
        let result = combine_data(input_a, input_b, None);
        assert_compiler_error(result, "Can only combine 2 of the same type of data structure!");
    }

    #[test]
    fn test_invalid_combine_structs() {
        let input_a = quote! {
            struct A(i32, i32);
        };
        let input_b = quote! {
            struct B {
                one: i32
            }
        };
        let result = combine_data(input_a, input_b, None);
        assert_compiler_error(result, "Can not combine provided structs. Either make sure they are the same type, or filter out the offending struct.");
    }

    #[test]
    fn test_invalid_args() {
        let input_a = quote! {
            struct A;
        };
        let input_b = quote! {
            struct B;
        };
        let result = combine_data(input_a, input_b, Some(quote!(fake arg)));
        assert_compiler_error(result, "Unsupported Argument");
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
        let result = combine_data(input_a, input_b, None);
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
        let result = combine_data(input_a, input_b, None);
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
        let result = combine_data(input_a, input_b, None);
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
        let result = combine_data(input_a, input_b, None);
        assert_streams(result, expected);
    }

    #[test]
    fn test_namespace_confict_overwrite() {
        let input_a = quote! {
            enum A {
                One,
                #[one_attr]
                Two,
                Three {
                    x: i32
                }
            }
        };
        let input_b = quote! {
            enum B {
                #[two_attr]
                Two(Thing),
                Three {
                    y: i32
                },
                Four
            }
        };
        let expected = quote! {
            enum B {
                One,
                #[two_attr]
                Two(Thing),
                Three {
                    y: i32
                },
                Four
            }
        };
        let result = combine_data(input_a, input_b, None);
        assert_streams(result, expected);
    }

    #[test]
    fn test_namespace_conflict_merge() {
        let input_a = quote! {
            enum A {
                One,
                #[one_attr]
                Two,
                Three {
                    x: i32
                }
            }
        };
        let input_b = quote! {
            enum B {
                #[two_attr]
                Two(Thing),
                Three {
                    y: i32
                },
                Four
            }
        };
        let expected = quote! {
            enum B {
                #[one_attr]
                #[two_attr]
                Two(Thing),
                Three {
                    x: i32,
                    y: i32
                },
                Four
            }
        };
        let args = quote!(merge_on_conflict, filter(One));
        let result = combine_data(input_a, input_b, Some(args));
        assert_streams(result, expected);
    }

    #[test]
    fn test_namespace_conflict_struct() {
        let input_a = quote! {
            struct A {
                x: i32,
                y: i32
            }
        };
        let input_b = quote! {
            struct B {
                x: i64
            }
        };
        let expected = quote! {
            struct B {
                x: i64,
                y: i32
            }
        };
        let result = combine_data(input_a, input_b, None);
        assert_streams(result, expected);
    }

    #[test]
    fn test_filter() {
        let input_a = quote! {
            #[attr]
            enum A {
                #[another_attr]
                One,
                Two,
                #[another_attr]
                Three
            }
        };
        let input_b = quote! {
            enum B {
                One,
                Four,
            }
        };
        let expected = quote! {
            enum B {
                One,
                #[another_attr]
                Three,
                Four
            }
        };
        let filter = quote!(filter(Two, attr, another_attr));
        let result = combine_data(input_a, input_b, Some(filter));
        assert_streams(result, expected);
    }
}