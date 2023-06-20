use proc_macro2::{Ident, Span};
use proc_macro::{TokenStream};
use syn::{parse_macro_input, DeriveInput};
use quote::quote;

/// Procedural attribute macro that combines ("extends") data.
///
/// Generates a new procedural macro which allows you to extend some data structure A into data structure B. 
///
/// See extendable_data::combine_data for how the data is actually combined.
#[proc_macro_attribute]
pub fn extendable_data(args: TokenStream, source: TokenStream) -> TokenStream {
    let mut name_string = args.to_string();
    let source_ast = parse_macro_input!(source as DeriveInput);
    if args.is_empty() {
        name_string = format!("extend_from_{}", source_ast.ident);
    }
    let name = Ident::new(&name_string, Span::call_site());
    TokenStream::from(quote! {
        use quote::quote;
        use proc_macro2::TokenStream as TokenStream2;
        use proc_macro::TokenStream;

        #[proc_macro_attribute]
        pub fn #name(args: TokenStream, dst: TokenStream) -> TokenStream {
            let base = TokenStream2::from(quote! {
                #source_ast
            });
            let dst_convert: TokenStream2 = dst.into();
            let args_convert: TokenStream2 = args.into();
            let resp: TokenStream = extendable_data::combine_data(base, dst_convert, Some(args_convert)).into();
            resp
        }
    })
}
