use proc_macro2::{Ident, Span};
use proc_macro::{TokenStream};
use syn::{parse_macro_input, DeriveInput};
use quote::quote;

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
        pub fn #name(_attr: TokenStream, dst: TokenStream) -> TokenStream {
            let base = TokenStream2::from(quote! {
                #source_ast
            });
            let dst_convert: TokenStream2 = dst.into();
            let resp: TokenStream = extendable_data::combine_data(base, dst_convert).into();
            resp
        }
    })
}
