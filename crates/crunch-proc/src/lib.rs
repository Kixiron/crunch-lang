use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, AttributeArgs, Error, ItemFn, Lit, Meta,
    MetaNameValue, NestedMeta, Result,
};

/// A macro for use in the parser, inserts a stack frame recording into the function
///
/// ```rust
/// # use crunch_proc::recursion_guard;
/// # struct Dummy;
/// # impl Dummy {
/// #     fn add_stack_frame(&self) -> Result<(), ()> { Ok(()) }
/// #
/// // Counting a the function as a single frame
/// #[recursion_guard]
/// # fn a(&self) -> Result<(), ()> { Ok(()) }
/// #[recursion_guard()]
/// # fn b(&self) -> Result<(), ()> { Ok(()) }
///
/// // Counting the function as more than one frame, `1` can be any integer
/// #[recursion_guard(1)]
/// # fn c(&self) -> Result<(), ()> { Ok(()) }
/// #[recursion_guard(frames = 1)]
/// # fn d(&self) -> Result<(), ()> { Ok(()) }
/// # }
/// ```
#[proc_macro_attribute]
pub fn recursion_guard(
    attrs: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    recursion_guard_inner(
        parse_macro_input!(attrs as _),
        parse_macro_input!(input as _),
    )
    .unwrap_or_else(|err| err.to_compile_error())
    .into()
}

fn recursion_guard_inner(mut meta: AttributeArgs, mut input: ItemFn) -> Result<TokenStream> {
    let frames_added = if meta.is_empty() {
        Ok(1)
    } else if meta.len() > 1 {
        Err(Error::new(
            Span::join(
                &meta.first().expect("There's more than 1").span(),
                meta.last().expect("There's more than 1").span(),
            )
            .expect("An attribute can only be from one file"),
            "Only one item is allowed for declaring the number of frames to add",
        ))
    } else {
        let meta = meta.pop().expect("There is exactly 1");

        match meta {
            NestedMeta::Lit(Lit::Int(int)) => int.base10_parse(),

            NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                path,
                lit: Lit::Int(int),
                ..
            })) if path.is_ident("frames") => int.base10_parse(),

            NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                lit: Lit::Int(..), ..
            })) => Err(Error::new(meta.span(), "Only integer literals are allowed")),

            meta => Err(Error::new(meta.span(), "Unrecognized attribute")),
        }
    }?;

    let block = &mut input.block;
    let add_stack_frames = (0..frames_added).map(|_| {
        quote! {
            let _frame = self.add_stack_frame()?;
        }
    });

    *block = parse_quote!({
        #(#add_stack_frames)*
        #block
    });

    Ok(quote! {
        #input
    })
}
