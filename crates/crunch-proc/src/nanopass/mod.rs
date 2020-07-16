use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::{
    collections::{HashMap, HashSet},
    mem,
};
use syn::{
    AttributeArgs, Error, Field, Fields, FieldsNamed, FieldsUnnamed, Ident, ItemEnum, Result,
    Variant,
};

mod schema;

use schema::{Context, Operation, Pass, Passes, Transformation};

#[derive(Debug)]
struct VariantData {
    modified: bool,
    variant: Variant,
}

pub struct Nanopass {
    /// All nano passes
    passes: Passes,
    /// The final code that's output
    generated_code: TokenStream,
    /// The variants of the current enum
    current_variants: HashMap<Ident, VariantData>,
    /// The enum the current pass is operating off of
    input_enum: ItemEnum,
    /// The enum that will be created by the current pass
    output_enum: Option<ItemEnum>,
    // FIXME: Better error spans
    file_span: Span,
    /// Every enum produced
    enums: HashMap<Ident, ItemEnum>,
}

impl Nanopass {
    pub fn compile(mut self) -> Result<TokenStream> {
        for pass in mem::take(&mut self.passes.passes) {
            self.compile_pass(pass)?;
        }

        Ok(self.generated_code)
    }

    /// Preforms some sanity checks on a pass to make sure it's alright
    ///
    /// * Variants are not mutated multiple times
    /// * Mutation must always produce a new enum
    ///
    // TODO: All referenced variants exist
    // TODO: Scanning and mutating different variants is ok
    fn verify_pass(&self, pass: &Pass) -> Result<()> {
        let mut mutated_variants = HashSet::new();
        let all_scan = pass.transformations.iter().all(|t| t.is_scan());

        // If the pass just scans then this can be skipped
        if !all_scan {
            for transform in pass.transformations.iter() {
                // If an enum is output and this isn't a scan-only pass
                if pass.output_enum.is_some() {
                    // If this variant has been touched before, throw an error
                    // FIXME: Will throw an error for passes that scan one variant multiple times but mutate others
                    if !mutated_variants.insert(&transform.input_variant) {
                        return Err(Error::new(
                            self.file_span,
                            format!(
                                "The '{}' pass mutates the '{}' variant of the '{}' enum multiple times",
                                pass.name, transform.input_variant, pass.input_enum,
                            ),
                        ));
                    }
                }
            }
        }

        // If the pass mutates yet doesn't produce a new enum throw an error
        if !all_scan && pass.output_enum.is_none() {
            Err(Error::new(
                self.file_span,
                format!(
                    "Multiple of the transformations for the '{}' pass mutate the '{}' enum, but outputEnum is not given\n\
                     Creating an output enum is required for passes that change the enum they operate on in any way, if \
                     you don't want to mutate the enum you're operating on then use `Operation.Scan`",
                    pass.name,
                    pass.input_enum,
                )))
        } else {
            Ok(())
        }
    }

    fn compile_pass(&mut self, mut pass: Pass) -> Result<()> {
        // Make sure the pass looks correct
        self.verify_pass(&pass)?;

        // Clear out the last pass's variant bookkeeping
        self.current_variants.clear();

        // Fill the current variants with metadata for the current set of variants we'll operate on
        let variants = self.passes.base_enum.variants.iter().map(|var| {
            (
                var.ident.clone(),
                VariantData {
                    variant: var.clone(),
                    modified: false,
                },
            )
        });
        self.current_variants.extend(variants);

        // If the pass has an output enum then set self.output_enum to that w/ a copy of the input enum preloaded
        // Additionally, load self.input_enum with the correct enum

        // Get the most recent enum we created or used
        let mut input_enum = self
            .enums
            .get(&pass.input_enum)
            .ok_or_else(|| {
                Error::new(
                    pass.input_enum.span(),
                    format!(
                        "An enum named '{}' does not exist for pass '{}'",
                        pass.input_enum, pass.name
                    ),
                )
            })?
            .clone();

        match pass.output_enum {
            // We're creating a new enum, so we need to do the setup for that
            Some(ref enum_name) => {
                self.input_enum = input_enum.clone();

                // Set the output enum's ident
                input_enum.ident = enum_name.clone();
                self.output_enum = Some(input_enum);
            }

            // Set the input enum to the correct one and output enum to None
            None => {
                self.input_enum = input_enum;
                self.output_enum.take();
            }
        }

        let mut match_arms = TokenStream::new();
        for transform in mem::take(&mut pass.transformations) {
            self.compile_transform(&mut match_arms, &pass, transform)?;
        }

        // For all unchanged variants, generate a straight conversion, e.g.
        //
        // ```rust
        // A::Unit => B::Unit,
        // A::Tuple(_0, _1) => B::Tuple(_0, _1),
        // A::Struct { a, b } => B::Struct { a, b },
        // ```
        if let Some(ref output_enum) = pass.output_enum {
            let input_enum = &pass.input_enum;

            for (
                ident,
                VariantData {
                    variant: Variant { fields, .. },
                    modified,
                },
            ) in self.current_variants.iter()
            {
                if !modified {
                    let fields = get_field_names(fields, true);

                    match_arms.extend(quote! {
                        #input_enum::#ident #fields => #output_enum::#ident #fields,
                    });
                }
            }
        }

        let function_context = match pass.function_context {
            Context::None => quote! {},
            Context::Mutable(ref ctx) => quote! { __user_context: &mut #ctx },
            Context::Immutable(ref ctx) => quote! { __user_context: &#ctx },
        };

        let output_enum = &self.output_enum;
        let vis = &pass.function_vis;

        let pass_name = &pass.function_name;
        let input_enum_name = &pass.input_enum;
        let output_enum_name = match pass.output_enum {
            Some(ref out) => out,
            // If the pass is all scans then the inputted enum is spat out
            None => &pass.input_enum,
        };

        // Add the newly generated pass to the total code
        self.generated_code.extend(quote! {
            #[automatically_derived]
            impl #input_enum_name {
                #vis fn #pass_name(self, #function_context) -> #output_enum_name {
                    match self {
                        #match_arms
                    }
                }
            }

            #[automatically_derived]
            #output_enum
        });

        Ok(())
    }

    fn compile_transform(
        &mut self,
        match_arms: &mut TokenStream,
        pass: &Pass,
        transform: Transformation,
    ) -> Result<()> {
        if !transform.operation.is_scan() {
            self.current_variants
                .get_mut(&transform.input_variant)
                .unwrap()
                .modified = true;
        }

        match transform.operation {
            Operation::Create(ref variant) => {
                let output_enum = self.output_enum.as_mut().expect("Create require an output");

                preform_operation(
                    match_arms,
                    pass,
                    &transform,
                    output_enum,
                    &variant.fields,
                    &variant.ident,
                )?;

                // TODO: I don't like the clone
                output_enum.variants.push(variant.clone());
            }

            Operation::Replace(ref variant) => {
                let output_enum = self
                    .output_enum
                    .as_mut()
                    .expect("Replace requires an output");

                // Remove the old variant
                // FIXME: Good error here
                assert!(remove_variant(output_enum, &transform.input_variant).is_some());

                preform_operation(
                    match_arms,
                    pass,
                    &transform,
                    output_enum,
                    &variant.fields,
                    &variant.ident,
                )?;

                // Add the new variant since we're replacing one
                // TODO: I don't like the clone
                output_enum.variants.push(variant.clone());
            }

            Operation::Merge(ref output_variant) => {
                let output_enum = self.output_enum.as_ref().expect("Merge require an output");

                preform_operation(
                    match_arms,
                    pass,
                    &transform,
                    output_enum,
                    // FIXME: Good error here
                    output_enum
                        .variants
                        .iter()
                        .find_map(|v| {
                            if v.ident == *output_variant {
                                Some(&v.fields)
                            } else {
                                None
                            }
                        })
                        .unwrap(),
                    output_variant,
                )?;
            }

            // FIXME: Some sort of bookkeeping for all the functions that want to scan each variant has to happen
            Operation::Scan => todo!(),
        }

        Ok(())
    }

    pub fn parse_from_attr(args: AttributeArgs, base_enum: ItemEnum) -> Result<Self> {
        let (passes, file_span) = Passes::parse_from_attr(args, base_enum)?;

        let mut generated_code = TokenStream::new();
        passes.base_enum.to_tokens(&mut generated_code);

        let current_variants = HashMap::with_capacity(passes.base_enum.variants.len());
        let input_enum = passes.base_enum.clone();

        let mut enums = HashMap::with_capacity(passes.passes.len());
        enums.insert(passes.base_enum.ident.clone(), passes.base_enum.clone());

        Ok(Self {
            passes,
            generated_code,
            current_variants,
            input_enum,
            output_enum: None,
            enums,
            file_span,
        })
    }
}

fn preform_operation(
    match_arms: &mut TokenStream,
    pass: &Pass,
    transform: &Transformation,
    output_enum: &ItemEnum,
    fields: &Fields,
    output_variant: &Ident,
) -> Result<()> {
    // We need bracing for the pattern match but not for the function call
    let fields_braced = get_field_names(fields, true);
    let fields_unbraced = get_field_names(fields, false);

    // Get all the stuff for quoting
    let user_function = &transform.user_function;
    let (input_enum, input_variant, output_enum) = (
        &pass.input_enum,
        &transform.input_variant,
        &output_enum.ident,
    );
    let context = if pass.function_context.is_none() {
        quote! {}
    } else {
        quote! { __user_context, }
    };

    match_arms.extend(quote! {
        #input_enum::#input_variant #fields_braced => #output_enum::#output_variant(#user_function(#context #fields_unbraced)),
    });

    Ok(())
}

fn remove_variant(enumeration: &mut ItemEnum, ident: &Ident) -> Option<Variant> {
    let position = enumeration
        .variants
        .iter()
        .position(|variant| variant.ident == *ident);

    match position {
        Some(idx) => {
            // `Punctuated<Variant, Comma>` has no way of removing an item, so this is a hack-around
            // FIXME: https://github.com/dtolnay/syn/issues/343
            let mut variants: Vec<Variant> = std::mem::take(&mut enumeration.variants)
                .into_iter()
                .collect();
            let variant = variants.remove(idx);
            enumeration.variants = variants.into_iter().collect();

            Some(variant)
        }

        None => None,
    }
}

/// Gets the names of a variant's fields in a comma separated list. If `bracing`
/// is true then the proper bracing will be included (Parens for tuples & braces for
/// struct variants)
///
/// ```rust
/// UnitVariant
/// // Becomes nothing, since it has no fields
/// ```
///
/// ```rust
/// TupleVariant(usize, usize)
/// // Becomes
/// (_0, _1,)
/// ```
///
/// ```rust
/// StructVariant {
///     a: isize,
///     b: u8,
/// }
/// // Becomes
/// { a, b, }
/// ```
///
fn get_field_names(fields: &Fields, bracing: bool) -> TokenStream {
    match fields {
        Fields::Named(FieldsNamed { named: fields, .. }) => {
            let field_names = fields
                .into_iter()
                .map(|Field { ident, .. }| ident.as_ref().unwrap());

            if bracing {
                quote! {
                    { #( #field_names , )* }
                }
            } else {
                quote! {
                    #( #field_names , )*
                }
            }
        }

        Fields::Unnamed(FieldsUnnamed {
            unnamed: fields, ..
        }) => {
            let field_names = (0..fields.len()).map(|i| format_ident!("_{}", i));

            if bracing {
                quote! {
                    ( #( #field_names , )* )
                }
            } else {
                quote! {
                    #( #field_names , )*
                }
            }
        }

        Fields::Unit => quote! {},
    }
}
