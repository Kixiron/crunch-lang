use proc_macro2::{Span, TokenStream};
use serde::Deserialize;
use serde_dhall::SimpleType;
use std::{fs, path::PathBuf};
use syn::{
    spanned::Spanned, AttributeArgs, Error, Ident, ItemEnum, Lit, Meta, MetaNameValue, NestedMeta,
    Path, Result, Variant,
};

#[derive(Debug)]
pub struct Passes {
    pub base_enum: ItemEnum,
    pub config: NanopassConfig,
    pub passes: Vec<Pass>,
}

impl Passes {
    pub fn parse_from_attr(args: AttributeArgs, base_enum: ItemEnum) -> Result<(Self, Span)> {
        let (file_path, file_span): (String, Span) = args
            .into_iter()
            .find_map(|arg| match arg {
                NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                    path,
                    lit: Lit::Str(name),
                    ..
                })) if path.is_ident("file") => Some((name.value(), name.span())),

                _ => None,
            })
            .ok_or_else(|| Error::new(base_enum.span(), "Missing the `file` attribute"))?;

        let manifest_dir: PathBuf = std::env::var("CARGO_MANIFEST_DIR")
            .expect("Cargo should set CARGO_MANIFEST DIR")
            .into();
        let dhall = fs::read_to_string(manifest_dir.join(&file_path)).map_err(|err| {
            Error::new(
                file_span,
                format!(
                    "Failed to read '{}' in the '{}' directory: {:?}",
                    &file_path,
                    manifest_dir.display(),
                    err,
                ),
            )
        })?;

        let dhall_schema: SimpleType = serde_dhall::from_str(include_str!("./schema.dhall"))
            .parse()
            .expect("Invalid nanopass schema");

        let raw::RawNanopass { config, passes } = serde_dhall::from_str(&dhall)
            .type_annotation(&dhall_schema)
            .parse()
            .map_err(|err| Error::new(file_span, format!("Dhall error: {}", err)))?;

        let passes = passes
            .into_iter()
            .map(|pass| pass.parse())
            .collect::<Result<_>>()?;

        Ok((
            Self {
                base_enum,
                config,
                passes,
            },
            file_span,
        ))
    }
}

#[derive(Debug)]
pub struct Pass {
    pub name: String,
    pub description: Option<String>,
    pub function_name: Ident,
    pub function_vis: TokenStream,
    pub function_context: Context,
    pub input_enum: Ident,
    pub output_enum: Option<Ident>,
    pub transformations: Vec<Transformation>,
}

#[derive(Debug)]
pub enum Context {
    None,
    Mutable(Path),
    Immutable(Path),
}

impl Context {
    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }
}

#[derive(Debug)]
pub struct Transformation {
    pub input_variant: Ident,
    pub operation: Operation,
    pub user_function: Path,
}

impl Transformation {
    pub fn is_scan(&self) -> bool {
        self.operation.is_scan()
    }
}

#[derive(Debug)]
pub enum Operation {
    Create(Variant),
    Replace(Variant),
    Merge(Ident),
    Scan,
}

impl Operation {
    pub fn is_scan(&self) -> bool {
        matches!(self, Self::Scan)
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename = "Config", rename_all = "camelCase")]
pub struct NanopassConfig {
    pub logging: bool,
}

mod raw {
    use super::{Context, NanopassConfig, Operation, Pass, Transformation};
    use proc_macro2::TokenStream;
    use quote::quote;
    use serde::Deserialize;
    use serde_dhall::SimpleType;
    use std::fmt::Display;
    use syn::{
        parse::{Parse, ParseStream},
        Error, Result,
    };

    fn maybe_parse<T: Parse, M: Display>(input: Option<&str>, msg: M) -> Result<Option<T>> {
        input
            .map_or_else(|| Ok(None), |out| syn::parse_str(&out).map(Some))
            .map_err(|err| annotate_error(err, msg))
    }

    fn annotate_error<M: Display>(orig: Error, msg: M) -> Error {
        let mut err = Error::new(orig.span(), msg);
        err.combine(orig);

        err
    }

    #[derive(Deserialize)]
    #[serde(rename = "Pass", rename_all = "camelCase")]
    pub struct RawNanopass {
        pub config: NanopassConfig,
        pub passes: Vec<RawPass>,
    }

    impl Parse for RawNanopass {
        fn parse(input: ParseStream) -> Result<Self> {
            let dhall_schema: SimpleType = serde_dhall::from_str(include_str!("./schema.dhall"))
                .parse()
                .expect("Invalid nanopass schema");

            let dhall_span = input.span();
            let dhall = input.to_string();

            // Eat the tokens of the string we just grabbed so we don't error out
            let _: TokenStream = input.parse().expect("Unreachable");

            serde_dhall::from_str(&dhall)
                .type_annotation(&dhall_schema)
                .parse()
                .map_err(|err| Error::new(dhall_span, err))
        }
    }

    #[derive(Deserialize)]
    #[serde(rename = "Context")]
    enum RawContext {
        Immutable(String),
        Mutable(String),
        None,
    }

    impl RawContext {
        fn parse(self) -> Result<Context> {
            let ctx = match self {
                Self::Immutable(ctx) => {
                    let path = syn::parse_str(&ctx).map_err(|err| {
                        annotate_error(err, "Context.Immutable must be a valid Rust path")
                    })?;

                    Context::Immutable(path)
                }

                Self::Mutable(ctx) => {
                    let path = syn::parse_str(&ctx).map_err(|err| {
                        annotate_error(err, "Context.Mutable must be a valid Rust path")
                    })?;

                    Context::Mutable(path)
                }

                Self::None => Context::None,
            };

            Ok(ctx)
        }
    }

    // TODO: More visibilities
    #[derive(Deserialize)]
    #[serde(rename = "Visibility")]
    enum RawVisibility {
        Private,
        Public,
        Crate,
        Super,
    }

    impl RawVisibility {
        fn parse(self) -> Result<TokenStream> {
            let vis = match self {
                Self::Private => quote! {},
                Self::Public => quote! { pub },
                Self::Crate => quote! { pub(crate) },
                Self::Super => quote! { pub(super) },
            };

            Ok(vis)
        }
    }

    #[derive(Deserialize)]
    #[serde(rename = "Pass", rename_all = "camelCase")]
    pub struct RawPass {
        name: String,
        description: Option<String>,
        function_name: String,
        function_vis: RawVisibility,
        function_context: RawContext,
        input_enum: String,
        output_enum: Option<String>,
        transformations: Vec<RawTransformation>,
    }

    impl RawPass {
        pub fn parse(self) -> Result<Pass> {
            let function_name = syn::parse_str(&self.function_name).map_err(|err| {
                annotate_error(err, "Pass.functionName must be a valid Rust ident")
            })?;

            let input_enum = syn::parse_str(&self.input_enum)
                .map_err(|err| annotate_error(err, "Pass.inputEnum must be a valid Rust ident"))?;

            let output_enum = maybe_parse(
                self.output_enum.as_deref(),
                "Pass.outputEnum must be a valid Rust ident",
            )?;

            let transformations = self
                .transformations
                .into_iter()
                .map(|transformation| transformation.parse())
                .collect::<Result<Vec<Transformation>>>()?;

            Ok(Pass {
                name: self.name,
                description: self.description,
                function_name,
                function_vis: self.function_vis.parse()?,
                function_context: self.function_context.parse()?,
                input_enum,
                output_enum,
                transformations,
            })
        }
    }

    #[derive(Deserialize)]
    #[serde(rename = "Operation")]
    enum RawOperation {
        Create(String),
        Merge(String),
        Replace(String),
        Scan,
    }

    impl RawOperation {
        fn parse(self) -> Result<Operation> {
            let op = match self {
                Self::Create(variant) => {
                    let variant = syn::parse_str(&variant).map_err(|err| {
                        annotate_error(err, "Operation.Create must be a valid Rust variant")
                    })?;

                    Operation::Create(variant)
                }
                Self::Merge(variant) => {
                    let variant = syn::parse_str(&variant).map_err(|err| {
                        annotate_error(err, "Operation.Merge must be a valid Rust variant")
                    })?;

                    Operation::Merge(variant)
                }
                Self::Replace(variant) => {
                    let variant = syn::parse_str(&variant).map_err(|err| {
                        annotate_error(err, "Operation.Replace must be a valid Rust variant")
                    })?;

                    Operation::Replace(variant)
                }
                Self::Scan => Operation::Scan,
            };

            Ok(op)
        }
    }

    #[derive(Deserialize)]
    #[serde(rename = "Transformation", rename_all = "camelCase")]
    struct RawTransformation {
        input_variant: String,
        operation: RawOperation,
        user_function: String,
    }

    impl RawTransformation {
        fn parse(self) -> Result<Transformation> {
            let input_variant = syn::parse_str(&self.input_variant).map_err(|err| {
                annotate_error(
                    err,
                    "Transformation.inputVariant must be a valid Rust ident",
                )
            })?;

            let user_function = syn::parse_str(&self.user_function).map_err(|err| {
                annotate_error(err, "Transformation.userFunction must be a valid Rust path")
            })?;

            Ok(Transformation {
                input_variant,
                operation: self.operation.parse()?,
                user_function,
            })
        }
    }
}
