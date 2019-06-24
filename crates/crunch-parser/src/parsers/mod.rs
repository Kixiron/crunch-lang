#![allow(dead_code)]

mod binary_operations;
mod function;
mod literal;
mod loops;
mod variable;
mod variable_parsing;
mod prelude {
    pub use crate::*;
    pub use crunch_error::*;
    pub use crunch_token::*;
    pub use typed_arena::Arena;
}

pub use binary_operations::*;
pub use function::*;
pub use literal::*;
pub use loops::*;
pub use variable::*;
pub use variable_parsing::*;
