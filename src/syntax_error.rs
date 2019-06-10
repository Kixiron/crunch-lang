use std::fmt::{self, Display, Formatter};

#[derive(PartialEq, Debug)]
pub enum SyntaxError {
    MissingRightHand,
    MissingLeftHand,
    InvalidType,
    NoIdentifier,
    Unsupported,
    MissingClosingBracket,
}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use SyntaxError::*;

        match *self {
            MissingRightHand => write!(f, "No right hand side was supplied"),
            MissingLeftHand => write!(f, "No left hand side was supplied"),
            InvalidType => write!(f, "An invalid type was supplied"),
            NoIdentifier => write!(f, "A `let` binding was created, but without an identifier"),
            Unsupported => write!(f, "A token that is not currently supported was used"),
            MissingClosingBracket => write!(
                f,
                "An expression that requires a closing bracket was left open"
            ),
        }
    }
}
