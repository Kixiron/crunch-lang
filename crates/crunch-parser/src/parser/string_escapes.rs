use crate::error::{Error, SyntaxError};

use alloc::{format, string::String, vec::IntoIter, vec::Vec};
use core::{char, ops::Range};

// TODO: Verify that these all work and are actually recognized by the parser/lexer and supported by
//       the backends
// TODO: Have this process inlined variables & formatting
/// Turns all escape sequences in a string into their actual representation
#[inline]
pub(super) fn unescape_string(queue: Vec<char>) -> Result<String, (Error, Range<usize>)> {
    let mut s = String::with_capacity(queue.len());
    let mut queue = CharStream::new(queue.into_iter());
    let mut index = 0;

    while let Ok(c) = queue.next(&mut index) {
        if c != '\\' {
            s.push(c);
            continue;
        }

        match queue.next(&mut index)? {
            '\\' => s.push('\\'),
            '"' => s.push('"'),
            '\'' => s.push('\''),
            'n' => s.push('\n'),
            'r' => s.push('\r'),
            't' => s.push('\t'),
            '0' => s.push('\0'),
            'x' => s.push(byte(&mut queue, &mut index)?),
            'u' => s.push(unicode_16(&mut queue, index, &mut index)?),
            'U' => s.push(unicode_32(&mut queue, index, &mut index)?),
            'o' => s.push(octal(&mut queue, index, &mut index)?),
            'b' => s.push(binary(&mut queue, &mut index)?),

            c => {
                return Err((
                    Error::Syntax(SyntaxError::UnrecognizedEscapeSeq(c)),
                    index - 1..index,
                ));
            }
        };
    }

    Ok(s)
}

macro_rules! missing_braces {
    ($index:expr) => {
        Err((
            Error::Syntax(SyntaxError::MissingEscapeBraces),
            *$index - 1..*$index,
        ))
    };
}

#[inline]
fn unicode_16(
    queue: &mut CharStream,
    start: usize,
    index: &mut usize,
) -> Result<char, (Error, Range<usize>)> {
    if queue.next(index)? != '{' {
        return missing_braces!(index);
    }

    let mut number = 0;
    for i in (0..4).rev() {
        let mut digit = queue.next(index)? as u32;

        if digit >= '0' as u32 && digit <= '9' as u32 {
            digit -= '0' as u32;
        } else if digit >= 'a' as u32 && digit <= 'f' as u32 {
            digit = (digit - 'a' as u32) + 10;
        } else if digit >= 'A' as u32 && digit <= 'F' as u32 {
            digit = (digit - 'A' as u32) + 10;
        } else {
            return Err((
                Error::Syntax(SyntaxError::InvalidEscapeCharacters(
                    "'0'..'9', 'a'..'f' and 'A'..'F'",
                )),
                *index..*index,
            ));
        }

        number += digit * 16u32.pow(i);
    }

    if queue.next(index)? != '}' {
        return missing_braces!(index);
    }

    char::from_u32(number).ok_or((
        Error::Syntax(SyntaxError::InvalidEscapeSeq(format!(
            "`\\u{{{:X}}}`",
            number
        ))),
        start..*index,
    ))
}

#[inline]
fn unicode_32(
    queue: &mut CharStream,
    start: usize,
    index: &mut usize,
) -> Result<char, (Error, Range<usize>)> {
    if queue.next(index)? != '{' {
        return missing_braces!(index);
    }

    let mut number = 0;
    for i in (0..8).rev() {
        let mut digit = queue.next(index)? as u32;

        if digit >= '0' as u32 && digit <= '9' as u32 {
            digit -= '0' as u32;
        } else if digit >= 'a' as u32 && digit <= 'f' as u32 {
            digit = (digit - 'a' as u32) + 10;
        } else if digit >= 'A' as u32 && digit <= 'F' as u32 {
            digit = (digit - 'A' as u32) + 10;
        } else {
            return Err((
                Error::Syntax(SyntaxError::InvalidEscapeCharacters(
                    "'0'..'9', 'a'..'f' and 'A'..'F'",
                )),
                *index..*index,
            ));
        }

        number += digit * 16u32.pow(i);
    }

    if queue.next(index)? != '}' {
        return missing_braces!(index);
    }

    char::from_u32(number).ok_or((
        Error::Syntax(SyntaxError::InvalidEscapeSeq(format!(
            "`\\U{{{:X}}}`",
            number
        ))),
        start..*index,
    ))
}

#[inline]
fn byte(queue: &mut CharStream, index: &mut usize) -> Result<char, (Error, Range<usize>)> {
    if queue.next(index)? != '{' {
        return missing_braces!(index);
    }

    let mut number = 0;
    for i in (0..2).rev() {
        let mut digit = queue.next(index)? as u32;

        if digit >= '0' as u32 && digit <= '9' as u32 {
            digit -= '0' as u32;
        } else if digit >= 'a' as u32 && digit <= 'f' as u32 {
            digit = (digit - 'a' as u32) + 10;
        } else if digit >= 'A' as u32 && digit <= 'F' as u32 {
            digit = (digit - 'A' as u32) + 10;
        } else {
            return Err((
                Error::Syntax(SyntaxError::InvalidEscapeCharacters(
                    "'0'..'9', 'a'..'f' and 'A'..'F'",
                )),
                *index..*index,
            ));
        }

        number += digit as u8 * 16u8.pow(i);
    }

    if queue.next(index)? != '}' {
        return missing_braces!(index);
    }

    Ok(number as char)
}

#[inline]
fn octal(
    queue: &mut CharStream,
    start: usize,
    index: &mut usize,
) -> Result<char, (Error, Range<usize>)> {
    if queue.next(index)? != '{' {
        return missing_braces!(index);
    }

    let mut number = 0;
    for i in (0..3).rev() {
        let mut digit = queue.next(index)? as u32;

        if digit >= '0' as u32 && digit <= '7' as u32 {
            digit -= '0' as u32;
        } else {
            return Err((
                Error::Syntax(SyntaxError::InvalidEscapeCharacters("'0'..'7'")),
                *index..*index,
            ));
        }

        number += digit * 8u32.pow(i);
    }

    if queue.next(index)? != '}' {
        return missing_braces!(index);
    }

    char::from_u32(number).ok_or((
        Error::Syntax(SyntaxError::InvalidEscapeSeq(format!(
            "`\\o{{{:o}}}`",
            number
        ))),
        start..*index,
    ))
}

#[inline]
fn binary(queue: &mut CharStream, index: &mut usize) -> Result<char, (Error, Range<usize>)> {
    if queue.next(index)? != '{' {
        return missing_braces!(index);
    }

    let mut number: u8 = 0;
    for i in (0..8).rev() {
        let mut digit = queue.next(index)? as u32;

        if digit == '0' as u32 || digit == '1' as u32 {
            digit -= '0' as u32
        } else {
            return Err((
                Error::Syntax(SyntaxError::InvalidEscapeCharacters("'0'..'1'")),
                *index..*index,
            ));
        }

        number += digit as u8 * 2u8.pow(i);
    }

    if queue.next(index)? != '}' {
        return missing_braces!(index);
    }

    Ok(number as char)
}

struct CharStream(IntoIter<char>);

impl CharStream {
    pub fn new(chars: IntoIter<char>) -> Self {
        Self(chars)
    }

    pub fn next(&mut self, index: &mut usize) -> Result<char, (Error, Range<usize>)> {
        if let Some(c) = self.0.next() {
            *index += 1;

            Ok(c)
        } else {
            Err((
                Error::Syntax(SyntaxError::MissingEscapeSpecifier),
                *index..*index,
            ))
        }
    }
}

// TODO: Test all types
#[cfg(test)]
mod tests {
    use super::*;

    fn chars(s: &str) -> Vec<char> {
        s.chars().collect()
    }

    #[test]
    fn unicode_16bit() {
        assert_eq!(Some(" ".into()), unescape_string(chars(r"\u{0020}")).ok());
        assert_eq!(Some("%".into()), unescape_string(chars(r"\u{0025}")).ok());
        assert_eq!(Some("Z".into()), unescape_string(chars(r"\u{005A}")).ok());
        assert_eq!(Some("}".into()), unescape_string(chars(r"\u{007D}")).ok());
        assert_eq!(Some("®".into()), unescape_string(chars(r"\u{00AE}")).ok());
        assert_eq!(Some("Ø".into()), unescape_string(chars(r"\u{00D8}")).ok());
        assert_eq!(Some("Ɯ".into()), unescape_string(chars(r"\u{019C}")).ok());
        assert_eq!(Some("Ǌ".into()), unescape_string(chars(r"\u{01CA}")).ok());
        assert_eq!(Some("Ω".into()), unescape_string(chars(r"\u{03A9}")).ok());
        assert_eq!(Some("Ӿ".into()), unescape_string(chars(r"\u{04FE}")).ok());
        assert_eq!(Some("▙".into()), unescape_string(chars(r"\u{2599}")).ok());
        assert_eq!(Some("凰".into()), unescape_string(chars(r"\u{51F0}")).ok());
        assert_eq!(None, unescape_string(chars(r"\u2599}")).ok());
        assert_eq!(None, unescape_string(chars(r"\u{t59ertwe}")).ok());
        assert_eq!(None, unescape_string(chars(r"\u{tf}")).ok());
        assert_eq!(None, unescape_string(chars(r"\u{tdftr9}")).ok());
        assert_eq!(None, unescape_string(chars(r"\u{}")).ok());
    }
}
