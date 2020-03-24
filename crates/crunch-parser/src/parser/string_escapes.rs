use crunch_error::parse_prelude::Diagnostic;

use alloc::{format, string::String, vec, vec::IntoIter, vec::Vec};
use core::char;

// TODO: Verify that these all work and are actually recognized by the parser/lexer and supported by
//       the backends
// TODO: Have this process inlined variables & formatting
/// Turns all escape sequences in a string into their actual representation
#[inline]
pub(super) fn unescape_string(queue: Vec<char>) -> Result<String, Vec<Diagnostic<usize>>> {
    let mut s = String::with_capacity(queue.len());
    let mut queue = CharStream::new(queue.into_iter());

    while let Ok(c) = queue.next() {
        if c != '\\' {
            s.push(c);
            continue;
        }

        match queue.next()? {
            '\\' => s.push('\\'),
            '"' => s.push('"'),
            '\'' => s.push('\''),
            'n' => s.push('\n'),
            'r' => s.push('\r'),
            't' => s.push('\t'),
            '0' => s.push('\0'),
            'x' => s.push(byte(&mut queue)?),
            'u' => s.push(unicode_16(&mut queue)?),
            'U' => s.push(unicode_32(&mut queue)?),
            'o' => s.push(octal(&mut queue)?),
            'b' => s.push(binary(&mut queue)?),

            c => {
                return Err(vec![Diagnostic::error()
                    .with_message(format!("Unrecognized escape sequence: `\\{}`", c))]);
            }
        };
    }

    Ok(s)
}

#[inline]
fn unicode_16(queue: &mut CharStream) -> Result<char, Vec<Diagnostic<usize>>> {
    if queue.next()? != '{' {
        return Err(vec![
            Diagnostic::error().with_message("Unicode escape sequences start with a `{`")
        ]);
    }

    let mut number = 0;
    for i in (0..4).rev() {
        let mut digit = queue.next()? as u32;

        if digit >= '0' as u32 && digit <= '9' as u32 {
            digit -= '0' as u32;
        } else if digit >= 'a' as u32 && digit <= 'f' as u32 {
            digit = (digit - 'a' as u32) + 10;
        } else if digit >= 'A' as u32 && digit <= 'F' as u32 {
            digit = (digit - 'A' as u32) + 10;
        } else {
            return Err(vec![Diagnostic::error().with_message(
                "Only characters from '0'..'9', 'a'..'f' and 'A'..'F' are allowed inside unicode escapes",
            )]);
        }

        number += digit * 16u32.pow(i);
    }

    if queue.next()? != '}' {
        return Err(vec![
            Diagnostic::error().with_message("Unicode escape sequences end with a `}`")
        ]);
    }

    char::from_u32(number).ok_or(vec![
        Diagnostic::error().with_message(format!("Invalid unicode escape: `\\u{{{:X}}}`", number))
    ])
}

#[inline]
fn unicode_32(queue: &mut CharStream) -> Result<char, Vec<Diagnostic<usize>>> {
    if queue.next()? != '{' {
        return Err(vec![
            Diagnostic::error().with_message("Unicode escape sequences start with a `{`")
        ]);
    }

    let mut number = 0;
    for i in (0..8).rev() {
        let mut digit = queue.next()? as u32;

        if digit >= '0' as u32 && digit <= '9' as u32 {
            digit -= '0' as u32;
        } else if digit >= 'a' as u32 && digit <= 'f' as u32 {
            digit = (digit - 'a' as u32) + 10;
        } else if digit >= 'A' as u32 && digit <= 'F' as u32 {
            digit = (digit - 'A' as u32) + 10;
        } else {
            return Err(vec![Diagnostic::error().with_message(
                "Only characters from '0'..'9', 'a'..'f' and 'A'..'F' are allowed inside unicode escapes",
            )]);
        }

        number += digit * 16u32.pow(i);
    }

    if queue.next()? != '}' {
        return Err(vec![
            Diagnostic::error().with_message("Unicode escape sequences end with a `}`")
        ]);
    }

    char::from_u32(number).ok_or(vec![
        Diagnostic::error().with_message(format!("Invalid unicode escape: `\\U{{{:X}}}`", number))
    ])
}

#[inline]
fn byte(queue: &mut CharStream) -> Result<char, Vec<Diagnostic<usize>>> {
    if queue.next()? != '{' {
        return Err(vec![
            Diagnostic::error().with_message("Byte escape sequences start with a `{`")
        ]);
    }

    let mut number = 0;
    for i in (0..2).rev() {
        let mut digit = queue.next()? as u32;

        if digit >= '0' as u32 && digit <= '9' as u32 {
            digit -= '0' as u32;
        } else if digit >= 'a' as u32 && digit <= 'f' as u32 {
            digit = (digit - 'a' as u32) + 10;
        } else if digit >= 'A' as u32 && digit <= 'F' as u32 {
            digit = (digit - 'A' as u32) + 10;
        } else {
            return Err(vec![Diagnostic::error().with_message(
                "Only characters from '0'..'9', 'a'..'f' and 'A'..'F' are allowed inside byte escapes",
            )]);
        }

        number += digit as u8 * 16u8.pow(i);
    }

    if queue.next()? != '}' {
        return Err(vec![
            Diagnostic::error().with_message("Byte escape sequences end with a `}`")
        ]);
    }

    Ok(number as char)
}

#[inline]
fn octal(queue: &mut CharStream) -> Result<char, Vec<Diagnostic<usize>>> {
    if queue.next()? != '{' {
        return Err(vec![
            Diagnostic::error().with_message("Octal escape sequences start with a `{`")
        ]);
    }

    let mut number = 0;
    for i in (0..3).rev() {
        let mut digit = queue.next()? as u32;

        if digit >= '0' as u32 && digit <= '7' as u32 {
            digit -= '0' as u32;
        } else {
            return Err(vec![Diagnostic::error().with_message(
                "Only characters from '0'..'7' are allowed inside octal escapes",
            )]);
        }

        number += digit * 8u32.pow(i);
    }

    if queue.next()? != '}' {
        return Err(vec![
            Diagnostic::error().with_message("Octal escape sequences end with a `}`")
        ]);
    }

    char::from_u32(number).ok_or(vec![
        Diagnostic::error().with_message(format!("Invalid octal escape: `\\o{{{:o}}}`", number))
    ])
}

#[inline]
fn binary(queue: &mut CharStream) -> Result<char, Vec<Diagnostic<usize>>> {
    if queue.next()? != '{' {
        return Err(vec![
            Diagnostic::error().with_message("Binary escape sequences start with a `{`")
        ]);
    }

    let mut number: u8 = 0;
    for i in (0..8).rev() {
        let mut digit = queue.next()? as u32;

        if digit == '0' as u32 || digit == '1' as u32 {
            digit -= '0' as u32
        } else {
            return Err(vec![Diagnostic::error().with_message(
                "Only characters from '0'..'1' are allowed inside binary escapes",
            )]);
        }

        number += digit as u8 * 2u8.pow(i);
    }

    if queue.next()? != '}' {
        return Err(vec![
            Diagnostic::error().with_message("Binary escape sequences end with a `}`")
        ]);
    }

    Ok(number as char)
}

struct CharStream(IntoIter<char>);

impl CharStream {
    pub fn new(chars: IntoIter<char>) -> Self {
        Self(chars)
    }

    pub fn next(&mut self) -> Result<char, Vec<Diagnostic<usize>>> {
        if let Some(c) = self.0.next() {
            Ok(c)
        } else {
            Err(vec![
                Diagnostic::error().with_message("Unexpected end of string literal")
            ])
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
