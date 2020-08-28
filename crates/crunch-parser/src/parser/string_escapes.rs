use alloc::{
    format,
    string::{String, ToString},
};
use core::{char, ops::Range};
use crunch_shared::{
    error::{Error, SyntaxError},
    trees::ast::{Rune, Text},
};

// TODO: Finish logos string lexer and add string escapes
//
// use logos::{Lexer, Logos};
//
// #[derive(Logos, Debug, PartialEq, Eq, Clone, Copy, Hash)]
// #[logos(subpattern hex = "[0-9a-fA-F]", subpattern oct = "[0-7]")]
// enum EscapeToken {
//     #[error]
//     Error,
//
//     #[token("\\\\")]
//     Backslash,
//     #[token("\\n")]
//     Newline,
//     #[token("\\t")]
//     Tab,
//     #[token("\\r")]
//     CarriageReturn,
//     #[token("\\'")]
//     SingleQuote,
//     #[token("\\\"")]
//     DoubleQuote,
//     #[regex("\\\\x\\{(?&hex)(?&hex)\\}")]
//     Hex,
//     #[regex("\\\\u\\{(?&hex)(?&hex)(?&hex)(?&hex)\\}")]
//     Unicode16,
//     #[regex("\\\\U\\{(?&hex)(?&hex)(?&hex)(?&hex)(?&hex)(?&hex)(?&hex)(?&hex)\\}")]
//     Unicode32,
//     #[regex("\\\\o\\{(?&oct)(?&oct)(?&oct)\\}")]
//     Octal,
//     #[regex("\\\\b\\{[01][01][01][01][01][01][01][01]\\}")]
//     Binary,
//     #[regex(".")]
//     Other,
// }

// TODO: Verify that these all work and are actually recognized by the parser/lexer and supported by
//       the backends
// TODO: Have this process inlined variables & formatting
/// Turns all escape sequences in a string into their actual representation
pub(super) fn unescape_string<I: Iterator<Item = char>>(
    queue: I,
) -> Result<Text, (Error, Range<usize>)> {
    let (low, high) = queue.size_hint();
    let mut s = String::with_capacity(high.unwrap_or(low));

    let mut queue = CharStream::new(queue);
    let mut index = 0;

    while let Ok(c) = queue.next(&mut index) {
        if c != '\\' {
            s.push(c);
            continue;
        }

        #[rustfmt::skip]
        match queue.next(&mut index)? {
            '\\' => s.push('\\'),
            '"'  => s.push('"'),
            '\'' => s.push('\''),
            'n'  => s.push('\n'),
            'r'  => s.push('\r'),
            't'  => s.push('\t'),
            '0'  => s.push('\0'),
            'x'  => s.push(byte(&mut queue, &mut index)?.as_char()),
            'u'  => s.push(unicode_16(&mut queue, index, &mut index)?.as_char()),
            'U'  => s.push(unicode_32(&mut queue, index, &mut index)?.as_char()),
            'o'  => s.push(octal(&mut queue, index, &mut index)?.as_char()),
            'b'  => s.push(binary(&mut queue, &mut index)?.as_char()),

            c => {
                return Err((
                    Error::Syntax(SyntaxError::UnrecognizedEscapeSeq(c)),
                    index - 1..index,
                ));
            }
        };
    }

    Ok(Text::new(s))
}

pub(super) fn unescape_rune<I: Iterator<Item = char>>(
    queue: I,
) -> Result<Rune, (Error, Range<usize>)> {
    let mut queue = CharStream::new(queue);
    let mut index = 0;

    let c = queue.next(&mut index)?;
    if c != '\\' {
        Ok(Rune::from_char(c))
    } else {
        match queue.next(&mut index)? {
            '\\' => Ok(Rune::from_char('\\')),
            '"' => Ok(Rune::from_char('"')),
            '\'' => Ok(Rune::from_char('\'')),
            'n' => Ok(Rune::from_char('\n')),
            'r' => Ok(Rune::from_char('\r')),
            't' => Ok(Rune::from_char('\t')),
            'x' => Ok(byte(&mut queue, &mut index)?),
            'u' => Ok(unicode_16(&mut queue, index, &mut index)?),
            'U' => Ok(unicode_32(&mut queue, index, &mut index)?),
            'o' => Ok(octal(&mut queue, index, &mut index)?),
            'b' => Ok(binary(&mut queue, &mut index)?),

            err => Err((
                Error::Syntax(SyntaxError::UnrecognizedEscapeSeq(err)),
                index - 1..index,
            )),
        }
    }
}

macro_rules! missing_braces {
    ($index:expr) => {
        Err((
            Error::Syntax(SyntaxError::MissingEscapeBraces),
            *$index - 1..*$index,
        ))
    };
}

fn unicode_16<I: Iterator<Item = char>>(
    queue: &mut CharStream<I>,
    start: usize,
    index: &mut usize,
) -> Result<Rune, (Error, Range<usize>)> {
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
                    "'0'..'9', 'a'..'f' and 'A'..'F'".to_string(),
                )),
                *index..*index,
            ));
        }

        number += digit * 16u32.pow(i);
    }

    if queue.next(index)? != '}' {
        return missing_braces!(index);
    }

    Rune::from_u32(number).ok_or((
        Error::Syntax(SyntaxError::InvalidEscapeSeq(format!(
            "`\\u{{{:X}}}`",
            number
        ))),
        start..*index,
    ))
}

fn unicode_32<I: Iterator<Item = char>>(
    queue: &mut CharStream<I>,
    start: usize,
    index: &mut usize,
) -> Result<Rune, (Error, Range<usize>)> {
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
                    "'0'..'9', 'a'..'f' and 'A'..'F'".to_string(),
                )),
                *index..*index,
            ));
        }

        number += digit * 16u32.pow(i);
    }

    if queue.next(index)? != '}' {
        return missing_braces!(index);
    }

    Rune::from_u32(number).ok_or((
        Error::Syntax(SyntaxError::InvalidEscapeSeq(format!(
            "`\\U{{{:X}}}`",
            number
        ))),
        start..*index,
    ))
}

fn byte<I: Iterator<Item = char>>(
    queue: &mut CharStream<I>,
    index: &mut usize,
) -> Result<Rune, (Error, Range<usize>)> {
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
                    "'0'..'9', 'a'..'f' and 'A'..'F'".to_string(),
                )),
                *index..*index,
            ));
        }

        number += digit as u8 * 16u8.pow(i);
    }

    if queue.next(index)? != '}' {
        return missing_braces!(index);
    }

    Ok(Rune::from_u32(number as u32).unwrap())
}

fn octal<I: Iterator<Item = char>>(
    queue: &mut CharStream<I>,
    start: usize,
    index: &mut usize,
) -> Result<Rune, (Error, Range<usize>)> {
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
                Error::Syntax(SyntaxError::InvalidEscapeCharacters("'0'..'7'".to_string())),
                *index..*index,
            ));
        }

        number += digit * 8u32.pow(i);
    }

    if queue.next(index)? != '}' {
        return missing_braces!(index);
    }

    Rune::from_u32(number).ok_or((
        Error::Syntax(SyntaxError::InvalidEscapeSeq(format!(
            "`\\o{{{:o}}}`",
            number
        ))),
        start..*index,
    ))
}

fn binary<I: Iterator<Item = char>>(
    queue: &mut CharStream<I>,
    index: &mut usize,
) -> Result<Rune, (Error, Range<usize>)> {
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
                Error::Syntax(SyntaxError::InvalidEscapeCharacters("'0'..'1'".to_string())),
                *index..*index,
            ));
        }

        number += digit as u8 * 2u8.pow(i);
    }

    if queue.next(index)? != '}' {
        return missing_braces!(index);
    }

    Ok(Rune::from_u32(number as u32).unwrap())
}

struct CharStream<I: Iterator<Item = char>>(I);

impl<I: Iterator<Item = char>> CharStream<I> {
    pub fn new(chars: I) -> Self {
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

    #[test]
    fn unicode_16bit() {
        assert_eq!(Some(" ".into()), unescape_string(r"\u{0020}".chars()).ok());
        assert_eq!(Some("%".into()), unescape_string(r"\u{0025}".chars()).ok());
        assert_eq!(Some("Z".into()), unescape_string(r"\u{005A}".chars()).ok());
        assert_eq!(Some("}".into()), unescape_string(r"\u{007D}".chars()).ok());
        assert_eq!(Some("®".into()), unescape_string(r"\u{00AE}".chars()).ok());
        assert_eq!(Some("Ø".into()), unescape_string(r"\u{00D8}".chars()).ok());
        assert_eq!(Some("Ɯ".into()), unescape_string(r"\u{019C}".chars()).ok());
        assert_eq!(Some("Ǌ".into()), unescape_string(r"\u{01CA}".chars()).ok());
        assert_eq!(Some("Ω".into()), unescape_string(r"\u{03A9}".chars()).ok());
        assert_eq!(Some("Ӿ".into()), unescape_string(r"\u{04FE}".chars()).ok());
        assert_eq!(Some("▙".into()), unescape_string(r"\u{2599}".chars()).ok());
        assert_eq!(Some("凰".into()), unescape_string(r"\u{51F0}".chars()).ok());
        assert_eq!(None, unescape_string(r"\u2599}".chars()).ok());
        assert_eq!(None, unescape_string(r"\u{t59ertwe}".chars()).ok());
        assert_eq!(None, unescape_string(r"\u{tf}".chars()).ok());
        assert_eq!(None, unescape_string(r"\u{tdftr9}".chars()).ok());
        assert_eq!(None, unescape_string(r"\u{}".chars()).ok());
    }
}
