use crunch_error::parse_prelude::Diagnostic;

use alloc::{format, string::String, vec, vec::IntoIter, vec::Vec};
use core::char;

// TODO: *Having* errors is a good start, but make them good ones
// TODO: Verify that these all work and are actually recognized by the parser/lexer and supported by
//       the backends
// TODO: Add all escape sequences to the grammar spec or somewhere where it can be recorded
// TODO: Test all this
// TODO: Decide if required `{ .. }` delimiters is where we want to actually go with this, some could be ok with
//       only a leading `\` and their escape sequence starter (e.g. `\xFF`)
// TODO: The bit escape is probably wholly unneeded, but octal escapes are needed

#[inline]
pub(super) fn unescape_string(
    queue: &mut IntoIter<char>,
) -> Result<String, Vec<Diagnostic<usize>>> {
    let mut queue = CharStream::new(queue);
    let mut s = String::new();

    while let Ok(c) = queue.next() {
        if c != '\\' {
            s.push(c);
            continue;
        }

        match queue.next()? {
            'n' => s.push('\n'),
            'r' => s.push('\r'),
            't' => s.push('\t'),
            '\'' => s.push('\''),
            '"' => s.push('"'),
            '\\' => s.push('\\'),
            '0' => s.push('\0'),
            'u' => s.push(unescape_unicode(&mut queue)?),
            'x' => s.push(unescape_byte(&mut queue)?),
            'b' => s.push(unescape_bits(&mut queue)?),

            c => {
                return Err(vec![Diagnostic::error()
                    .with_message(format!("Unrecognized escape sequence: `\\{}`", c))]);
            }
        };
    }

    Ok(s)
}

#[inline]
fn unescape_unicode<'a>(queue: &mut CharStream<'a>) -> Result<char, Vec<Diagnostic<usize>>> {
    let mut s = String::with_capacity(4);

    if queue.next()? != '{' {
        return Err(vec![
            Diagnostic::error().with_message("Escape sequences start with a `{`")
        ]);
    }

    for _ in 0..4 {
        s.push(queue.next()?);
    }

    if queue.next()? != '}' {
        return Err(vec![
            Diagnostic::error().with_message("Escape sequences end with a `}`")
        ]);
    }

    let u = u32::from_str_radix(&s, 16).map_err(|_| {
        vec![Diagnostic::error().with_message(format!("Invalid unicode escape: `\\u{{{}}}`", s))]
    })?;

    char::from_u32(u).ok_or(vec![
        Diagnostic::error().with_message(format!("Invalid unicode escape: `\\u{{{:X}}}`", u))
    ])
}

#[inline]
fn unescape_byte<'a>(queue: &mut CharStream<'a>) -> Result<char, Vec<Diagnostic<usize>>> {
    let mut s = String::with_capacity(2);

    if queue.next()? != '{' {
        return Err(vec![
            Diagnostic::error().with_message("Escape sequences start with a `{{`")
        ]);
    }

    for _ in 0..2 {
        s.push(queue.next()?);
    }

    if queue.next()? != '}' {
        return Err(vec![
            Diagnostic::error().with_message("Escape sequences end with a `}`")
        ]);
    }

    u8::from_str_radix(&s, 16).map(|b| b as char).map_err(|_| {
        vec![Diagnostic::error().with_message(format!("Invalid byte escape: `\\x{{{}}}", s))]
    })
}

#[inline]
fn unescape_bits<'a>(queue: &mut CharStream<'a>) -> Result<char, Vec<Diagnostic<usize>>> {
    let mut s = String::with_capacity(8);

    if queue.next()? != '{' {
        return Err(vec![
            Diagnostic::error().with_message("Escape sequences start with a `{{`")
        ]);
    }

    for _ in 0..8 {
        s.push(queue.next()?);
    }

    if queue.next()? != '}' {
        return Err(vec![
            Diagnostic::error().with_message("Escape sequences end with a `}`")
        ]);
    }

    u8::from_str_radix(&s, 2).map(|b| b as char).map_err(|_| {
        vec![Diagnostic::error().with_message(format!("Invalid bit escape: `\\b{{{}}}`", s))]
    })
}

struct CharStream<'a>(&'a mut IntoIter<char>);

impl<'a> CharStream<'a> {
    pub fn new(chars: &'a mut IntoIter<char>) -> Self {
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
