use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_till, take_until, take_while},
    character::{
        complete::{alphanumeric0, char, one_of},
        is_digit,
    },
    combinator::{cut, map},
    error::{context, ParseError},
    multi::separated_list,
    number::complete::be_i32,
    sequence::{preceded, terminated},
    Compare, IResult, InputLength, InputTake, InputTakeAtPosition,
};
use std::collections::HashMap;

const WHITESPACE_CHARS: &'static str = " \n\r\t";
const IDENT_CHARS: &'static str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_";

#[derive(Debug, PartialEq, Eq)]
enum Value {
    Vector(Vec<Value>),
    Boolean(bool),
    Str(String),
    Int(i32),
}

#[derive(Debug, PartialEq, Eq)]
enum ValueType {
    Vector,
    Boolean,
    Str,
    Int,
}

fn function_start<'a>(i: &'a str) -> IResult<&'a str, &'a str> {
    context("function start", tag("fn"))(i)
}

fn w<'a>(i: &'a str) -> IResult<&'a str, &'a str> {
    context(
        "whitespace",
        take_while(move |c| WHITESPACE_CHARS.contains(c)),
    )(i)
}

fn ident<'a>(i: &'a str) -> IResult<&'a str, &'a str> {
    context("ident", take_while(move |c| !IDENT_CHARS.contains(c)))(i)
}

fn parameters<'a>(i: &'a str) -> IResult<&'a str, Vec<(String, ValueType)>> {
    context(
        "parameters",
        preceded(
            char('('),
            cut(terminated(
                separated_list(preceded(w, char(',')), typed_param),
                preceded(w, char(')')),
            )),
        ),
    )(i)
}

fn typed_param<'a>(i: &'a str) -> IResult<&'a str, (String, ValueType)> {
    let (ident, ty) = take_till(move |c| WHITESPACE_CHARS.contains(c))(i)?;
    let (_, ty) = value_type(ty)?;
    let (i, ident) = take_while(move |c| WHITESPACE_CHARS.contains(c))(ident)?;

    Ok((i, (ident.to_owned(), ty)))
}

fn value_type<'a>(i: &'a str) -> IResult<&'a str, ValueType> {
    alt((
        map(tag("int"), |_| ValueType::Int),
        map(tag("str"), |_| ValueType::Str),
        map(tag("vec"), |_| ValueType::Vector),
        map(tag("bool"), |_| ValueType::Boolean),
    ))(i)
}

fn value<'a>(i: &'a str) -> IResult<&'a str, Value> {
    context(
        "value",
        preceded(
            w,
            alt((
                map(boolean, Value::Boolean),
                map(string, |s| Value::Str(String::from(s))),
                map(int, Value::Int),
                map(vector, Value::Vector),
            )),
        ),
    )(i)
}

fn int<'a>(i: &'a str) -> IResult<&'a str, i32> {
    let (i, int) = context("integer", take_while(|c| (c as char).is_digit(10)))(i)?;

    Ok((i, int.parse::<i32>().expect("Failed to convert to integer")))
}

fn boolean<'a>(i: &'a str) -> IResult<&'a str, bool> {
    alt((map(tag("false"), |_| false), map(tag("true"), |_| true)))(i)
}

fn string<'a>(i: &'a str) -> IResult<&'a str, &'a str> {
    context(
        "string",
        preceded(char('\"'), cut(terminated(parse_str, char('\"')))),
    )(i)
}

fn parse_str<'a>(i: &'a str) -> IResult<&'a str, &'a str> {
    escaped(alphanumeric0, '\\', one_of("\"n\\"))(i)
}

fn vector<'a>(i: &'a str) -> IResult<&'a str, Vec<Value>> {
    context(
        "vector",
        preceded(
            char('['),
            cut(terminated(
                separated_list(preceded(w, char(',')), value),
                preceded(w, char(']')),
            )),
        ),
    )(i)
}

fn function<'a>(i: &'a str) -> IResult<&'a str, ()> {
    let (i, _) = function_start(i)?;
    let (i, _) = w(i)?;
    let (i, function_name) = ident(i)?;
    Ok((i, ()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn function_start() {
        assert_eq!(
            Ok((" test() {}", "fn")),
            super::function_start("fn test() {}")
        );
    }

    #[test]
    fn whitespace() {
        assert_eq!(Ok(("test ", "\n \r \t")), w("\n \r \ttest "));
    }

    // #[test]
    // fn raw_vector() {
    //     println!("Raw Vec: {:?}", vector("[1, \"string\", true];\n    test"));
    //     assert_eq!(
    //         Ok((
    //             ";\n    test",
    //             vec![
    //                 Value::Int(1),
    //                 Value::Str(String::from("string")),
    //                 Value::Boolean(true)
    //             ]
    //         )),
    //         vector("[1, \"string\", true];\n    test")
    //     )
    // }
}
