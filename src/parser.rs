use logos::Logos;

#[derive(Logos, Debug, PartialEq, Eq)]
pub enum Token {
    #[end]
    End,
    #[error]
    Error,
    #[token = "+"]
    Plus,
    #[token = "-"]
    Minus,
    #[token = "="]
    Equal,
    #[token = "=="]
    IsEqual,
    #[token = "["]
    LeftBrace,
    #[token = "]"]
    RightBrace,
    #[token = "/"]
    Divide,
    #[token = "*"]
    Star,
    #[token = "loop"]
    Loop,
    #[token = "while"]
    While,
    #[token = "if"]
    If,
    #[token = "else"]
    Else,
    #[token = "fn"]
    Function,
    #[token = "("]
    LeftParen,
    #[token = ")"]
    RightParen,
    #[token = "{"]
    LeftBracket,
    #[token = "}"]
    RightBracket,
    #[regex = "[a-zA-Z_][a-zA-Z0-9_]*"]
    Ident,
    #[regex = "\".*\""]
    String,
    #[token = " "]
    Space,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        println!("Start of lex");
        let mut l = Token::lexer("\"test\" {}()[] fn else if while loop / * = + == -");
        while l.token != Token::End {
            println!("{:?}", l.token);
            l.advance()
        }

        println!("End of lex");
    }
}
