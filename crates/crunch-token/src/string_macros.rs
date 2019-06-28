#[macro_export]
macro_rules! comment {
    () => {
        "//[^\n]+\n"
    };
}

#[macro_export]
macro_rules! multiline_comment {
    () => {
        r#"/\*[^\*/]+\*/"#
    };
}

#[macro_export]
macro_rules! doc_comment {
    () => {
        "///[^\n]+\n"
    };
}

#[macro_export]
macro_rules! int_literal {
    () => {
        "-?[0-9_]+"
    };
}

#[macro_export]
macro_rules! float_literal {
    () => {
        r#"-?[0-9_]+\.[0-9_]+"#
    };
}

#[macro_export]
macro_rules! vector_literal {
    () => {
        r#"\[[.*]+\]"#
    };
}

#[macro_export]
macro_rules! vector {
    () => {
        r#"\[[(int)|(str)]+\]"#
    };
}

#[macro_export]
macro_rules! identifier {
    () => {
        "[a-zA-Z_]([a-zA-Z0-9_]+)?"
    };
}
