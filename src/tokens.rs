use colored::Colorize;
use logos::{Lexer, Logos};

fn parse_number(lex: &mut Lexer<TokenType>) -> bool {
    let start = lex.span().start;

    if start == 0 {
        return true;
    }

    let prev_char: Option<&str> = lex.source().get(start - 1..start);

    if let Some(s) = prev_char {
        let char: char = s.chars().reduce(|c1, _| c1).unwrap();
        if char.is_alphabetic() {
            return false;
        }
    }

    true
}

fn parse_int(lex: &mut Lexer<TokenType>) -> Option<i32> {
    if parse_number(lex) {
        lex.slice().parse().ok()
    } else {
        None
    }
}

fn parse_float(lex: &mut Lexer<TokenType>) -> Option<f32> {
    if parse_number(lex) {
        lex.slice().parse().ok()
    } else {
        None
    }
}

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum TokenType {
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,

    #[token("->")]
    Arrow,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicln,
    #[token(":")]
    Colon,
    #[token("&")]
    Addr,

    // #[regex(r"[0-9]+(\.[0-9]*)?", parse_number)]
    // Number(i32),
    #[regex(r"[0-9]+", parse_int)]
    IntLiteral(i32),

    #[regex(r"([0-9]+)?(\.[0-9]+)", parse_float)]
    FloatLiteral(f32),

    #[token("+")]
    Add,
    #[token("++")]
    AddAdd,
    #[token("-")]
    Min,
    #[token("--")]
    MinMin,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("==")]
    Equal,
    #[token(">")]
    Greater,
    #[token("<")]
    Less,
    #[token(">=")]
    GreaterEq,
    #[token("<=")]
    LessEq,
    #[token("=")]
    Assign,

    #[regex("[a-zA-Z_][a-zA-Z_0-9]*")]
    Var,

    #[token("const")]
    Const,
    #[token("return")]
    Return,
    #[token("this")]
    This,
    #[token("...")]
    Any,

    #[regex("//.*")]
    Comment,
    #[regex(r"\n")]
    NewLine,

    #[regex("[ ]+")]
    Space,
    #[regex(r"\t")]
    Tab,

    #[error]
    Error,
}

pub type Token = (TokenType, std::ops::Range<usize>);

pub fn lex_tokens(code: &str) -> Vec<Token> {
    let lex = TokenType::lexer(code);
    let tokens: Vec<_> = lex.spanned().collect();
    tokens
}

pub fn print_tokens(code: &str, tokens: &Vec<Token>) {
    for (token, range) in tokens {
        let s = &code[range.clone()];

        use TokenType::*;
        let colored_s = match token {
            LBrace | RBrace | LParen | RParen => s.white(),

            Const | Return | This | Any => s.magenta(),

            Add | AddAdd | Min | MinMin | Mul | Div | Equal | Greater | Less | GreaterEq
            | LessEq | Assign => s.blue(),

            Arrow | Dot | Comma | Semicln | Colon | Addr => s.yellow(),

            IntLiteral(val) => val.to_string().green(),
            FloatLiteral(val) => val.to_string().green(),

            Var => s.cyan(),

            Comment => s.truecolor(90, 100, 110).italic(),

            NewLine => "\n".clear(),
            Tab => "\t".clear(),
            Space => " ".clear(),

            Error => s.white().on_red(),
        };

        print!("{}", colored_s);
    }
}
