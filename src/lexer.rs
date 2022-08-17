use colored::Colorize;
use logos::{Lexer, Logos};

fn parse_number(lex: &mut Lexer<TokenType>) -> bool {
    let start = lex.span().start;

    if start == 0 {
        return true;
    }

    let prev_char: Option<char> = lex.source().chars().nth(start);

    if let Some(c) = prev_char {
        if c.is_alphabetic() {
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

fn parse_string(lex: &mut Lexer<TokenType>) -> Option<String> {
    let mut chars = lex.remainder().chars();

    let mut str = String::new();
    let mut escape = false;
    let mut len = 0;

    while let Some(c) = chars.next() {
        len += 1;

        if escape {
            match c {
                'n' => str.push_str("\n"),
                't' => str.push_str("\t"),
                _ => str.push_str(&format!("\\{}", c).to_string()),
            }

            escape = false;
            continue;
        }

        match c {
            '\\' => escape = true,
            '"' => break,
            _ => str.push(c),
        }
    }

    lex.bump(len);
    Some(str)
}

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum TokenType {
    EOF,

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

    #[regex(r"[0-9]+", parse_int)]
    IntLiteral(i32),
    #[regex(r"([0-9]+)?(\.[0-9]+)", parse_float)]
    FloatLiteral(f32),
    #[token("\"", parse_string)]
    StringLiteral(String),

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

#[derive(Debug, Clone)]
pub struct Token {
    pub typ: TokenType,
    pub range: std::ops::Range<usize>,
}

pub fn lex_tokens(code: &str) -> Vec<Token> {
    let lex = TokenType::lexer(code);
    let mut tokens: Vec<Token> = lex
        .spanned()
        .map(|tok| Token {
            typ: tok.0,
            range: tok.1,
        })
        .collect();

    tokens.push(Token {
        typ: TokenType::EOF,
        range: (code.len()..code.len()),
    });
    tokens
}

pub fn print_tokens(code: &str, tokens: &Vec<Token>) {
    for tok in tokens {
        let s = &code[tok.range.clone()];

        use TokenType::*;
        let colored_s = match &tok.typ {
            LBrace | RBrace | LParen | RParen => s.white(),

            Const | Return | This | Any => s.magenta(),

            Add | AddAdd | Min | MinMin | Mul | Div | Equal | Greater | Less | GreaterEq
            | LessEq | Assign => s.blue(),

            Arrow | Dot | Comma | Semicln | Colon | Addr => s.yellow(),

            IntLiteral(val) => val.to_string().green(),
            FloatLiteral(val) => val.to_string().green(),
            StringLiteral(val) => format!("\"{}\"", val).green(),

            Var => s.cyan(),

            Comment => s.truecolor(90, 100, 110).italic(),
            NewLine => "\n".clear(),
            Tab => "\t".clear(),
            Space => " ".clear(),
            Error => s.white().on_red(),
            EOF => "".clear(),
        };

        print!("{}", colored_s);
    }
}
