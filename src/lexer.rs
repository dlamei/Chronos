use colored::Colorize;
use logos::{Lexer, Logos};
use paste::paste;

macro_rules! enum_or {

    ($x:pat) => {
        paste!(Self::$x)
    };

    ($x1:pat, $($x2:pat),*) => {
        paste!(Self::$x1) | enum_or!($($x2),*)
    };
}

macro_rules! enum_match {

    ($self:ident, $default:expr, $n:expr,) => {$default};

    ($self:ident, $default:expr, $n:expr, [$($x1:pat),*] $([$($x2:pat),*])*) => {

        if let enum_or!($($x1),*) = $self {
            $n
        } else {
                enum_match!($self, $default, $n+1, $([$($x2),*])*)
            }
    };
}

macro_rules! priority_func {

    ($name:tt -> $typ:ty, $default:expr, $([$($x:pat),*])*) =>
    {
        pub fn $name(&self) -> $typ {
            enum_match!(self, $default, $default + 1, $([$($x),*])*)
        }
    };
}

macro_rules! assign_func {

    ($name: tt -> $typ:ty, $default: expr, $([$val:expr; $($x2:pat),*])*) => {
        pub fn $name(&self) -> $typ {
            #[allow(unreachable_patterns)]
            match self {
                $($(paste!(Self::$x2))|* => $val,)*
                    _ => $default,
            }
        }
    };
}

fn parse_string(lex: &mut Lexer<TokenType>) -> Option<String> {
    let chars = lex.remainder().chars();

    let mut str = String::new();
    let mut escape = false;
    let mut len = 0;

    for c in chars {
        len += 1;

        if escape {
            match c {
                'n' => str.push('\n'),
                't' => str.push('\t'),
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
    Eof,

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

    #[regex(r"[0-9]+", |lex| lex.slice().parse())]
    I32Lit(i32),
    #[regex(r"(([0-9]+)(\.[0-9]+))", |lex| lex.slice().parse())]
    F32Lit(f32),
    #[token("\"", parse_string)]
    StringLit(String),

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

    #[regex("[_a-zA-Z][_0-9a-zA-Z]*", |lex| lex.slice().parse())]
    Id(String),

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

    #[error(|lex| lex.slice().parse())]
    Error,
}

impl TokenType {
    priority_func!(precedence -> i32, 0,
        [I32Lit(_), F32Lit(_), StringLit(_)]
        [Equal, Greater, GreaterEq, Less, LessEq]
        [Add, Min]
        [Mul, Div]
    );

    assign_func!(is_op -> bool, false,
        [true; Add, Min, Mul, Div, Equal, Greater, GreaterEq, Less, LessEq]
    );
}

pub type Position = std::ops::Range<usize>;

#[derive(Debug, Clone)]
pub struct Token {
    pub typ: TokenType,
    pub range: Position,
}

pub fn lex_tokens(code: &str) -> (Vec<Token>, bool) {
    let lex = TokenType::lexer(code);
    let mut err_flag = false;

    let mut tokens: Vec<Token> = lex
        .spanned()
        .map(|(typ, range)| {
            if TokenType::Error == typ {
                err_flag = true;
            }
            Token { typ, range }
        })
        .collect();

    tokens.push(Token {
        typ: TokenType::Eof,
        range: (code.len()..code.len() + 1),
    });

    (tokens, err_flag)
}

pub fn filter_tokens(tokens: &mut Vec<Token>) {
    use TokenType::*;
    tokens.retain(|tok| !matches!(tok.typ, Tab | Space | Comment));
}

pub fn print_tokens(code: &str, tokens: &Vec<Token>) {
    let mut res = String::new();
    use TokenType::*;

    for tok in tokens {
        if let TokenType::Eof = tok.typ {
            continue;
        }

        let s = &code[tok.range.clone()];

        let colored_str = match &tok.typ {
            LBrace | RBrace | LParen | RParen => s.white(),

            Const | Return | This | Any => s.magenta(),

            Add | AddAdd | Min | MinMin | Mul | Div | Equal | Greater | Less | GreaterEq
            | LessEq | Assign => s.blue(),

            Arrow | Dot | Comma | Semicln | Colon | Addr => s.yellow(),

            I32Lit(val) => val.to_string().green(),
            F32Lit(val) => val.to_string().green(),
            StringLit(val) => format!("\"{}\"", val).green(),

            Id(val) => val.to_string().cyan(),

            Comment => s.truecolor(90, 100, 110).italic(),
            NewLine => "\n".clear(),
            Tab => "\t".clear(),
            Space => " ".clear(),
            Error => s.white().on_red(),
            Eof => "".clear(),
        };

        res.push_str(&colored_str.to_string());
    }

    println!("{}", res);
}
