use colored::Colorize;
use logos::{Lexer, Logos};
use paste::paste;

#[macro_export]
macro_rules! enum_or {

    ($x:pat) => {
        paste!(Self::$x)
    };

    ($x1:pat, $($x2:pat),*) => {
        paste!(Self::$x1) | enum_or!($($x2),*)
    };
}

#[macro_export]
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

#[macro_export]
macro_rules! priority_func {

    ($name:tt -> $typ:ty, $default:expr, $([$($x:pat),*])*) =>
    {
        pub fn $name(&self) -> $typ {
            enum_match!(self, $default, $default + 1, $([$($x),*])*)
        }
    };
}

#[macro_export]
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
                _ => str.push_str(&format!("\\{}", c).to_owned()),
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

fn parse_bool(lex: &mut Lexer<TokenType>) -> bool {
    match lex.slice() {
        "false" => false,
        "true" => true,
        _ => panic!(),
    }
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

    #[token("false", parse_bool)]
    #[token("true", parse_bool)]
    BoolLit(bool),

    #[regex(r"[0-9]+", |lex| lex.slice().parse())]
    #[regex(r"[0-9]+i32", |lex| lex.slice().parse())]
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
    Sub,
    #[token("--")]
    SubSub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("!")]
    Not,
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
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
    #[token("void")]
    Void,


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
        [Assign]
        [Equal, NotEqual, Greater, GreaterEq, Less, LessEq]
        [Add, Sub]
        [Mul, Div]
    );

    pub fn is_op(&self) -> bool {
        self.precedence() != 0
    }
    // assign_func!(is_op -> bool, false,
    //     [true; Add, Sub, Mul, Div, Equal, Greater, GreaterEq, Less, LessEq, Assign]
    // );
}

pub type Position = std::ops::Range<usize>;

#[derive(Debug, Clone)]
pub struct Token {
    pub typ: TokenType,
    pub range: Position,
}

fn merge_errors<I>(mut tokens: I) -> Vec<Token>
where
    I: Iterator<Item = Token>,
{
    let mut vec = Vec::<Token>::new();
    let mut err_range: Option<Position> = None;

    while let Some(tok) = tokens.next() {
        if tok.typ == TokenType::Error {
            err_range = Some({
                if err_range.is_none() {
                    tok.range.start..tok.range.end
                } else {
                    err_range.unwrap().start..tok.range.end
                }
            });
        } else {
            if err_range.is_some() {
                vec.push(Token {
                    typ: TokenType::Error,
                    range: err_range.unwrap(),
                });

                err_range = None;
            }
            vec.push(tok);
        }
    }

    if err_range.is_some() {
        vec.push(Token {
            typ: TokenType::Error,
            range: err_range.unwrap(),
        });
    }

    vec
}

pub fn lex_tokens(code: &str) -> (Vec<Token>, bool) {
    let lex = TokenType::lexer(code);
    let mut err_flag = false;

    let mut tokens: Vec<Token> = merge_errors(lex.spanned().map(|(typ, range)| {
        if TokenType::Error == typ {
            err_flag = true;
        }
        Token { typ, range }
    }));

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
        // println!("{:?}", tok);
        if let TokenType::Eof = tok.typ {
            continue;
        }

        let s = &code[tok.range.clone()];

        let colored_str = match &tok.typ {
            LBrace | RBrace | LParen | RParen => s.white(),

            Const | Return | This | Any => s.magenta(),

            Add | AddAdd | Sub | SubSub | Mul | Div | Equal | Not | NotEqual | Greater | Less
            | GreaterEq | LessEq | Assign => s.blue(),

            Arrow | Dot | Comma | Semicln | Colon | Addr => s.yellow(),

            BoolLit(val) => val.to_string().red(),
            I32Lit(val) => val.to_string().red(),
            F32Lit(val) => val.to_string().red(),
            Void => "void".to_owned().red(),
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
