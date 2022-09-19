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

fn parse_lit<T>(lex: &mut Lexer<TokenType>, lit: &str) -> Option<T>
where
    T: std::str::FromStr,
{
    let mut num = lex.slice();

    if let Some(end) = num.find(lit) {
        num = &num[..end];
        if let Ok(res) = num.parse() {
            Some(res)
        } else {
            None
        }
    } else {
        None
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
    Ref,

    #[token("false", parse_bool)]
    #[token("true", parse_bool)]
    BoolLit(bool),

    #[regex(r"[0-9]+i8", |lex| parse_lit::<i8>(lex, "i8"))]
    I8Lit(i8),
    #[regex(r"[0-9]+u8", |lex| parse_lit::<u8>(lex, "u8"))]
    U8Lit(u8),

    #[regex(r"[0-9]+i16", |lex| parse_lit::<i16>(lex, "i16"))]
    I16Lit(i16),
    #[regex(r"[0-9]+u16", |lex| parse_lit::<u16>(lex, "u16"))]
    U16Lit(u16),

    #[regex(r"[0-9]+", |lex| lex.slice().parse())]
    #[regex(r"[0-9]+i32", |lex| parse_lit::<i32>(lex, "i32"))]
    I32Lit(i32),
    #[regex(r"[0-9]+u32", |lex| parse_lit::<u32>(lex, "u32"))]
    U32Lit(u32),

    #[regex(r"[0-9]+i64", |lex| parse_lit::<i64>(lex, "i64"))]
    I64Lit(i64),
    #[regex(r"[0-9]+u64", |lex| parse_lit::<u64>(lex, "u64"))]
    U64Lit(u64),

    #[regex(r"[0-9]+isize", |lex| parse_lit::<isize>(lex, "isize"))]
    ISizeLit(isize),
    #[regex(r"[0-9]+usize", |lex| parse_lit::<usize>(lex, "usize"))]
    USizeLit(usize),

    #[regex(r"[0-9]+i128", |lex| parse_lit::<i128>(lex, "i128"))]
    I128Lit(i128),
    #[regex(r"[0-9]+u128", |lex| parse_lit::<u128>(lex, "u128"))]
    U128Lit(u128),

    #[regex(r"(([0-9]+)(\.[0-9]+))", |lex| lex.slice().parse())]
    #[regex(r"(([0-9]+)(\.[0-9]+))f32", |lex| parse_lit::<f32>(lex, "f32"))]
    F32Lit(f32),

    #[regex(r"(([0-9]+)(\.[0-9]+))f64", |lex| parse_lit::<f64>(lex, "f64"))]
    F64Lit(f64),

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

    #[token("+=")]
    AddEq,
    #[token("-=")]
    SubEq,
    #[token("*=")]
    MulEq,
    #[token("/=")]
    DivEq,

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
        [Assign, AddEq, SubEq, MulEq, DivEq]
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

fn merge_errors<I>(tokens: I) -> Vec<Token>
where
    I: Iterator<Item = Token>,
{
    let mut vec = Vec::<Token>::new();
    let mut err_range: Option<Position> = None;

    for tok in tokens {
        if tok.typ == TokenType::Error {
            err_range = Some({
                if let Some(range) = err_range {
                    range.start..tok.range.end
                } else {
                    tok.range.start..tok.range.end
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

    if let Some(range) = err_range {
        vec.push(Token {
            typ: TokenType::Error,
            range,
        });
    }

    vec
}

fn trim_newline(s: &mut String) {
    if s.ends_with('\n') {
        s.pop();
        if s.ends_with('\r') {
            s.pop();
        }
    }
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

            Add | AddAdd | Sub | SubSub | Mul | Div | AddEq | SubEq | MulEq | DivEq | Equal
            | Not | NotEqual | Greater | Less | GreaterEq | LessEq | Assign => s.blue(),

            Arrow | Dot | Comma | Semicln | Colon | Ref => s.yellow(),

            BoolLit(val) => val.to_string().red(),

            I8Lit(val) => val.to_string().red(),
            I16Lit(val) => val.to_string().red(),
            I32Lit(val) => val.to_string().red(),
            I64Lit(val) => val.to_string().red(),
            ISizeLit(val) => val.to_string().red(),
            I128Lit(val) => val.to_string().red(),
            U8Lit(val) => val.to_string().red(),
            U16Lit(val) => val.to_string().red(),
            U32Lit(val) => val.to_string().red(),
            U64Lit(val) => val.to_string().red(),
            USizeLit(val) => val.to_string().red(),
            U128Lit(val) => val.to_string().red(),

            F32Lit(val) => val.to_string().red(),
            F64Lit(val) => val.to_string().red(),

            StringLit(val) => format!("\"{}\"", val).green(),

            Void => "void".to_owned().red(),

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

    println!("lexer: {}", res);
}

#[test]
fn lex_lit() {
    use TokenType::*;
    assert_eq!(lex_tokens("23i8").0[0].typ, I8Lit(23));
    assert_eq!(lex_tokens("2i16").0[0].typ, I16Lit(2));
    assert_eq!(lex_tokens("123i32").0[0].typ, I32Lit(123));
    assert_eq!(lex_tokens("321i64").0[0].typ, I64Lit(321));
    assert_eq!(lex_tokens("321isize").0[0].typ, ISizeLit(321));
    assert_eq!(lex_tokens("128i128").0[0].typ, I128Lit(128));

    assert_eq!(lex_tokens("23u8").0[0].typ, U8Lit(23));
    assert_eq!(lex_tokens("2u16").0[0].typ, U16Lit(2));
    assert_eq!(lex_tokens("123u32").0[0].typ, U32Lit(123));
    assert_eq!(lex_tokens("321u64").0[0].typ, U64Lit(321));
    assert_eq!(lex_tokens("321usize").0[0].typ, USizeLit(321));
    assert_eq!(lex_tokens("128u128").0[0].typ, U128Lit(128));
    // assert_eq!(lex_tokens("234234i8").0[0].typ, I8Lit(123));
}
