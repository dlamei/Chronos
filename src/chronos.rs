use std::{cell::RefCell, collections::HashMap, fmt, fmt::Debug, mem, rc::Rc};

use crate::datatypes::*;
use crate::errors::*;
use crate::interpreter::*;
use crate::lexer::Lexer;
use crate::parser::Parser;

pub const DIGITS: &str = "0123456789";
pub const LETTERS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

pub type ChInt = i32;
pub type ChFloat = f32;

#[allow(clippy::mem_discriminant_non_enum)]
pub fn match_enum_type<T>(t1: &T, t2: &T) -> bool {
    mem::discriminant(t1) == mem::discriminant(t2)
}

//TODO: remove file_name and text from position!!!
#[derive(Debug, Clone, Default)]
pub struct Position {
    pub file_name: Rc<String>,
    pub index: usize,
    pub line: usize,
    pub column: usize,
    pub text: Rc<String>,
}

impl Position {
    fn new(file_name: String, index: usize, line: usize, column: usize, text: String) -> Self {
        Position {
            file_name: Rc::new(file_name),
            index,
            line,
            column,
            text: Rc::new(text),
        }
    }

    pub fn from_name(file_name: String) -> Self {
        Position {
            file_name: Rc::new(file_name),
            ..Default::default()
        }
    }

    pub fn advance(&mut self, current_char: &Option<char>) {
        match *current_char {
            Some('\n') => {
                self.line += 1;
                self.index += 1;
                self.column = 0;
            }

            _ => {
                self.index += 1;
                self.column += 1;
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Keyword {
    And,
    Or,
    Not,
    If,
    Elif,
    Else,
    While,
    For,
    Func,
}

pub fn get_keyword(s: &str) -> Result<Keyword, ()> {
    match s {
        "&&" => Ok(Keyword::And),
        "||" => Ok(Keyword::Or),
        "!" => Ok(Keyword::Not),
        "if" => Ok(Keyword::If),
        "elif" => Ok(Keyword::Elif),
        "else" => Ok(Keyword::Else),
        "while" => Ok(Keyword::While),
        "for" => Ok(Keyword::For),
        "fn" => Ok(Keyword::Func),
        _ => Err(()),
    }
}

#[derive(Debug, Clone)]
pub enum TokenType {
    Int(ChInt),
    String(String),
    Float(ChFloat),
    Add,
    AddEq,
    Sub,
    SubEq,
    Mul,
    Div,
    Pow,
    LRound,
    RRound,
    LCurly,
    RCurly,
    LBrace,
    RBrace,
    Semicln,
    Comma,
    Eof,

    Id(String),
    Keywrd(Keyword),
    Assign,

    Equal,
    NEqual,
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub start_pos: Position,
    pub end_pos: Position,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.token_type)
    }
}

impl Token {
    pub fn new(token_type: TokenType, start_pos: Position, end_pos: Option<Position>) -> Self {
        if let Some(end_pos) = end_pos {
            return Token {
                token_type,
                start_pos,
                end_pos,
            };
        }

        let mut end_pos = start_pos.clone();
        end_pos.advance(&None);

        Token {
            token_type,
            start_pos,
            end_pos,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Node {
    Num(Token),
    String(Token),
    Array(Vec<Node>, Position, Position),
    BinOp(Box<Node>, Token, Box<Node>),
    UnryOp(Token, Box<Node>),
    Assign(Token, Box<Node>),
    Access(Token),
    If(Vec<(Node, Node)>, Option<Box<Node>>),
    While(Box<Node>, Box<Node>, Position, Position),
    For(
        Option<Box<Node>>,
        Box<Node>,
        Option<Box<Node>>,
        Box<Node>,
        Position,
        Position,
    ),
    FuncDef(Option<Token>, Vec<Token>, Box<Node>, Position, Position),
    Call(Box<Node>, Vec<Node>),
}

impl ConvertType for bool {
    fn into_number_type(self) -> NumberType {
        NumberType::Int(if self { 1 } else { 0 })
    }

    fn get_number_type(&self) -> NumberType {
        NumberType::Int(if *self { 1 } else { 0 })
    }

    fn convert_to_number(self) -> Result<NumberType, Error> {
        Ok(NumberType::Int(if self { 1 } else { 0 }))
    }
}

impl ConvertType for ChInt {
    fn into_number_type(self) -> NumberType {
        NumberType::Int(self)
    }

    fn get_number_type(&self) -> NumberType {
        NumberType::Int(*self)
    }

    fn convert_to_number(self) -> Result<NumberType, Error> {
        Ok(NumberType::Int(self))
    }
}

impl ConvertType for ChFloat {
    fn into_number_type(self) -> NumberType {
        NumberType::Float(self)
    }

    fn get_number_type(&self) -> NumberType {
        NumberType::Float(*self)
    }

    fn convert_to_number(self) -> Result<NumberType, Error> {
        Ok(NumberType::Float(self))
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub display_name: String,
    pub parent: Option<Rc<RefCell<Scope>>>,
    pub position: Option<Position>,
    pub symbol_table: SymbolTable,
}

impl Scope {
    pub fn empty(display_name: String) -> Self {
        Scope {
            display_name,
            parent: None,
            position: None,
            symbol_table: SymbolTable::default(),
        }
    }

    pub fn from_parent(
        display_name: String,
        parent: Rc<RefCell<Scope>>,
        position: Position,
    ) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Scope {
            display_name,
            parent: Some(parent),
            position: Some(position),
            symbol_table: SymbolTable::default(),
        }))
    }

    fn set_pos(&mut self, position: Position) {
        match &mut self.position {
            Some(p) => {
                *p = position;
            }
            _ => (),
        }
    }

    fn count_parents(&self) -> i32 {
        if let Some(p) = &self.parent {
            p.borrow().count_parents() + 1
        } else {
            0
        }
    }

    pub fn get(&self, key: &str) -> Option<ChType> {
        match self.symbol_table.get(key) {
            Some(v) => Some(v),
            None => match &self.parent {
                Some(p) => p.borrow().get(key),
                None => None,
            },
        }
    }

    pub fn set_mut(&mut self, key: &str, value: ChType) -> bool {
        self.symbol_table.set_mut(key, value)
    }

    pub fn set(&mut self, key: &str, value: ChType) -> bool {
        self.symbol_table.set(key, value)
    }
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    table: HashMap<String, ChType>,
    immutable: Vec<String>,
}

impl SymbolTable {
    fn get(&self, key: &str) -> Option<ChType> {
        self.table.get(key).cloned()
    }

    fn set_mut(&mut self, key: &str, value: ChType) -> bool {
        if self.table.contains_key(key) {
            if self.immutable.iter().any(|s| s == key) {
                false
            } else {
                *self.table.get_mut(key).unwrap() = value;
                true
            }
        } else {
            self.table.insert(key.to_string(), value);
            true
        }
    }

    fn set(&mut self, key: &str, value: ChType) -> bool {
        let b = self.set_mut(key, value);
        if b {
            self.immutable.push(key.to_string())
        };
        b
    }

    fn remove(&mut self, key: &str) {
        self.table.remove(key);
    }
}

fn ch_print(args: Vec<ChType>, _name: Option<String>) -> Result<ChType, Error> {
    let ret = ChType::None(ChNone {
        start_pos: Position::default(),
        end_pos: Position::default(),
    });
    if args.is_empty() {
        return Ok(ret);
    }

    let mut it = args.iter();
    let first = it.next();
    print!("{}", first.unwrap());

    for arg in it {
        print!(", {}", arg);
    }
    println!();

    Ok(ret)
}

fn ch_len(args: Vec<ChType>, _name: Option<String>) -> Result<ChType, Error> {
    let mut start = Position::default();
    let mut end = Position::default();

    if args.len() != 1 {
        return Err(Error::new(
            ErrType::Runtime,
            &start,
            &end,
            format!("Expected 1 argument found: {}", args.len()),
            None,
        ));
    }

    let arg = args.first().unwrap();
    start = arg.get_start();
    end = arg.get_end();

    match arg {
        ChType::String(s) => Ok(ChType::Number(ChNumber {
            value: (s.string.len() as i32).get_number_type(),
            start_pos: start,
            end_pos: end,
        })),
        _ => Err(Error::new(
            ErrType::Runtime,
            &start,
            &end,
            format!("{} has no len", arg),
            None,
        )),
    }
}

pub struct Compiler {
    pub global_scope: Rc<RefCell<Scope>>,
}

impl Compiler {
    pub fn new() -> Self {
        let mut table = SymbolTable::default();
        table.set(&String::from("false"), ChType::Bool(ChBool::from(false)));
        table.set(&String::from("true"), ChType::Bool(ChBool::from(true)));
        table.set(
            &String::from("none"),
            ChType::None(ChNone {
                start_pos: Position::default(),
                end_pos: Position::default(),
            }),
        );

        table.set(
            &String::from("print"),
            ChType::Function(Box::new(ChFunction {
                func_type: FuncType::RustFunc(RustFunc {
                    name: "print".to_string(),
                    function: ch_print,
                }),
            })),
        );

        table.set(
            &String::from("len"),
            ChType::Function(Box::new(ChFunction {
                func_type: FuncType::RustFunc(RustFunc {
                    name: "len".to_string(),
                    function: ch_len,
                }),
            })),
        );

        Compiler {
            global_scope: Rc::new(RefCell::new(Scope {
                display_name: "<module>".to_string(),
                parent: None,
                position: None,
                symbol_table: table,
            })),
        }
    }

    pub fn interpret(&mut self, file_name: String, text: String) -> Result<ChType, Error> {
        let mut lexer = Lexer::new(file_name, text);
        let tokens = lexer.parse_tokens()?;
        let mut parser = Parser::new(tokens);
        let mut ast = parser.parse()?;

        visit_node(&mut ast, &mut self.global_scope)
    }
}
