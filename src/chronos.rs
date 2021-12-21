use std::{
    cell::RefCell,
    collections::HashMap,
    fmt,
    fmt::{Debug, Display},
    mem,
    rc::Rc,
};

use crate::datatypes::*;
use crate::errors::*;
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

#[derive(Clone, Debug)]
pub enum NumberType {
    Int(ChInt),
    Float(ChFloat),
}



pub trait IsChValue: Display + HasPosition + HasScope + ChOperators + ConvertType {
    fn get_desc(&self) -> String;
    fn into_type(self) -> ChType;
}

impl ConvertType for ChType {
    fn into_number_type(self) -> NumberType {
        match self {
            ChType::Number(n) => n.into_number_type(),
            ChType::String(s) => s.into_number_type(),
            ChType::Bool(b) => b.into_number_type(),
            ChType::Function(f) => f.into_number_type(),
            ChType::None(_) => 0.into_number_type(),
        }
    }

    fn convert_to_number(self) -> Result<NumberType, Error> {
        match self {
            ChType::Number(n) => n.convert_to_number(),
            ChType::String(s) => s.convert_to_number(),
            ChType::Bool(b) => b.convert_to_number(),
            ChType::Function(f) => f.convert_to_number(),
            ChType::None(_) => Err(Error::new(
                ErrType::Runtime,
                &self.get_start(),
                &self.get_end(),
                format!("Could not convert '{}' to Number", self),
                None,
            )),
        }
    }

    fn get_number_type(&self) -> NumberType {
        match self {
            ChType::Number(n) => n.get_number_type(),
            ChType::Bool(b) => b.get_number_type(),
            ChType::String(b) => b.get_number_type(),
            ChType::Function(_) | ChType::None(_) => {
                panic!("could not get value type of type {}", self)
            }
        }
    }
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

//#[allow(clippy::large_enum_variant)]

#[derive(Debug, Clone)]
pub enum ChType {
    Number(ChNumber),
    String(ChString),
    Function(Box<ChFunction>),
    Bool(ChBool),
    None(ChNone),
}

impl HasScope for ChType {
    fn set_scope(&mut self, scope: Rc<RefCell<Scope>>) {
        match self {
            ChType::Function(func) => func.set_scope(scope),
            _ => {}
        }
    }
}

impl Display for ChType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ChType::Number(n) => write!(f, "{}", n),
            ChType::String(n) => write!(f, "{}", n),
            ChType::None(n) => write!(f, "{}", n),
            ChType::Bool(b) => write!(f, "{}", b),
            ChType::Function(func) => write!(f, "{}", func),
        }
    }
}

impl HasPosition for ChType {
    fn get_start(&self) -> Position {
        match self {
            ChType::Number(num) => num.start_pos.clone(),
            ChType::String(t) => t.start_pos.clone(),
            ChType::Bool(b) => b.start_pos.clone(),
            ChType::None(none) => none.start_pos.clone(),
            ChType::Function(f) => f.get_start(),
        }
    }

    fn get_end(&self) -> Position {
        match self {
            ChType::Number(num) => num.end_pos.clone(),
            ChType::String(t) => t.end_pos.clone(),
            ChType::Bool(b) => b.end_pos.clone(),
            ChType::Function(f) => f.get_end(),
            ChType::None(none) => none.end_pos.clone(),
        }
    }

    fn set_position(&mut self, start_pos: Position, end_pos: Position) {
        match self {
            ChType::Number(num) => num.set_position(start_pos, end_pos),
            ChType::Bool(b) => b.set_position(start_pos, end_pos),
            ChType::Function(f) => f.set_position(start_pos, end_pos),
            ChType::None(none) => none.set_position(start_pos, end_pos),
            ChType::String(t) => t.set_position(start_pos, end_pos),
        }
    }
}

impl ChType {
    pub fn is_true(&self) -> bool {
        match self {
            ChType::Number(n) => n.is_true(),
            ChType::None(n) => n.is_true(),
            ChType::Bool(n) => n.is_true(),
            ChType::Function(f) => f.is_true(),
            ChType::String(f) => f.is_true(),
        }
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

pub fn visit_node(node: &mut Node, scope: &mut Rc<RefCell<Scope>>) -> Result<ChType, Error> {
    match node {
        Node::Num(token) => visit_numb_node(token, scope),
        Node::String(token) => visit_string_node(token, scope),
        Node::UnryOp(op, node) => visit_unryop_node(op, node, scope),
        Node::BinOp(left, op, right) => visit_binop_node(left, op, right, scope),
        Node::Access(id) => visit_access_node(id, scope),
        Node::Assign(id, value) => visit_assign_node(id, value, scope),
        Node::If(cases, else_case) => visit_if_node(cases, else_case, scope),
        Node::While(cond, body, start, end) => visit_while_node(cond, body, scope, start, end),
        Node::For(c1, c2, c3, body, start, end) => {
            visit_for_node(c1, c2, c3, body, scope, start, end)
        }
        Node::FuncDef(name, args, body, start, end) => {
            visit_funcdef_node(name, args, body, start, end, scope)
        }
        Node::Call(name, args) => visit_call_node(name, args, scope),
    }
}

fn visit_numb_node(token: &mut Token, _scope: &mut Rc<RefCell<Scope>>) -> Result<ChType, Error> {
    match token.token_type {
        TokenType::Int(value) => Ok(ChType::Number(ChNumber {
            value: value.into_number_type(),
            start_pos: token.start_pos.clone(),
            end_pos: token.end_pos.clone(),
        })),
        TokenType::Float(value) => Ok(ChType::Number(ChNumber {
            value: value.into_number_type(),
            start_pos: token.start_pos.clone(),
            end_pos: token.end_pos.clone(),
        })),
        _ => panic!("called visit_numb_node on a number node that has a non number token"),
    }
}

fn visit_string_node(token: &mut Token, _scope: &mut Rc<RefCell<Scope>>) -> Result<ChType, Error> {
    match &token.token_type {
        TokenType::String(s) => Ok(ChType::String(ChString {
            string: s.to_string(),
            start_pos: token.start_pos.clone(),
            end_pos: token.end_pos.clone(),
        })),
        _ => panic!("called visit_string_node on a string node that has a non string token"),
    }
}

fn visit_access_node(token: &mut Token, scope: &mut Rc<RefCell<Scope>>) -> Result<ChType, Error> {
    let var = &token.token_type;
    match var {
        TokenType::Id(var_name) => {
            let mut entry = scope.borrow().get(var_name);

            match &mut entry {
                Some(num) => {
                    num.set_position(token.start_pos.clone(), token.end_pos.clone());
                    num.set_scope(scope.clone());
                    Ok(num.clone())
                }
                None => Err(Error::new(
                    ErrType::Runtime,
                    &token.start_pos,
                    &token.end_pos,
                    format!("{:?} is not defined", var_name),
                    Some(scope.clone()),
                )),
            }
        }
        _ => panic!("called visit_access_node on a non ID token"),
    }
}

fn visit_assign_node(
    id: &mut Token,
    value: &mut Node,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChType, Error> {
    let t = id.clone();
    let ch_type = visit_node(value, scope)?;

    match t.token_type {
        TokenType::Id(var_name) => {
            if !scope.borrow_mut().set_mut(&var_name, ch_type.clone()) {
                return Err(Error::new(
                    ErrType::Runtime,
                    &ch_type.get_start(),
                    &ch_type.get_end(),
                    format!("cannot assign {} to const {:?}", ch_type, var_name),
                    Some(scope.clone()),
                ));
            }
            Ok(ch_type)
        }
        _ => panic!("called visit_assign_node on {:?}", value),
    }
}

fn unryop_chvalue<T: IsChValue>(op_token: &Token, value: T) -> Result<ChType, Error> {
    match op_token.token_type {
        TokenType::Sub => value.negate(),
        TokenType::Keywrd(Keyword::Not) => value.not(),
        _ => panic!("called unryop_self on {:?}", op_token),
    }
}

fn visit_unryop_node(
    op: &mut Token,
    node: &mut Node,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChType, Error> {
    let mut ch_type = visit_node(node, scope)?;
    ch_type.set_position(op.start_pos.clone(), ch_type.get_end());

    match ch_type {
        ChType::Number(n) => unryop_chvalue(op, n),
        ChType::String(s) => unryop_chvalue(op, s),
        ChType::Bool(n) => unryop_chvalue(op, n),
        ChType::Function(n) => unryop_chvalue(op, *n),
        ChType::None(n) => unryop_chvalue(op, n),
    }
}

fn binop_chvalue<T: IsChValue>(left: T, op_token: &Token, right: ChType) -> Result<ChType, Error> {
    match op_token.token_type {
        TokenType::Add => left.add(right),
        TokenType::Sub => left.sub(right),
        TokenType::Mul => left.mult(right),
        TokenType::Div => left.div(right),
        TokenType::Pow => left.pow(right),
        TokenType::Less => left.less(right),
        TokenType::Equal => left.equal(right),
        TokenType::NEqual => left.not_equal(right),
        TokenType::LessEq => left.less_equal(right),
        TokenType::Greater => left.greater(right),
        TokenType::GreaterEq => left.greater_equal(right),
        TokenType::Keywrd(Keyword::And) => left.and(right),
        TokenType::Keywrd(Keyword::Or) => left.or(right),
        _ => panic!("called binop_bool on {:?}", op_token.token_type),
    }
}

fn visit_binop_node(
    left: &mut Node,
    op: &mut Token,
    right: &mut Node,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChType, Error> {
    if matches!(op.token_type, TokenType::AddEq) || matches!(op.token_type, TokenType::SubEq) {
        return in_de_crement(left, op, right, scope);
    }
    let mut left = visit_node(left, scope)?;
    let right = visit_node(right, scope)?;

    left.set_position(left.get_start(), right.get_end());

    match match left {
        ChType::Number(n) => binop_chvalue(n, op, right),
        ChType::String(s) => binop_chvalue(s, op, right),
        ChType::None(n) => binop_chvalue(n, op, right),
        ChType::Function(n) => binop_chvalue(*n, op, right),
        ChType::Bool(n) => binop_chvalue(n, op, right),
    } {
        Ok(res) => Ok(res),
        Err(mut e) => {
            e.set_scope(scope.clone());
            Err(e)
        }
    }
}

fn in_de_crement(
    left_node: &mut Node,
    op: &mut Token,
    right_node: &mut Node,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChType, Error> {
    let mut left = visit_node(left_node, scope)?;
    let right = visit_node(right_node, scope)?;
    let start = left.get_start();
    let end = left.get_end();

    match left_node {
        Node::Access(var_name) => {
            left.set_position(left.get_start(), right.get_end());

            match op.token_type {
                TokenType::AddEq => {
                    let res = match left {
                        ChType::Number(n) => n.add_equal(right),
                        ChType::String(s) => s.add_equal(right),
                        ChType::None(n) => n.add_equal(right),
                        ChType::Function(func) => func.add_equal(right),
                        ChType::Bool(b) => b.add_equal(right),
                    }?;

                    let name = match &var_name.token_type {
                        TokenType::Id(n) => n,
                        _ => panic!("could not resolve name"),
                    };

                    scope.borrow_mut().set_mut(name, res.clone());
                    Ok(res)
                }
                TokenType::SubEq => {
                    let res = match left {
                        ChType::Number(n) => n.sub_equal(right),
                        ChType::String(s) => s.sub_equal(right),
                        ChType::None(n) => n.sub_equal(right),
                        ChType::Function(func) => func.sub_equal(right),
                        ChType::Bool(b) => b.sub_equal(right),
                    }?;

                    let name = match &var_name.token_type {
                        TokenType::Id(n) => n,
                        _ => panic!("could not resolve name"),
                    };

                    scope.borrow_mut().set_mut(name, res.clone());
                    Ok(res)
                }
                _ => panic!("called in/decrement on {:?}", op),
            }
        }
        _ => Err(Error::new(
            ErrType::Runtime,
            &start,
            &end,
            format!("expected LVALUE, found {:?}", left_node),
            Some(scope.clone()),
        )),
    }
}

fn visit_if_node(
    cases: &mut Vec<(Node, Node)>,
    else_case: &mut Option<Box<Node>>,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChType, Error> {
    let mut start = Position::default();
    let mut end = Position::default();
    let mut first_cond = true;

    for (condition, expr) in cases {
        let cond = visit_node(condition, scope)?;

        if first_cond {
            start = cond.get_start();
            end = cond.get_end();
            first_cond = false;
        }

        if cond.is_true() {
            return visit_node(expr, scope);
        }
    }

    match else_case {
        Some(node) => visit_node(node, scope),
        _ => Ok(ChType::None(ChNone {
            start_pos: start,
            end_pos: end,
        })),
    }
}

fn visit_for_node(
    c1: &mut Option<Box<Node>>,
    c2: &mut Box<Node>,
    c3: &mut Option<Box<Node>>,
    body: &mut Node,
    scope: &mut Rc<RefCell<Scope>>,
    start: &mut Position,
    end: &mut Position,
) -> Result<ChType, Error> {
    let mut n_scope = Scope::from_parent(String::from("<for>"), scope.clone(), start.clone());

    if let Some(c) = c1 {
        visit_node(c, &mut n_scope)?;
    }

    while visit_node(c2, &mut n_scope)?.is_true() {
        visit_node(body, &mut n_scope)?;
        if let Some(c) = c3 {
            visit_node(c, &mut n_scope)?;
        }
    }

    Ok(ChType::None(ChNone {
        start_pos: start.clone(),
        end_pos: end.clone(),
    }))
}

fn visit_while_node(
    condition: &mut Node,
    body: &mut Node,
    scope: &mut Rc<RefCell<Scope>>,
    start: &mut Position,
    end: &mut Position,
) -> Result<ChType, Error> {
    let mut n_scope = Scope::from_parent(String::from("<while>"), scope.clone(), start.clone());

    while visit_node(condition, scope)?.is_true() {
        visit_node(body, &mut n_scope)?;
    }

    Ok(ChType::None(ChNone {
        start_pos: start.clone(),
        end_pos: end.clone(),
    }))
}

fn visit_funcdef_node(
    func_name: &mut Option<Token>,
    args: &mut Vec<Token>,
    body: &mut Node,
    start: &mut Position,
    end: &mut Position,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChType, Error> {
    let name = match func_name {
        Some(tok) => match &tok.token_type {
            TokenType::Id(s) => s,
            _ => {
                return Err(Error::new(
                    ErrType::InvalidSyntax,
                    start,
                    end,
                    format!("expected ID found '{:?}'", tok),
                    Some(scope.clone()),
                ))
            }
        },
        _ => "lambda",
    }
    .to_string();

    let func = ChType::Function(Box::new(ChFunction {
        func_type: FuncType::ChronFunc(ChronosFunc {
            name: name.clone(),
            args_name: args.clone(),
            body: body.clone(),
            start_pos: start.clone(),
            end_pos: end.clone(),
            scope: scope.clone(),
        }),
    }));

    if func_name.is_some() {
        scope.borrow_mut().set_mut(&name, func.clone());
    }

    Ok(func)
}

fn visit_call_node(
    func_name: &mut Node,
    args: &mut Vec<Node>,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChType, Error> {
    let name = match func_name {
        Node::Access(tok) => {
            if let TokenType::Id(s) = &tok.token_type {
                Some(s.to_string())
            } else {
                None
            }
        }
        _ => None,
    };

    let c = visit_node(func_name, scope)?;

    let mut call = match c {
        ChType::Function(func) => func,
        _ => {
            return Err(Error::new(
                ErrType::Runtime,
                &c.get_start(),
                &c.get_end(),
                format!("expected FUNCTION found {}", c),
                Some(scope.clone()),
            ))
        }
    };

    let mut arg_values: Vec<ChType> = Vec::new();

    for arg in args {
        arg_values.push(visit_node(arg, scope)?);
    }

    call.set_scope(scope.clone());
    call.execute(arg_values, name)
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
