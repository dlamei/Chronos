use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;

use crate::errors::*;

const DIGITS: &str = "0123456789";
const LETTERS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

type ChInt = i32;
type ChUInt = i32;
type ChFloat = f32;

fn match_tokens(t1: &TokenType, t2: &TokenType) -> bool {
    match (t1, t2) {
        (&TokenType::ADD, &TokenType::ADD)
        | (&TokenType::SUB, &TokenType::SUB)
        | (&TokenType::MUL, &TokenType::MUL)
        | (&TokenType::DIV, &TokenType::DIV)
        | (&TokenType::POW, &TokenType::POW)
        | (&TokenType::LPAREN, &TokenType::LPAREN)
        | (&TokenType::RPAREN, &TokenType::RPAREN)
        | (&TokenType::EOF, &TokenType::EOF) => true,
        _ => false,
    }
}

#[derive(Debug, Clone)]
//TODO: remove file_name and text from position!!!
pub struct Position {
    pub file_name: String,
    pub index: usize,
    pub line: usize,
    pub column: usize,
    pub text: String,
}

impl Position {
    fn new(file_name: String, index: usize, line: usize, column: usize, text: String) -> Self {
        Position {
            file_name,
            index,
            line,
            column,
            text,
        }
    }

    fn empty() -> Self {
        Position {
            file_name: String::from(""),
            index: 0,
            line: 0,
            column: 0,
            text: String::from(""),
        }
    }

    fn advance(&mut self, current_char: &Option<char>) {
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
    LET,
}

fn keyword_to_string(k: Keyword) -> String {
    String::from(match k {
        Keyword::LET => "let",
    })
}

fn is_keyword(s: &String) -> bool {
    s.eq(&keyword_to_string(Keyword::LET))
}

fn get_keyword(s: &String) -> Keyword {
    match s.as_ref() {
        "let" => Keyword::LET,
        _ => panic!("undefined keyword: {}", s),
    }
}

#[derive(Debug, Clone)]
pub enum TokenType {
    INT(ChInt),
    FLOAT(ChFloat),
    ADD,
    SUB,
    MUL,
    DIV,
    POW,
    LPAREN,
    RPAREN,
    EOF,

    ID(String),
    KEYWRD(Keyword),
    EQ,
}

#[derive(Clone)]
pub struct Token {
    token_type: TokenType,
    start_pos: Position,
    end_pos: Position,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.token_type)
    }
}

impl Token {
    pub fn new(token_type: TokenType, start_pos: &Position, end_pos: Option<&Position>) -> Self {
        if let Some(end) = end_pos {
            return Token {
                token_type,
                start_pos: start_pos.clone(),
                end_pos: end.clone(),
            };
        }

        let mut end_pos = start_pos.clone();
        end_pos.advance(&None);

        Token {
            token_type,
            start_pos: start_pos.clone(),
            end_pos,
        }
    }
}

struct Lexer {
    text: Box<[u8]>,
    position: Position,
    current_char: Option<char>,
}

impl Lexer {
    pub fn new(file_name: String, text: String) -> Self {
        let mut l = Lexer {
            text: (text.as_bytes().into()),
            position: Position {
                file_name,
                index: 0,
                line: 0,
                column: 0,
                text,
            },
            current_char: None,
        };
        l.current_char = Some(l.text[l.position.index] as char);
        l
    }

    fn advance(&mut self) {
        self.position.advance(&self.current_char);

        self.current_char = if self.position.index < self.text.len() {
            Some(self.text[self.position.index] as char)
        } else {
            None
        };
    }

    fn parse_tokens(&mut self) -> Result<Vec<Token>, Error> {
        let mut tokens: Vec<Token> = Vec::new();

        while self.current_char != None {
            let c = self.current_char.unwrap();

            if " \t\n".contains(c) {
                self.advance();
            } else if match c {
                '+' => {
                    tokens.push(Token::new(TokenType::ADD, &self.position, None));
                    true
                }
                '-' => {
                    tokens.push(Token::new(TokenType::SUB, &self.position, None));
                    true
                }
                '/' => {
                    tokens.push(Token::new(TokenType::DIV, &self.position, None));
                    true
                }
                '*' => {
                    tokens.push(Token::new(TokenType::MUL, &self.position, None));
                    true
                }
                '^' => {
                    tokens.push(Token::new(TokenType::POW, &self.position, None));
                    true
                }
                '(' => {
                    tokens.push(Token::new(TokenType::LPAREN, &self.position, None));
                    true
                }
                ')' => {
                    tokens.push(Token::new(TokenType::RPAREN, &self.position, None));
                    true
                }
                '=' => {
                    tokens.push(Token::new(TokenType::EQ, &self.position, None));
                    true
                }
                _ => false,
            } {
                self.advance();
            } else if LETTERS.contains(c) {
                tokens.push(self.make_identifier());
            } else if DIGITS.contains(c) {
                tokens.push(self.make_number());
            } else {
                let start_pos = self.position.clone();
                self.advance();
                return Err(Error::new(
                    ErrType::IllegalCharError,
                    &start_pos,
                    &self.position,
                    format!("Lexer found '{}'", c),
                    None,
                ));
            }
        }

        tokens.push(Token::new(TokenType::EOF, &self.position, None));
        Ok(tokens)
    }

    fn make_identifier(&mut self) -> Token {
        let mut id = String::from("");
        let pos_start = self.position.clone();

        let allowed = LETTERS.to_owned() + "_";

        while self.current_char != None && allowed.contains(self.current_char.unwrap()) {
            id.push(self.current_char.unwrap());
            self.advance();
        }

        let token_type = if is_keyword(&id) {
            TokenType::KEYWRD(get_keyword(&id))
        } else {
            TokenType::ID(id)
        };
        Token::new(token_type, &pos_start, Some(&self.position))
    }

    //TODO: don't use strings
    fn make_number(&mut self) -> Token {
        let mut num: String = String::new();
        let mut dot_count: u8 = 0;

        let s = DIGITS.to_owned() + ".";

        while self.current_char != None && (s).contains(self.current_char.unwrap()) {
            let c = self.current_char.unwrap();
            if c == '.' {
                if dot_count >= 1 {
                    break;
                }
                dot_count += 1;
                num += ".";
            } else {
                num.push(c);
            }

            self.advance();
        }

        if dot_count == 0 {
            return Token::new(
                TokenType::INT(num.parse::<ChInt>().unwrap()),
                &self.position,
                None,
            );
        }

        Token::new(
            TokenType::FLOAT(num.parse::<ChFloat>().unwrap()),
            &self.position,
            None,
        )
    }
}

struct Parser {
    tokens: Vec<Token>,
    token_index: usize,
    current_token: Token,
}

#[derive(Debug, Clone)]
pub enum Node {
    NUM(Token),
    BINOP(Box<Node>, Token, Box<Node>),
    UNRYOP(Token, Box<Node>),
    ASSIGN(Token, Box<Node>),
    ACCESS(Token),
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let t = tokens[0].clone();
        Parser {
            tokens,
            token_index: 0,
            current_token: t,
        }
    }

    fn parse(&mut self) -> Result<Node, Error> {
        let nodes = self.expression()?;

        match self.current_token.token_type {
            TokenType::EOF => Ok(nodes),

            _ => Err(Error::new(
                ErrType::InvalidSyntaxError,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!(
                    "Parser expected EOF found {:?}",
                    self.current_token.token_type
                ),
                None,
            )),
        }
    }

    fn advance(&mut self) {
        self.token_index += 1;

        if self.token_index < self.tokens.len() {
            self.current_token = self.tokens[self.token_index].clone();
        }
    }

    fn atom(&mut self) -> Result<Node, Error> {
        let t = self.current_token.clone();

        return match t.token_type {
            TokenType::INT(_) | TokenType::FLOAT(_) => {
                self.advance();
                Ok(Node::NUM(t))
            }
            TokenType::ID(_) => {
                self.advance();
                Ok(Node::ACCESS(t))
            }
            TokenType::LPAREN => {
                self.advance();
                let expr = self.expression()?;
                match self.current_token.token_type {
                    TokenType::RPAREN => {
                        self.advance();
                        Ok(expr)
                    }
                    _ => Err(Error::new(
                        ErrType::InvalidSyntaxError,
                        &t.start_pos,
                        &self.current_token.end_pos,
                        format!(
                            "Parser expected ')' found {:?}",
                            self.current_token.token_type
                        ),
                        None,
                    )),
                }
            }
            _ => Err(Error::new(
                ErrType::InvalidSyntaxError,
                &t.start_pos,
                &t.end_pos,
                format!(
                    "Parser expected INT, FLOAT, '+', '-' or '(, found: {:?}",
                    t.token_type
                ),
                None,
            )),
        };
    }

    fn power(&mut self) -> Result<Node, Error> {
        self.binary_operation(
            Parser::atom,
            (TokenType::POW, TokenType::POW),
            Parser::factor,
        )
    }

    fn factor(&mut self) -> Result<Node, Error> {
        let t = self.current_token.clone();

        return match t.token_type {
            TokenType::SUB | TokenType::ADD => {
                self.advance();
                let factor = self.factor()?;
                Ok(Node::UNRYOP(t, factor.into()))
            }
            _ => self.power(),
        };
    }

    fn binary_operation(
        &mut self,
        func_a: fn(parser: &mut Parser) -> Result<Node, Error>,
        ops: (TokenType, TokenType),
        func_b: fn(parser: &mut Parser) -> Result<Node, Error>,
    ) -> Result<Node, Error> {
        let mut left_node = func_a(self)?;

        while match_tokens(&self.current_token.token_type, &ops.0)
            || match_tokens(&self.current_token.token_type, &ops.1)
        {
            let op_token = self.current_token.clone();
            self.advance();
            let right_node = func_b(self)?;

            left_node = Node::BINOP(left_node.into(), op_token, right_node.into());
        }

        Ok(left_node)
    }

    fn term(&mut self) -> Result<Node, Error> {
        self.binary_operation(
            Parser::factor,
            (TokenType::MUL, TokenType::DIV),
            Parser::factor,
        )
    }

    fn expression(&mut self) -> Result<Node, Error> {
        match self.current_token.token_type {
            TokenType::KEYWRD(Keyword::LET) => {
                self.advance();

                match self.current_token.token_type {
                    TokenType::ID(_) => {
                        let var = self.current_token.clone();
                        self.advance();

                        match self.current_token.token_type {
                            TokenType::EQ => {
                                self.advance();
                                Ok(Node::ASSIGN(var, Box::new(self.expression()?)))
                            }
                            _ => Err(Error::new(
                                ErrType::InvalidSyntaxError,
                                &self.current_token.start_pos,
                                &self.current_token.end_pos,
                                format!(
                                    "Expected assignment operator: '=', found: {:?}",
                                    self.current_token
                                ),
                                None,
                            )),
                        }
                    }
                    _ => Err(Error::new(
                        ErrType::InvalidSyntaxError,
                        &self.current_token.start_pos,
                        &self.current_token.end_pos,
                        format!("Expected identifier, found: {:?}", self.current_token),
                        None,
                    )),
                }
            }
            _ => {
                self.binary_operation(Parser::term, (TokenType::ADD, TokenType::SUB), Parser::term)
            }
        }

        //self.binary_operation(Parser::term, (TokenType::ADD, TokenType::SUB), Parser::term)
    }
}

#[derive(Clone, Debug)]
pub enum NumberType {
    INT(ChInt),
    FLOAT(ChFloat),
}

#[derive(Debug, Clone)]
pub struct Number {
    value: NumberType,
    start_pos: Position,
    end_pos: Position,
    context: Option<Context>,
}

pub trait AsNumberType {
    fn as_number_type(self) -> NumberType;
    fn get_value_type(&self) -> NumberType;
}

impl AsNumberType for ChInt {
    fn as_number_type(self) -> NumberType {
        NumberType::INT(self)
    }

    fn get_value_type(&self) -> NumberType {
        NumberType::INT(self.clone())
    }
}

impl AsNumberType for ChFloat {
    fn as_number_type(self) -> NumberType {
        NumberType::FLOAT(self)
    }

    fn get_value_type(&self) -> NumberType {
        NumberType::FLOAT(self.clone())
    }
}

impl AsNumberType for Number {
    fn as_number_type(self) -> NumberType {
        self.value
    }

    fn get_value_type(&self) -> NumberType {
        self.value.clone()
    }
}

impl Number {
    fn set_position(&mut self, start_pos: Position, end_pos: Position) {
        self.start_pos = start_pos;
        self.end_pos = end_pos;
    }

    fn set_context(&mut self, context: Option<Context>) {
        self.context = context;
    }

    fn operate_on<T: AsNumberType>(
        mut self,
        other: T,
        int_op: fn(ChInt, ChInt) -> ChInt,
        float_op: fn(ChFloat, ChFloat) -> ChFloat,
    ) -> Self {
        self.value = match (self.value, other.as_number_type()) {
            (NumberType::INT(v1), NumberType::INT(v2)) => NumberType::INT(int_op(v1, v2)),
            (NumberType::FLOAT(v1), NumberType::INT(v2)) => {
                NumberType::FLOAT(float_op(v1, v2 as ChFloat))
            }
            (NumberType::INT(v1), NumberType::FLOAT(v2)) => {
                NumberType::FLOAT(float_op(v1 as ChFloat, v2))
            }
            (NumberType::FLOAT(v1), NumberType::FLOAT(v2)) => NumberType::FLOAT(float_op(v1, v2)),
        };

        self
    }

    fn add<T: AsNumberType>(self, other: T) -> Self {
        self.operate_on(
            other,
            |v1: ChInt, v2: ChInt| v1 + v2,
            |v1: ChFloat, v2: ChFloat| v1 + v2,
        )
    }

    fn sub<T: AsNumberType>(self, other: T) -> Self {
        self.operate_on(
            other,
            |v1: ChInt, v2: ChInt| v1 - v2,
            |v1: ChFloat, v2: ChFloat| v1 - v2,
        )
    }

    fn mult<T: AsNumberType>(self, other: T) -> Self {
        self.operate_on(
            other,
            |v1: ChInt, v2: ChInt| v1 * v2,
            |v1: ChFloat, v2: ChFloat| v1 * v2,
        )
    }

    fn div<T: AsNumberType>(self, other: T) -> Result<Self, Error> {
        if match other.get_value_type() {
            NumberType::INT(v) => v == 0,
            NumberType::FLOAT(v) => v == 0.0,
        } {
            return Err(Error::new(
                ErrType::RuntimeError,
                &self.start_pos,
                &self.end_pos,
                String::from("Division by 0"),
                self.context.as_ref(),
            ));
        } else {
            Ok(self.operate_on(
                other,
                |v1: ChInt, v2: ChInt| v1 / v2,
                |v1: ChFloat, v2: ChFloat| v1 / v2,
            ))
        }
    }

    fn pow<T: AsNumberType>(mut self, other: T) -> Result<Self, Error> {
        if match other.get_value_type() {
            NumberType::INT(v) => v == 0,
            _ => false,
        } {
            self.value = 0.as_number_type();
            Ok(self)
        } else {
            Ok(self.operate_on(
                other,
                |v1: ChInt, v2: ChInt| v1.pow(v2.try_into().unwrap_or(0)),
                |v1: ChFloat, v2: ChFloat| v1.powf(v2),
            ))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Context {
    pub display_name: String,
    pub parent: Option<(Box<Context>, Position)>,
    pub symbol_table: Rc<RefCell<SymbolTable>>,
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    parent: Option<Rc<SymbolTable>>,
    table: HashMap<String, Number>,
}

impl SymbolTable {
    pub fn empty() -> Self {
        SymbolTable {
            parent: None,
            table: HashMap::new(),
        }
    }

    fn get(&self, key: &String) -> Option<Number> {
        match self.table.get(key) {
            Some(v) => Some(v.clone()),
            None => match &self.parent {
                Some(p) => p.get(key),
                None => None,
            },
        }
    }

    fn set(&mut self, key: &String, value: Number) {
        if self.table.contains_key(key) {
            *self.table.get_mut(key).unwrap() = value;
        } else {
            self.table.insert(key.to_string(), value);
        }
    }

    fn remove(&mut self, key: &String) {
        self.table.remove(key);
    }
}

impl Context {
    fn from(display_name: &str, symbol_table: &mut Rc<RefCell<SymbolTable>>) -> Self {
        Context {
            display_name: display_name.to_string(),
            parent: None,
            symbol_table: Rc::clone(symbol_table),
        }
    }
}

fn visit_node(node: &mut Node, context: &mut Context) -> Result<Number, Error> {
    match node {
        Node::NUM(token) => visit_numb_node(token, context),
        Node::UNRYOP(op, node) => visit_unryop_node(op, node, context),
        Node::BINOP(left, op, right) => visit_binop_node(left, op, right, context),
        Node::ACCESS(id) => visit_access_node(id, context),
        Node::ASSIGN(id, value) => visit_assign_node(id, value, context),
    }
}

fn visit_numb_node(token: &mut Token, context: &mut Context) -> Result<Number, Error> {
    match token.token_type {
        TokenType::INT(value) => Ok(Number {
            value: value.as_number_type(),
            start_pos: token.start_pos.clone(),
            end_pos: token.end_pos.clone(),
            context: Some(context.clone()),
        }),
        TokenType::FLOAT(value) => Ok(Number {
            value: value.as_number_type(),
            start_pos: token.start_pos.clone(),
            end_pos: token.end_pos.clone(),
            context: Some(context.clone()),
        }),
        _ => panic!("called visit_numb_node on a number node that has a non number token"),
    }
}

fn visit_access_node(token: &mut Token, context: &mut Context) -> Result<Number, Error> {
    let var = &token.token_type;
        match var {
            TokenType::ID(var_name) => {
                let mut entry = context.symbol_table.borrow().get(&var_name);

                match &mut entry {
                    Some(num) => {
                        num.set_position(token.start_pos.clone(), token.end_pos.clone());
                        Ok(num.clone())
                    },
                    None => Err(Error::new(
                        ErrType::RuntimeError,
                        &token.start_pos,
                        &token.end_pos,
                        format!("{:?} is not defined", var_name),
                        Some(context),
                    )),
                }
            }
            _ => panic!("called visit_access_node on a non ID token"),
        }
}

fn visit_assign_node(
    id: &mut Token,
    value: &mut Node,
    context: &mut Context,
) -> Result<Number, Error> {
    let v = visit_node(value, context)?;
    let t = id.clone();

    match t.token_type {
        TokenType::ID(var_name) => {
                context.symbol_table.borrow_mut().set(&var_name, v.clone());
                return Ok(v);
        }
        _ => panic!("called visit_assign_node on {:?}", value),
    }
}

fn visit_unryop_node(
    op: &mut Token,
    node: &mut Node,
    context: &mut Context,
) -> Result<Number, Error> {
    let mut number = visit_node(node, context)?;
    number.set_position(op.start_pos.clone(), number.end_pos.clone());

    match op.token_type {
        TokenType::SUB => Ok(number.mult(-1)),
        TokenType::ADD => Ok(number),

        _ => panic!("called visit_unryop_node on a binop node that has a non Operation token"),
    }
}

fn visit_binop_node(
    left: &mut Node,
    op: &mut Token,
    right: &mut Node,
    context: &mut Context,
) -> Result<Number, Error> {
    let mut left = visit_node(left, context)?;
    let right = visit_node(right, context)?;
    left.set_position(left.start_pos.clone(), right.end_pos.clone());

    match op.token_type {
        TokenType::ADD => Ok(left.add(right)),
        TokenType::SUB => Ok(left.sub(right)),
        TokenType::MUL => Ok(left.mult(right)),
        TokenType::DIV => left.div(right),
        TokenType::POW => left.pow(right),
        _ => panic!("called visit_binop_node on a binop node that has a non Operation token"),
    }
}

pub struct Compiler {
    pub global_symbol_table: Rc<RefCell<SymbolTable>>,
}

impl Compiler {

    pub fn new() -> Self {
        Compiler { global_symbol_table: Rc::new(RefCell::new(SymbolTable::empty())) }
    }

    pub fn interpret(&mut self, file_name: String, text: String) -> Result<Number, Error> {
        let mut lexer = Lexer::new(file_name, text);
        let tokens = lexer.parse_tokens()?;

        let mut parser = Parser::new(tokens);
        let mut ast = parser.parse()?;

        let mut context = Context::from("<program>", &mut self.global_symbol_table);

        let res = visit_node(&mut ast, &mut context);

        res
    }
}
