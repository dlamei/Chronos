use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::mem;
use std::rc::Rc;

use crate::errors::*;

const DIGITS: &str = "0123456789";
const LETTERS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

type ChInt = i32;
type ChUInt = i32;
type ChFloat = f32;
type ChBool = bool;

fn match_enum_type<T>(t1: &T, t2: &T) -> bool {
    mem::discriminant(t1) == mem::discriminant(t2)
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
    AND,
    OR,
    NOT,
    IF,
    ELIF,
    ELSE,
    WHILE
}

fn keyword_to_string(k: Keyword) -> String {
    String::from(match k {
        Keyword::LET => "let",
        Keyword::AND => "and",
        Keyword::OR => "or",
        Keyword::NOT => "not",
        Keyword::IF => "if",
        Keyword::ELIF => "elif",
        Keyword::ELSE => "else",
        Keyword::WHILE => "while",
    })
}

//fn is_keyword(s: &String) -> bool {
//    s.eq(&keyword_to_string(Keyword::LET))
//        || s.eq(&keyword_to_string(Keyword::AND))
//        || s.eq(&keyword_to_string(Keyword::OR))
//        || s.eq(&keyword_to_string(Keyword::NOT))
//}

fn get_keyword(s: &String) -> Result<Keyword, ()> {
    match s.as_ref() {
        "let" => Ok(Keyword::LET),
        "&&" => Ok(Keyword::AND),
        "||" => Ok(Keyword::OR),
        "!" => Ok(Keyword::NOT),
        "if" => Ok(Keyword::IF),
        "elif" => Ok(Keyword::ELIF),
        "else" => Ok(Keyword::ELSE),
        "while" => Ok(Keyword::WHILE),
        _ => Err(()),
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
    LROUND,
    RROUND,
    LCURLY,
    RCURLY,
    EOF,

    ID(String),
    KEYWRD(Keyword),
    ASSIGN,

    EQUAL,
    NEQUAL,
    LESS,
    LESSEQ,
    GREATER,
    GREATEREQ,
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
                    self.advance();
                    true
                }
                '-' => {
                    tokens.push(Token::new(TokenType::SUB, &self.position, None));
                    self.advance();
                    true
                }
                '/' => {
                    tokens.push(Token::new(TokenType::DIV, &self.position, None));
                    self.advance();
                    true
                }
                '*' => {
                    tokens.push(Token::new(TokenType::MUL, &self.position, None));
                    self.advance();
                    true
                }
                '^' => {
                    tokens.push(Token::new(TokenType::POW, &self.position, None));
                    self.advance();
                    true
                }
                '(' => {
                    tokens.push(Token::new(TokenType::LROUND, &self.position, None));
                    self.advance();
                    true
                }
                ')' => {
                    tokens.push(Token::new(TokenType::RROUND, &self.position, None));
                    self.advance();
                    true
                }
                '{' => {
                    tokens.push(Token::new(TokenType::LCURLY, &self.position, None));
                    self.advance();
                    true
                }
                '}' => {
                    tokens.push(Token::new(TokenType::RCURLY, &self.position, None));
                    self.advance();
                    true
                }
                '=' => {
                    tokens.push(self.make_equal());
                    true
                }
                '!' => {
                    tokens.push(self.make_not()?);
                    true
                }
                '<' => {
                    tokens.push(self.make_less());
                    true
                }
                '>' => {
                    tokens.push(self.make_greater());
                    true
                }

                '&' | '|' => {
                    tokens.push(self.make_keyword()?);
                    true
                }
                _ => false,
            } {
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
                    format!("Lexer: found '{}'", c),
                    None,
                ));
            }
        }

        tokens.push(Token::new(TokenType::EOF, &self.position, None));
        Ok(tokens)
    }

    fn make_keyword(&mut self) -> Result<Token, Error> {
        let mut keyword = self.current_char.unwrap().to_string();
        let start = self.position.clone();

        self.advance();
        if self.current_char != None {
            keyword.push(self.current_char.unwrap())
        }

        self.advance();
        match get_keyword(&keyword) {
            Ok(k) => Ok(Token::new(
                TokenType::KEYWRD(k),
                &start,
                Some(&self.position),
            )),
            Err(_) => Err(Error::new(
                ErrType::IllegalCharError,
                &start,
                &self.position,
                format!(
                    "Lexer: Unknown Keyword, expected '&&', '||' or '!' found '{}'",
                    keyword
                ),
                None,
            )),
        }
    }

    fn make_not(&mut self) -> Result<Token, Error> {
        let start = self.position.clone();
        self.advance();

        if self.current_char != None && self.current_char.unwrap() == '=' {
            self.advance();
            Ok(Token::new(TokenType::NEQUAL, &start, Some(&self.position)))
        } else {
            Ok(Token::new(
                TokenType::KEYWRD(Keyword::NOT),
                &start,
                Some(&self.position),
            ))
            //return Err(Error::new(
            //    ErrType::ExpectedCharError,
            //    &start,
            //    &self.position,
            //    "Lexer: Expected '=' after '!'".into(),
            //    None,
            //));
        }
    }

    fn make_equal(&mut self) -> Token {
        let start = self.position.clone();
        let mut token_type = TokenType::ASSIGN;
        self.advance();

        if self.current_char != None && self.current_char.unwrap() == '=' {
            self.advance();
            token_type = TokenType::EQUAL;
        }

        Token::new(token_type, &start, Some(&self.position))
    }

    fn make_less(&mut self) -> Token {
        let start = self.position.clone();
        let mut token_type = TokenType::LESS;
        self.advance();

        if self.current_char != None && self.current_char.unwrap() == '=' {
            self.advance();
            token_type = TokenType::LESSEQ;
        }

        Token::new(token_type, &start, Some(&self.position))
    }

    fn make_greater(&mut self) -> Token {
        let start = self.position.clone();
        let mut token_type = TokenType::GREATER;
        self.advance();

        if self.current_char != None && self.current_char.unwrap() == '=' {
            self.advance();
            token_type = TokenType::GREATEREQ;
        }

        Token::new(token_type, &start, Some(&self.position))
    }

    fn make_identifier(&mut self) -> Token {
        let mut id = String::from("");
        let pos_start = self.position.clone();

        let allowed = LETTERS.to_owned() + "_";

        while self.current_char != None && allowed.contains(self.current_char.unwrap()) {
            id.push(self.current_char.unwrap());
            self.advance();
        }

        let token_type = match get_keyword(&id) {
            Ok(k) => TokenType::KEYWRD(k),
            Err(()) => TokenType::ID(id),
        };
        //let token_type = if is_keyword(&id) {
        //    TokenType::KEYWRD(get_keyword(&id))
        //} else {
        //    TokenType::ID(id)
        //};
        Token::new(token_type, &pos_start, Some(&self.position))
    }

    //TODO: don't use strings
    fn make_number(&mut self) -> Token {
        let mut num: String = String::new();
        let mut dot_count: u8 = 0;

        let start = self.position.clone();

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
                &start,
                Some(&self.position),
            );
        }

        Token::new(
            TokenType::FLOAT(num.parse::<ChFloat>().unwrap()),
            &start,
            Some(&self.position),
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
    IF(Vec<(Node, Node)>, Option<Box<Node>>),
    WHILE(Box<Node>, Box<Node>),
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
                    "Parser: expected EOF found {:?}",
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
            TokenType::LROUND => {
                self.advance();
                let expr = self.expression()?;
                match self.current_token.token_type {
                    TokenType::RROUND => {
                        self.advance();
                        Ok(expr)
                    }
                    _ => Err(Error::new(
                        ErrType::InvalidSyntaxError,
                        &t.start_pos,
                        &self.current_token.end_pos,
                        format!(
                            "Parser: expected ')' found {:?}",
                            self.current_token.token_type
                        ),
                        None,
                    )),
                }
            }
            TokenType::KEYWRD(Keyword::IF) => self.if_expression(),
            TokenType::KEYWRD(Keyword::WHILE) => self.while_expression(),
            _ => Err(Error::new(
                ErrType::InvalidSyntaxError,
                &t.start_pos,
                &t.end_pos,
                format!(
                    "Parser: expected INT, FLOAT, IDENTIFIER, '+', '-' or '(, found: {:?}",
                    t.token_type
                ),
                None,
            )),
        };
    }

    fn power(&mut self) -> Result<Node, Error> {
        self.binary_operation(
            Parser::atom,
            vec![TokenType::POW],
            Vec::new(),
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
        ops: Vec<TokenType>,
        keywords: Vec<Keyword>,
        func_b: fn(parser: &mut Parser) -> Result<Node, Error>,
    ) -> Result<Node, Error> {
        let mut left_node = func_a(self)?;

        //Allowes chaining operators (e.g 1 + 1 + 1)
        while {
            let mut found = false;
            for t in &ops {
                if match_enum_type(t, &self.current_token.token_type) {
                    found = true;
                    break;
                }
            }
            if !found {
                if let TokenType::KEYWRD(k) = &self.current_token.token_type {
                    for key in &keywords {
                        if match_enum_type(key, &k) {
                            found = true;
                            break;
                        }
                    }
                }
            }
            found
        } {
            let op_token = self.current_token.clone();
            self.advance();
            let right_node = func_b(self)?;

            left_node = Node::BINOP(left_node.into(), op_token, right_node.into());
        }

        Ok(left_node)
    }

    fn if_expression(&mut self) -> Result<Node, Error> {
        let mut cases: Vec<(Node, Node)> = Vec::new();
        let mut else_case = None;

        if let TokenType::KEYWRD(Keyword::IF) = self.current_token.token_type {
        } else {
            return Err(Error::new(
                ErrType::InvalidSyntaxError,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!("Parser: expected IF found '{:?}'", self.current_token),
                None,
            ));
        }

        self.advance();

        let condition = self.expression()?;

        if !match_enum_type(&self.current_token.token_type, &TokenType::LCURLY) {
            return Err(Error::new(
                ErrType::InvalidSyntaxError,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!("Parser: expected LCURLY found '{:?}'", self.current_token),
                None,
            ));
        }

        self.advance();
        let expr = self.expression()?;
        cases.push((condition, expr));

        if !matches!(self.current_token.token_type, TokenType::RCURLY) {
            return Err(Error::new(
                ErrType::InvalidSyntaxError,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!("Parser: expected RCURLY found '{:?}'", self.current_token),
                None,
            ));
        }
        self.advance();

        while matches!(self.current_token.token_type, TokenType::KEYWRD(Keyword::ELIF)) {
            self.advance();
            let cond = self.expression()?;

            if !matches!(self.current_token.token_type, TokenType::LCURLY) {
                return Err(Error::new(
                    ErrType::InvalidSyntaxError,
                    &self.current_token.start_pos,
                    &self.current_token.end_pos,
                    format!("Parser: expected LCURLY found '{:?}'", self.current_token),
                    None,
                ));
            }
            self.advance();

            let expr = self.expression()?;
            cases.push((cond, expr));

            if !matches!(self.current_token.token_type, TokenType::RCURLY) {
                return Err(Error::new(
                    ErrType::InvalidSyntaxError,
                    &self.current_token.start_pos,
                    &self.current_token.end_pos,
                    format!("Parser: expected RCURLY found '{:?}'", self.current_token),
                    None,
                ));
            }
            self.advance();

        }

        if matches!(self.current_token.token_type, TokenType::KEYWRD(Keyword::ELSE)) {
        //if let TokenType::KEYWRD(Keyword::ELSE) = self.current_token.token_type {
            self.advance();
            if !matches!(self.current_token.token_type, TokenType::LCURLY) {
                return Err(Error::new(
                    ErrType::InvalidSyntaxError,
                    &self.current_token.start_pos,
                    &self.current_token.end_pos,
                    format!("Parser: expected LCURLY found '{:?}'", self.current_token),
                    None,
                ));
            }
            self.advance();

            else_case = Some(Box::new(self.expression()?));

            if !matches!(&self.current_token.token_type, &TokenType::RCURLY) {
                return Err(Error::new(
                    ErrType::InvalidSyntaxError,
                    &self.current_token.start_pos,
                    &self.current_token.end_pos,
                    format!("Parser: expected RCURLY found '{:?}'", self.current_token),
                    None,
                ));
            }

            self.advance();
        }

        Ok(Node::IF(cases, else_case))
    }

    fn while_expression(&mut self) -> Result<Node, Error> {
        if !matches!(self.current_token.token_type, TokenType::KEYWRD(Keyword::WHILE)) {
            return Err(Error::new(
                ErrType::InvalidSyntaxError,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!("Parser: expected IF found '{:?}'", self.current_token),
                None,
            ));
        }

        self.advance();
        let cond = self.expression()?;
        
        if !match_enum_type(&self.current_token.token_type, &TokenType::LCURLY) {
            return Err(Error::new(
                ErrType::InvalidSyntaxError,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!("Parser: expected LCURLY found '{:?}'", self.current_token),
                None,
            ));
        }

        self.advance();
        let body = self.expression()?;

        if !match_enum_type(&self.current_token.token_type, &TokenType::RCURLY) {
            return Err(Error::new(
                ErrType::InvalidSyntaxError,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!("Parser: expected RCURLY found '{:?}'", self.current_token),
                None,
            ));
        }

        self.advance();
        Ok(Node::WHILE(cond.into(), body.into()))
    }

    fn arith_expression(&mut self) -> Result<Node, Error> {
        self.binary_operation(
            Parser::term,
            vec![TokenType::ADD, TokenType::SUB],
            Vec::new(),
            Parser::term,
        )
    }

    fn comp_expression(&mut self) -> Result<Node, Error> {
        match self.current_token.token_type {
            TokenType::KEYWRD(Keyword::NOT) => {
                let op = self.current_token.clone();
                self.advance();
                let node = self.comp_expression()?;
                Ok(Node::UNRYOP(op, Box::new(node)))
            }

            _ => match self.binary_operation(
                Parser::arith_expression,
                vec![
                    TokenType::EQUAL,
                    TokenType::NEQUAL,
                    TokenType::LESS,
                    TokenType::LESSEQ,
                    TokenType::GREATER,
                    TokenType::GREATEREQ,
                ],
                Vec::new(),
                Parser::arith_expression,
            ) {
                Ok(node) => Ok(node),
                Err(_) => Err(Error::new(
                    ErrType::InvalidSyntaxError,
                    &self.current_token.start_pos,
                    &self.current_token.end_pos,
                    format!("Parser: expected INT, FLOAT, IDENTIFIER, '+', '-', '(' or '!'"),
                    None,
                )),
            },
        }
    }

    fn term(&mut self) -> Result<Node, Error> {
        self.binary_operation(
            Parser::factor,
            vec![TokenType::MUL, TokenType::DIV],
            Vec::new(),
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
                            TokenType::ASSIGN => {
                                self.advance();
                                Ok(Node::ASSIGN(var, Box::new(self.expression()?)))
                            }
                            _ => Err(Error::new(
                                ErrType::InvalidSyntaxError,
                                &self.current_token.start_pos,
                                &self.current_token.end_pos,
                                format!(
                                    "Parser: Expected assignment operator: '=', found: {:?}",
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
                        format!(
                            "Parser: Expected identifier, found: {:?}",
                            self.current_token
                        ),
                        None,
                    )),
                }
            }
            _ => self.binary_operation(
                Parser::comp_expression,
                Vec::new(),
                vec![Keyword::AND, Keyword::OR],
                Parser::comp_expression,
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub enum NumberType {
    INT(ChInt),
    FLOAT(ChFloat),
}

#[derive(Clone, Debug)]
pub struct None {
    start_pos: Position,
    end_pos: Position,
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

impl AsNumberType for ChBool {
    fn as_number_type(self) -> NumberType {
        NumberType::INT(if self { 1 } else { 0 })
    }

    fn get_value_type(&self) -> NumberType {
        NumberType::INT(if *self { 1 } else { 0 })
    }
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
    fn from(value: NumberType) -> Self {
        Number {
            value,
            start_pos: Position::empty(),
            end_pos: Position::empty(),
            context: None,
        }
    }

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

    fn equal<T: AsNumberType>(self, other: T) -> Number {
        let value = other.as_number_type();

        Number {
            value: match (self.value, value) {
                (NumberType::INT(v1), NumberType::INT(v2)) => v1 == v2,
                (NumberType::FLOAT(v1), NumberType::FLOAT(v2)) => v1 == v2,
                (NumberType::INT(v1), NumberType::FLOAT(v2)) => v1 as ChFloat == v2,
                (NumberType::FLOAT(v1), NumberType::INT(v2)) => v1 == v2 as ChFloat,
            }
            .as_number_type(),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
            context: self.context,
        }
    }

    fn not_equal<T: AsNumberType>(self, other: T) -> Number {
        let value = other.as_number_type();

        Number {
            value: match (self.value, value) {
                (NumberType::INT(v1), NumberType::INT(v2)) => v1 != v2,
                (NumberType::FLOAT(v1), NumberType::FLOAT(v2)) => v1 != v2,
                (NumberType::INT(v1), NumberType::FLOAT(v2)) => v1 as ChFloat != v2,
                (NumberType::FLOAT(v1), NumberType::INT(v2)) => v1 != v2 as ChFloat,
            }
            .as_number_type(),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
            context: self.context,
        }
    }

    fn less<T: AsNumberType>(self, other: T) -> Number {
        let value = other.as_number_type();

        Number {
            value: match (self.value, value) {
                (NumberType::INT(v1), NumberType::INT(v2)) => v1 < v2,
                (NumberType::FLOAT(v1), NumberType::FLOAT(v2)) => v1 < v2,
                (NumberType::INT(v1), NumberType::FLOAT(v2)) => (v1 as ChFloat) < v2,
                (NumberType::FLOAT(v1), NumberType::INT(v2)) => v1 < v2 as ChFloat,
            }
            .as_number_type(),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
            context: self.context,
        }
    }

    fn less_equal<T: AsNumberType>(self, other: T) -> Number {
        let value = other.as_number_type();

        Number {
            value: match (self.value, value) {
                (NumberType::INT(v1), NumberType::INT(v2)) => v1 <= v2,
                (NumberType::FLOAT(v1), NumberType::FLOAT(v2)) => v1 <= v2,
                (NumberType::INT(v1), NumberType::FLOAT(v2)) => (v1 as ChFloat) <= v2,
                (NumberType::FLOAT(v1), NumberType::INT(v2)) => v1 <= v2 as ChFloat,
            }
            .as_number_type(),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
            context: self.context,
        }
    }

    fn greater<T: AsNumberType>(self, other: T) -> Number {
        let value = other.as_number_type();

        Number {
            value: match (self.value, value) {
                (NumberType::INT(v1), NumberType::INT(v2)) => v1 > v2,
                (NumberType::FLOAT(v1), NumberType::FLOAT(v2)) => v1 > v2,
                (NumberType::INT(v1), NumberType::FLOAT(v2)) => (v1 as ChFloat) > v2,
                (NumberType::FLOAT(v1), NumberType::INT(v2)) => v1 > v2 as ChFloat,
            }
            .as_number_type(),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
            context: self.context,
        }
    }

    fn greater_equal<T: AsNumberType>(self, other: T) -> Number {
        let value = other.as_number_type();

        Number {
            value: match (self.value, value) {
                (NumberType::INT(v1), NumberType::INT(v2)) => v1 >= v2,
                (NumberType::FLOAT(v1), NumberType::FLOAT(v2)) => v1 >= v2,
                (NumberType::INT(v1), NumberType::FLOAT(v2)) => (v1 as ChFloat) >= v2,
                (NumberType::FLOAT(v1), NumberType::INT(v2)) => v1 >= v2 as ChFloat,
            }
            .as_number_type(),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
            context: self.context,
        }
    }

    fn and<T: AsNumberType>(self, other: T) -> Number {
        let value = other.as_number_type();

        Number {
            value: match (self.value, value) {
                (NumberType::INT(v1), NumberType::INT(v2)) => v1 >= 1 && v2 >= 1,
                (NumberType::FLOAT(v1), NumberType::FLOAT(v2)) => v1 >= 1.0 && v2 >= 1.0,
                (NumberType::INT(v1), NumberType::FLOAT(v2)) => v1 >= 1 && v2 >= 1.0,
                (NumberType::FLOAT(v1), NumberType::INT(v2)) => v1 >= 1.0 && v2 >= 1,
            }
            .as_number_type(),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
            context: self.context,
        }
    }

    fn or<T: AsNumberType>(self, other: T) -> Number {
        let value = other.as_number_type();

        Number {
            value: match (self.value, value) {
                (NumberType::INT(v1), NumberType::INT(v2)) => v1 >= 1 || v2 >= 1,
                (NumberType::FLOAT(v1), NumberType::FLOAT(v2)) => v1 >= 1.0 || v2 >= 1.0,
                (NumberType::INT(v1), NumberType::FLOAT(v2)) => v1 >= 1 || v2 >= 1.0,
                (NumberType::FLOAT(v1), NumberType::INT(v2)) => v1 >= 1.0 || v2 >= 1,
            }
            .as_number_type(),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
            context: self.context,
        }
    }

    fn not(mut self) -> Number {
        self.value = match self.value {
            NumberType::INT(value) => if value != 0 { 0 } else { 1 }.as_number_type(),
            NumberType::FLOAT(value) => if value != 0.0 { 0.0 } else { 1.0 }.as_number_type(),
        };
        self
    }

    fn is_true(&self) -> bool {
        match self.value {
            NumberType::INT(value) => value != 0,
            NumberType::FLOAT(value) => value != 0.0,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ChType {
    NUMBER(Number),
    NONE(None),
}

impl Display for ChType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ChType::NUMBER(n) => write!(f, "{:?}", n.get_value_type()),
            ChType::NONE(_) => write!(f, "none"),
        }
    }
}

impl ChType {
    pub fn get_start(&self) -> Position {
        match self {
            ChType::NUMBER(num) => num.start_pos.clone(),
            ChType::NONE(none) => none.start_pos.clone(),
        }
    }

    pub fn get_end(&self) -> Position {
        match self {
            ChType::NUMBER(num) => num.end_pos.clone(),
            ChType::NONE(none) => none.end_pos.clone(),
        }
    }

    pub fn set_pos(&mut self, start_pos: Position, end_pos: Position) {
        match self {
            ChType::NUMBER(num) => num.set_position(start_pos, end_pos),
            ChType::NONE(none) => {
                none.start_pos = start_pos;
                none.end_pos = end_pos;
            }
        }
    }

    pub fn is_true(&self) -> bool {
        match self {
            ChType::NUMBER(num) => num.is_true(),
            ChType::NONE(_) => false,
        }
    }
}

pub fn compare_chtype(t1: &ChType, t2: &ChType) -> bool {
    match (t1, t2) {
        (ChType::NUMBER(n1), ChType::NUMBER(n2)) => n1.clone().equal(n2.clone()).is_true(),
        (ChType::NONE(_), ChType::NONE(_)) => true,
        _ => false,
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
    table: HashMap<String, ChType>,
    immutable: Vec<String>,
}

impl SymbolTable {
    pub fn empty() -> Self {
        SymbolTable {
            parent: None,
            table: HashMap::new(),
            immutable: Vec::new(),
        }
    }

    fn get(&self, key: &String) -> Option<ChType> {
        match self.table.get(key) {
            Some(v) => Some(v.clone()),
            None => match &self.parent {
                Some(p) => p.get(key),
                None => None,
            },
        }
    }

    fn set_mut(&mut self, key: &String, value: ChType) -> bool {
        if self.table.contains_key(key) {
            if self.immutable.contains(&key) {
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

    fn set(&mut self, key: &String, value: ChType) -> bool {
        let b = self.set_mut(key, value);
        if b {
            self.immutable.push(key.to_string())
        };
        b
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

fn visit_node(node: &mut Node, context: &mut Context) -> Result<ChType, Error> {
    match node {
        Node::NUM(token) => visit_numb_node(token, context),
        Node::UNRYOP(op, node) => visit_unryop_node(op, node, context),
        Node::BINOP(left, op, right) => visit_binop_node(left, op, right, context),
        Node::ACCESS(id) => visit_access_node(id, context),
        Node::ASSIGN(id, value) => visit_assign_node(id, value, context),
        Node::IF(cases, else_case) => visit_if_node(cases, else_case, context),
        Node::WHILE(cond, body) => visit_while_node(cond, body, context),
    }
}

fn visit_numb_node(token: &mut Token, context: &mut Context) -> Result<ChType, Error> {
    match token.token_type {
        TokenType::INT(value) => Ok(ChType::NUMBER(Number {
            value: value.as_number_type(),
            start_pos: token.start_pos.clone(),
            end_pos: token.end_pos.clone(),
            context: Some(context.clone()),
        })),
        TokenType::FLOAT(value) => Ok(ChType::NUMBER(Number {
            value: value.as_number_type(),
            start_pos: token.start_pos.clone(),
            end_pos: token.end_pos.clone(),
            context: Some(context.clone()),
        })),
        _ => panic!("called visit_numb_node on a number node that has a non number token"),
    }
}

fn visit_access_node(token: &mut Token, context: &mut Context) -> Result<ChType, Error> {
    let var = &token.token_type;
    match var {
        TokenType::ID(var_name) => {
            let mut entry = context.symbol_table.borrow().get(&var_name);

            match &mut entry {
                Some(num) => {
                    num.set_pos(token.start_pos.clone(), token.end_pos.clone());
                    Ok(num.clone())
                }
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
) -> Result<ChType, Error> {
    let t = id.clone();
    let ch_type = visit_node(value, context)?;

    match t.token_type {
        TokenType::ID(var_name) => match ch_type {
            ChType::NUMBER(num) => {
                if !context
                    .symbol_table
                    .borrow_mut()
                    .set_mut(&var_name, ChType::NUMBER(num.clone()))
                {
                    return Err(Error::new(
                        ErrType::RuntimeError,
                        &num.start_pos,
                        &num.end_pos,
                        format!("cannot assign {:?} to {:?}", num.value, var_name),
                        Some(context),
                    ));
                }
                Ok(ChType::NUMBER(num))
            }
            ChType::NONE(none) => {
                if !context
                    .symbol_table
                    .borrow_mut()
                    .set_mut(&var_name, ChType::NONE(none.clone()))
                {
                    return Err(Error::new(
                        ErrType::RuntimeError,
                        &none.start_pos,
                        &none.end_pos,
                        format!("cannot assign 'none' to {:?}", var_name),
                        Some(context),
                    ));
                }
                Ok(ChType::NONE(none.clone()))
            }
        },
        _ => panic!("called visit_assign_node on {:?}", value),
    }
}

fn visit_unryop_node(
    op: &mut Token,
    node: &mut Node,
    context: &mut Context,
) -> Result<ChType, Error> {
    let mut ch_type = visit_node(node, context)?;
    ch_type.set_pos(op.start_pos.clone(), ch_type.get_end());

    match ch_type {
        ChType::NUMBER(n) => match op.token_type {
            TokenType::SUB => Ok(ChType::NUMBER(n.mult(-1))),
            TokenType::KEYWRD(Keyword::NOT) => Ok(ChType::NUMBER(n.not())),
            _ => panic!("called visit_unryop_node on a binop node that has a non Operation token"),
        },
        ChType::NONE(none) => Err(Error::new(
            ErrType::RuntimeError,
            &none.start_pos,
            &none.end_pos,
            String::from("undefined operation for type: 'none'"),
            Some(context),
        )),
    }
}

fn visit_binop_node(
    left: &mut Node,
    op: &mut Token,
    right: &mut Node,
    context: &mut Context,
) -> Result<ChType, Error> {
    let mut left = visit_node(left, context)?;
    let right = visit_node(right, context)?;
    left.set_pos(left.get_start(), right.get_end());

    let start = left.get_start();
    let end = left.get_end();

    match (left, right) {
        (ChType::NUMBER(n1), ChType::NUMBER(n2)) => Ok(ChType::NUMBER(match op.token_type {
            TokenType::ADD => n1.add(n2),
            TokenType::SUB => n1.sub(n2),
            TokenType::MUL => n1.mult(n2),
            TokenType::DIV => n1.div(n2)?,
            TokenType::POW => n1.pow(n2)?,
            TokenType::LESS => n1.less(n2),
            TokenType::EQUAL => n1.equal(n2),
            TokenType::NEQUAL => n1.not_equal(n2),
            TokenType::LESSEQ => n1.less_equal(n2),
            TokenType::GREATER => n1.greater(n2),
            TokenType::GREATEREQ => n1.greater_equal(n2),
            TokenType::KEYWRD(Keyword::AND) => n1.and(n2),
            TokenType::KEYWRD(Keyword::OR) => n1.or(n2),
            _ => panic!("called visit_binop_node on a binop node that has a non Operation token"),
        })),
        _ => Err(Error::new(
            ErrType::RuntimeError,
            &start,
            &end,
            format!("operation not defined for type: 'none'"),
            Some(context),
        )),
    }
}

fn visit_if_node(
    cases: &mut Vec<(Node, Node)>,
    else_case: &mut Option<Box<Node>>,
    context: &mut Context,
) -> Result<ChType, Error> {
    let mut start = Position::empty();
    let mut end = Position::empty();
    let mut first_cond = true;

    for (condition, expr) in cases {
        let cond = visit_node(condition, context)?;

        if first_cond {
            start = cond.get_start();
            end = cond.get_end();
            first_cond = false;
        }

        if cond.is_true() {
            return visit_node(expr, context);
        }
    }

    match else_case {
        Some(node) => visit_node(node, context),
        _ => Ok(ChType::NONE(None {
            start_pos: start,
            end_pos: end,
        })),
    }
}

fn visit_while_node(condition: &mut Node, body: &mut Node, context: &mut Context) -> Result<ChType, Error> {
    let mut start = Position::empty();
    let mut end = Position::empty();
    let mut first_cond = true;

    while visit_node(condition, context)?.is_true() {
        let res = visit_node(body, context)?;

        if first_cond {
            start = res.get_start();
            end = res.get_end();
            first_cond = false;
        }
    }

    Ok(ChType::NONE(None { start_pos: start, end_pos: end }))
}

pub struct Compiler {
    pub global_symbol_table: Rc<RefCell<SymbolTable>>,
}

impl Compiler {
    pub fn new() -> Self {
        let mut table = SymbolTable::empty();
        table.set(
            &String::from("false"),
            ChType::NUMBER(Number::from(0.as_number_type())),
        );
        table.set(
            &String::from("true"),
            ChType::NUMBER(Number::from(1.as_number_type())),
        );
        table.set(
            &String::from("none"),
            ChType::NONE(None {
                start_pos: Position::empty(),
                end_pos: Position::empty(),
            }),
        );

        Compiler {
            global_symbol_table: Rc::new(RefCell::new(table)),
        }
    }

    pub fn interpret(&mut self, file_name: String, text: String) -> Result<ChType, Error> {
        let mut lexer = Lexer::new(file_name, text);
        let tokens = lexer.parse_tokens()?;

        let mut parser = Parser::new(tokens);
        let mut ast = parser.parse()?;

        let mut context = Context::from("<program>", &mut self.global_symbol_table);

        let res = visit_node(&mut ast, &mut context);

        res
    }
}
