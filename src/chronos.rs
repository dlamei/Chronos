use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
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
}

fn keyword_to_string(k: Keyword) -> String {
    String::from(match k {
        Keyword::LET => "let",
        Keyword::AND => "and",
        Keyword::OR => "or",
        Keyword::NOT => "not",
    })
}

fn is_keyword(s: &String) -> bool {
    s.eq(&keyword_to_string(Keyword::LET))
        || s.eq(&keyword_to_string(Keyword::AND))
        || s.eq(&keyword_to_string(Keyword::OR))
        || s.eq(&keyword_to_string(Keyword::NOT))
}

fn get_keyword(s: &String) -> Result<Keyword, ()> {
    match s.as_ref() {
        "let" => Ok(Keyword::LET),
        "&&" => Ok(Keyword::AND),
        "||" => Ok(Keyword::OR),
        "!" => Ok(Keyword::NOT),
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
    LPAREN,
    RPAREN,
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
                    tokens.push(Token::new(TokenType::LPAREN, &self.position, None));
                    self.advance();
                    true
                }
                ')' => {
                    tokens.push(Token::new(TokenType::RPAREN, &self.position, None));
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
                            "Parser: expected ')' found {:?}",
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
            //(TokenType::POW, TokenType::POW),
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
        //ops: (TokenType, TokenType),
        ops: Vec<TokenType>,
        keywords: Vec<Keyword>,
        func_b: fn(parser: &mut Parser) -> Result<Node, Error>,
    ) -> Result<Node, Error> {
        let mut left_node = func_a(self)?;
        //while match_enum(&self.current_token.token_type, &ops.0)
        //    || match_enum(&self.current_token.token_type, &ops.1)

        //while match ops {
        //    (TokenType::KEYWRD(v1), TokenType::KEYWRD(v2)) => {
        //        if let TokenType::KEYWRD(v) = self.current_token.token_type {
        //            match_enum_type(&v1, &v) || match_enum_type(&v2, &v)
        //        } else {
        //            false
        //        }
        //    }
        //    _ => {
        //        match_enum_type(&self.current_token.token_type, &ops.0)
        //            || match_enum_type(&self.current_token.token_type, &ops.1)
        //    }
        //} {

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
                    "Parser: Expected INT, FLOAT, IDENTIFIER, '+', '-', '(' or '!'".into(),
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
                //(TokenType::KEYWRD(Keyword::AND), TokenType::KEYWRD(Keyword::OR)),
                Vec::new(),
                vec![Keyword::AND, Keyword::OR],
                Parser::comp_expression,
            ),
        }
    }

    //self.binary_operation(Parser::term, (TokenType::ADD, TokenType::SUB), Parser::term)
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
                (NumberType::INT(v1), NumberType::INT(v2)) => v1 == 1 && v2 == 1,
                (NumberType::FLOAT(v1), NumberType::FLOAT(v2)) => v1 == 1.0 && v2 == 1.0,
                (NumberType::INT(v1), NumberType::FLOAT(v2)) => v1 == 1 && v2 == 1.0,
                (NumberType::FLOAT(v1), NumberType::INT(v2)) => v1 == 1.0 && v2 == 1,
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
                (NumberType::INT(v1), NumberType::INT(v2)) => v1 == 1 || v2 == 1,
                (NumberType::FLOAT(v1), NumberType::FLOAT(v2)) => v1 == 1.0 || v2 == 1.0,
                (NumberType::INT(v1), NumberType::FLOAT(v2)) => v1 == 1 || v2 == 1.0,
                (NumberType::FLOAT(v1), NumberType::INT(v2)) => v1 == 1.0 || v2 == 1,
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
        TokenType::KEYWRD(Keyword::NOT) => Ok(number.not()),
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
        TokenType::LESS => Ok(left.less(right)),
        TokenType::EQUAL => Ok(left.equal(right)),
        TokenType::NEQUAL => Ok(left.not_equal(right)),
        TokenType::LESSEQ => Ok(left.less_equal(right)),
        TokenType::GREATER => Ok(left.greater(right)),
        TokenType::GREATEREQ => Ok(left.greater_equal(right)),
        TokenType::KEYWRD(Keyword::AND) => Ok(left.and(right)),
        TokenType::KEYWRD(Keyword::OR) => Ok(left.or(right)),
        _ => panic!("called visit_binop_node on a binop node that has a non Operation token"),
    }
}

pub struct Compiler {
    pub global_symbol_table: Rc<RefCell<SymbolTable>>,
}

impl Compiler {
    pub fn new() -> Self {
        let mut table = SymbolTable::empty();
        table.set(&String::from("False"), Number::from(0.as_number_type()));
        table.set(&String::from("True"), Number::from(1.as_number_type()));

        Compiler {
            global_symbol_table: Rc::new(RefCell::new(table)),
        }
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
