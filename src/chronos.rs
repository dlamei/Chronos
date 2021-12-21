use std::{
    cell::RefCell,
    collections::HashMap,
    fmt,
    fmt::{Debug, Display},
    mem,
    rc::Rc,
};

use crate::errors::*;

const DIGITS: &str = "0123456789";
const LETTERS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

type ChInt = i32;
type ChFloat = f32;

#[allow(clippy::mem_discriminant_non_enum)]
fn match_enum_type<T>(t1: &T, t2: &T) -> bool {
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

    fn from_name(file_name: String) -> Self {
        Position {
            file_name: Rc::new(file_name),
            ..Default::default()
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

fn get_keyword(s: &str) -> Result<Keyword, ()> {
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
                file_name: Rc::new(file_name),
                index: 0,
                line: 0,
                column: 0,
                text: Rc::new(text),
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
                    tokens.push(self.make_add()?);
                    true
                }
                '-' => {
                    tokens.push(self.make_sub()?);
                    true
                }
                '/' => {
                    tokens.push(Token::new(TokenType::Div, self.position.clone(), None));
                    self.advance();
                    true
                }
                '*' => {
                    tokens.push(Token::new(TokenType::Mul, self.position.clone(), None));
                    self.advance();
                    true
                }
                '"' | '\'' => {
                    tokens.push(self.make_string()?);
                    true
                }
                '^' => {
                    tokens.push(Token::new(TokenType::Pow, self.position.clone(), None));
                    self.advance();
                    true
                }
                '(' => {
                    tokens.push(Token::new(TokenType::LRound, self.position.clone(), None));
                    self.advance();
                    true
                }
                ')' => {
                    tokens.push(Token::new(TokenType::RRound, self.position.clone(), None));
                    self.advance();
                    true
                }
                '{' => {
                    tokens.push(Token::new(TokenType::LCurly, self.position.clone(), None));
                    self.advance();
                    true
                }
                '}' => {
                    tokens.push(Token::new(TokenType::RCurly, self.position.clone(), None));
                    self.advance();
                    true
                }
                ',' => {
                    tokens.push(Token::new(TokenType::Comma, self.position.clone(), None));
                    self.advance();
                    true
                }
                ';' => {
                    tokens.push(Token::new(TokenType::Semicln, self.position.clone(), None));
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
                    ErrType::IllegalChar,
                    &start_pos,
                    &self.position,
                    format!("Lexer: found '{}'", c),
                    None,
                ));
            }
        }

        tokens.push(Token::new(TokenType::Eof, self.position.clone(), None));
        Ok(tokens)
    }

    fn make_string(&mut self) -> Result<Token, Error> {
        let start = self.position.clone();
        let mut s = String::from("");
        self.advance();

        let escape_char = "\"\'";

        while self.current_char != None && !escape_char.contains(self.current_char.unwrap()) {
            s += &self.current_char.unwrap().to_string();
            self.advance();
        }
        self.advance();
        let end = self.position.clone();

        Ok(Token {
            token_type: TokenType::String(s),
            start_pos: start,
            end_pos: end,
        })
    }

    fn make_add(&mut self) -> Result<Token, Error> {
        let start = self.position.clone();
        self.advance();

        match self.current_char.unwrap() {
            '=' => {
                self.advance();
                Ok(Token::new(
                    TokenType::AddEq,
                    start,
                    Some(self.position.clone()),
                ))
            }
            _ => Ok(Token::new(TokenType::Add, start, None)),
        }
    }

    fn make_sub(&mut self) -> Result<Token, Error> {
        let start = self.position.clone();
        self.advance();

        match self.current_char.unwrap() {
            '=' => {
                self.advance();
                Ok(Token::new(
                    TokenType::SubEq,
                    start,
                    Some(self.position.clone()),
                ))
            }
            _ => Ok(Token::new(TokenType::Sub, start, None)),
        }
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
                TokenType::Keywrd(k),
                start,
                Some(self.position.clone()),
            )),
            Err(_) => Err(Error::new(
                ErrType::IllegalChar,
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
            Ok(Token::new(
                TokenType::NEqual,
                start,
                Some(self.position.clone()),
            ))
        } else {
            Ok(Token::new(
                TokenType::Keywrd(Keyword::Not),
                start,
                Some(self.position.clone()),
            ))
        }
    }

    fn make_equal(&mut self) -> Token {
        let start = self.position.clone();
        let mut token_type = TokenType::Assign;
        self.advance();

        if self.current_char != None && self.current_char.unwrap() == '=' {
            self.advance();
            token_type = TokenType::Equal;
        }

        Token::new(token_type, start, Some(self.position.clone()))
    }

    fn make_less(&mut self) -> Token {
        let start = self.position.clone();
        let mut token_type = TokenType::Less;
        self.advance();

        if self.current_char != None && self.current_char.unwrap() == '=' {
            self.advance();
            token_type = TokenType::LessEq;
        }

        Token::new(token_type, start, Some(self.position.clone()))
    }

    fn make_greater(&mut self) -> Token {
        let start = self.position.clone();
        let mut token_type = TokenType::Greater;
        self.advance();

        if self.current_char != None && self.current_char.unwrap() == '=' {
            self.advance();
            token_type = TokenType::GreaterEq;
        }

        Token::new(token_type, start, Some(self.position.clone()))
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
            Ok(k) => TokenType::Keywrd(k),
            Err(()) => TokenType::Id(id),
        };
        Token::new(token_type, pos_start, Some(self.position.clone()))
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
                TokenType::Int(num.parse::<ChInt>().unwrap()),
                start,
                Some(self.position.clone()),
            );
        }

        Token::new(
            TokenType::Float(num.parse::<ChFloat>().unwrap()),
            start,
            Some(self.position.clone()),
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
            TokenType::Eof => Ok(nodes),

            _ => Err(Error::new(
                ErrType::InvalidSyntax,
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

    fn retreat(&mut self) {
        self.token_index -= 1;
        self.current_token = self.tokens[self.token_index].clone();
    }

    fn atom(&mut self) -> Result<Node, Error> {
        let t = self.current_token.clone();

        return match t.token_type {
            TokenType::Int(_) | TokenType::Float(_) => {
                self.advance();
                Ok(Node::Num(t))
            }
            TokenType::String(_) => {
                self.advance();
                Ok(Node::String(t))
            }
            TokenType::Id(_) => {
                self.advance();
                Ok(Node::Access(t))
            }
            TokenType::LRound => {
                self.advance();
                let expr = self.expression()?;
                match self.current_token.token_type {
                    TokenType::RRound => {
                        self.advance();
                        Ok(expr)
                    }
                    _ => Err(Error::new(
                        ErrType::InvalidSyntax,
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
            TokenType::Keywrd(Keyword::If) => self.if_expression(),
            TokenType::Keywrd(Keyword::While) => self.while_expression(),
            TokenType::Keywrd(Keyword::For) => self.for_expression(),
            TokenType::Keywrd(Keyword::Func) => self.func_expression(),
            _ => Err(Error::new(
                ErrType::InvalidSyntax,
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
            Parser::call,
            vec![TokenType::Pow],
            Vec::new(),
            Parser::factor,
        )
    }

    fn call(&mut self) -> Result<Node, Error> {
        let res = self.atom()?;

        if matches!(self.current_token.token_type, TokenType::LRound) {
            self.advance();
            let mut arg_nodes: Vec<Node> = Vec::new();

            if !matches!(self.current_token.token_type, TokenType::RRound) {
                arg_nodes.push(self.expression()?);

                while matches!(self.current_token.token_type, TokenType::Comma) {
                    self.advance();
                    arg_nodes.push(self.expression()?);
                }

                if !matches!(self.current_token.token_type, TokenType::RRound,) {
                    return Err(Error::new(
                        ErrType::InvalidSyntax,
                        &self.current_token.start_pos,
                        &self.current_token.end_pos,
                        format!("Parser: expected RROUND found '{:?}'", self.current_token),
                        None,
                    ));
                }
            }

            self.advance();
            Ok(Node::Call(res.into(), arg_nodes))
        } else {
            Ok(res)
        }
    }

    fn factor(&mut self) -> Result<Node, Error> {
        let t = self.current_token.clone();

        match t.token_type {
            TokenType::Sub | TokenType::Add => {
                self.advance();
                let factor = self.factor()?;
                Ok(Node::UnryOp(t, factor.into()))
            }
            _ => self.power(),
        }
    }

    fn binary_operation(
        &mut self,
        func_a: fn(parser: &mut Parser) -> Result<Node, Error>,
        ops: Vec<TokenType>,
        keywords: Vec<Keyword>,
        func_b: fn(parser: &mut Parser) -> Result<Node, Error>,
    ) -> Result<Node, Error> {
        let mut left_node = func_a(self)?;

        let res = {
            let mut found = false;
            for t in &ops {
                if match_enum_type(t, &self.current_token.token_type) {
                    found = true;
                    break;
                }
            }
            if !found {
                if let TokenType::Keywrd(k) = &self.current_token.token_type {
                    for key in &keywords {
                        if match_enum_type(key, k) {
                            found = true;
                            break;
                        }
                    }
                }
            }
            found
        }; if res {
            let op_token = self.current_token.clone();
            self.advance();
            let right_node = func_b(self)?;

            left_node = Node::BinOp(left_node.into(), op_token, right_node.into());
        }

        Ok(left_node)
    }

    fn expect_token(&self, token: TokenType) -> Result<(), Error> {
        if !match_enum_type(&self.current_token.token_type, &token) {
            Err(Error::new(
                ErrType::InvalidSyntax,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!(
                    "Parser: expected {:?} found '{:?}'",
                    token, self.current_token
                ),
                None,
            ))
        } else {
            Ok(())
        }
    }

    fn if_expression(&mut self) -> Result<Node, Error> {
        let mut cases: Vec<(Node, Node)> = Vec::new();
        let mut else_case = None;

        if let TokenType::Keywrd(Keyword::If) = self.current_token.token_type {
        } else {
            return Err(Error::new(
                ErrType::InvalidSyntax,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!("Parser: expected IF found '{:?}'", self.current_token),
                None,
            ));
        }

        self.advance();

        let condition = self.expression()?;

        if !match_enum_type(&self.current_token.token_type, &TokenType::LCurly) {
            return Err(Error::new(
                ErrType::InvalidSyntax,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!("Parser: expected LCURLY found '{:?}'", self.current_token),
                None,
            ));
        }

        self.advance();
        let expr = self.expression()?;
        cases.push((condition, expr));

        if !matches!(self.current_token.token_type, TokenType::RCurly) {
            return Err(Error::new(
                ErrType::InvalidSyntax,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!("Parser: expected RCURLY found '{:?}'", self.current_token),
                None,
            ));
        }
        self.advance();

        while matches!(
            self.current_token.token_type,
            TokenType::Keywrd(Keyword::Elif)
        ) {
            self.advance();
            let cond = self.expression()?;

            if !matches!(self.current_token.token_type, TokenType::LCurly) {
                return Err(Error::new(
                    ErrType::InvalidSyntax,
                    &self.current_token.start_pos,
                    &self.current_token.end_pos,
                    format!("Parser: expected LCURLY found '{:?}'", self.current_token),
                    None,
                ));
            }
            self.advance();

            let expr = self.expression()?;
            cases.push((cond, expr));

            if !matches!(self.current_token.token_type, TokenType::RCurly) {
                return Err(Error::new(
                    ErrType::InvalidSyntax,
                    &self.current_token.start_pos,
                    &self.current_token.end_pos,
                    format!("Parser: expected RCURLY found '{:?}'", self.current_token),
                    None,
                ));
            }
            self.advance();
        }

        if matches!(
            self.current_token.token_type,
            TokenType::Keywrd(Keyword::Else)
        ) {
            self.advance();
            if !matches!(self.current_token.token_type, TokenType::LCurly) {
                return Err(Error::new(
                    ErrType::InvalidSyntax,
                    &self.current_token.start_pos,
                    &self.current_token.end_pos,
                    format!("Parser: expected LCURLY found '{:?}'", self.current_token),
                    None,
                ));
            }
            self.advance();

            else_case = Some(Box::new(self.expression()?));

            if !matches!(&self.current_token.token_type, &TokenType::RCurly) {
                return Err(Error::new(
                    ErrType::InvalidSyntax,
                    &self.current_token.start_pos,
                    &self.current_token.end_pos,
                    format!("Parser: expected RCURLY found '{:?}'", self.current_token),
                    None,
                ));
            }

            self.advance();
        }

        Ok(Node::If(cases, else_case))
    }

    fn func_expression(&mut self) -> Result<Node, Error> {
        let mut start: Option<Position> = None;
        let end: Option<Position>;

        if !matches!(
            self.current_token.token_type,
            TokenType::Keywrd(Keyword::Func)
        ) {
            return Err(Error::new(
                ErrType::InvalidSyntax,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!("Parser: expected FUNC found '{:?}'", self.current_token),
                None,
            ));
        }

        self.advance();

        let mut var_name: Option<Token> = None;

        if matches!(self.current_token.token_type, TokenType::Id(_)) {
            var_name = Some(self.current_token.clone());
            start = Some(self.current_token.start_pos.clone());
            self.advance();
        }

        self.expect_token(TokenType::LRound)?;
        self.advance();

        let mut arg_tokens: Vec<Token> = Vec::new();

        if matches!(self.current_token.token_type, TokenType::Id(_),) {
            arg_tokens.push(self.current_token.clone());
            if start.is_none() {
                start = Some(self.current_token.start_pos.clone());
            }

            self.advance();

            while matches!(self.current_token.token_type, TokenType::Comma) {
                self.advance();
                self.expect_token(TokenType::Id(String::from("")))?;

                arg_tokens.push(self.current_token.clone());
                self.advance();
            }
        }
        self.expect_token(TokenType::RRound)?;

        self.advance();

        self.expect_token(TokenType::LCurly)?;

        if start.is_none() {
            start = Some(self.current_token.start_pos.clone());
        }

        self.advance();

        let body = self.expression()?;

        self.expect_token(TokenType::RCurly)?;

        end = Some(self.current_token.end_pos.clone());

        self.advance();

        Ok(Node::FuncDef(
            var_name,
            arg_tokens,
            body.into(),
            start.unwrap_or_default(),
            end.unwrap_or_default(),
        ))
    }

    fn for_expression(&mut self) -> Result<Node, Error> {
        let mut c1: Option<Box<Node>> = None;
        let mut c3: Option<Box<Node>> = None;

        let start: Position;
        let end: Position;

        if !matches!(
            self.current_token.token_type,
            TokenType::Keywrd(Keyword::For)
        ) {
            return Err(Error::new(
                ErrType::InvalidSyntax,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!("Parser: expected FOR found '{:?}'", self.current_token),
                None,
            ));
        }

        self.advance();

        start = self.current_token.start_pos.clone();
        if !match_enum_type(&self.current_token.token_type, &TokenType::Semicln) {
            c1 = Some(self.expression()?.into());
        }

        self.expect_token(TokenType::Semicln)?;
        self.advance();

        let c2 = self.expression()?;

        self.expect_token(TokenType::Semicln)?;
        self.advance();

        if !match_enum_type(&self.current_token.token_type, &TokenType::LCurly) {
            c3 = Some(self.expression()?.into());
        }

        self.expect_token(TokenType::LCurly)?;
        self.advance();

        let body = self.expression()?;

        self.expect_token(TokenType::RCurly)?;
        end = self.current_token.start_pos.clone();
        self.advance();

        Ok(Node::For(c1, c2.into(), c3, body.into(), start, end))
    }

    fn while_expression(&mut self) -> Result<Node, Error> {
        if !matches!(
            self.current_token.token_type,
            TokenType::Keywrd(Keyword::While)
        ) {
            return Err(Error::new(
                ErrType::InvalidSyntax,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!("Parser: expected WHILE found '{:?}'", self.current_token),
                None,
            ));
        }

        let start = self.current_token.start_pos.clone();

        self.advance();
        let cond = self.expression()?;

        self.expect_token(TokenType::LCurly)?;
        self.advance();

        let body = self.expression()?;

        self.expect_token(TokenType::RCurly)?;

        let end = self.current_token.end_pos.clone();

        self.advance();

        Ok(Node::While(cond.into(), body.into(), start, end))
    }

    fn arith_expression(&mut self) -> Result<Node, Error> {
        self.binary_operation(
            Parser::term,
            vec![TokenType::Add, TokenType::Sub],
            Vec::new(),
            Parser::term,
        )
    }

    fn comp_expression(&mut self) -> Result<Node, Error> {
        match self.current_token.token_type {
            TokenType::Keywrd(Keyword::Not) => {
                let op = self.current_token.clone();
                self.advance();
                let node = self.comp_expression()?;
                Ok(Node::UnryOp(op, Box::new(node)))
            }

            _ => match self.binary_operation(
                Parser::arith_expression,
                vec![
                    TokenType::Equal,
                    TokenType::AddEq,
                    TokenType::SubEq,
                    TokenType::NEqual,
                    TokenType::Less,
                    TokenType::LessEq,
                    TokenType::Greater,
                    TokenType::GreaterEq,
                ],
                Vec::new(),
                Parser::arith_expression,
            ) {
                Ok(node) => Ok(node),
                Err(e) => Err(e),
                //TODO: look at error handling again
                //Err(_) => Err(Error::new(
                //    ErrType::InvalidSyntaxError,
                //    &self.current_token.start_pos,
                //    &self.current_token.end_pos,
                //    format!("Parser: expected INT, FLOAT, IDENTIFIER, '+', '-', '(' or '!'"),
                //    None,
                //)),
            },
        }
    }

    fn term(&mut self) -> Result<Node, Error> {
        self.binary_operation(
            Parser::factor,
            vec![TokenType::Mul, TokenType::Div],
            Vec::new(),
            Parser::factor,
        )
    }

    fn expression(&mut self) -> Result<Node, Error> {
        match self.current_token.token_type {
            TokenType::Id(_) => {
                let var = self.current_token.clone();
                self.advance();

                match self.current_token.token_type {
                    TokenType::Assign => {
                        self.advance();
                        Ok(Node::Assign(var, Box::new(self.expression()?)))
                    }
                    _ => {
                        self.retreat();
                        self.binary_operation(
                            Parser::comp_expression,
                            Vec::new(),
                            vec![Keyword::And, Keyword::Or],
                            Parser::comp_expression,
                        )
                    }
                }
            }
            _ => self.binary_operation(
                Parser::comp_expression,
                Vec::new(),
                vec![Keyword::And, Keyword::Or],
                Parser::comp_expression,
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub enum NumberType {
    Int(ChInt),
    Float(ChFloat),
}

#[derive(Clone, Debug)]
pub struct ChNone {
    start_pos: Position,
    end_pos: Position,
}

impl HasContext for ChNone {}

impl HasPosition for ChNone {
    fn get_start(&self) -> Position {
        self.start_pos.clone()
    }

    fn get_end(&self) -> Position {
        self.end_pos.clone()
    }

    fn set_position(&mut self, start_pos: Position, end_pos: Position) {
        self.start_pos = start_pos;
        self.end_pos = end_pos;
    }
}

impl ChOperators for ChNone {
    fn equal(self, other: ChType) -> Result<ChType, Error> {
        Ok(ChBool {
            value: matches!(other, ChType::None(_)),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn not_equal(self, other: ChType) -> Result<ChType, Error> {
        Ok(ChBool {
            value: !matches!(other, ChType::None(_)),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn is_true(&self) -> bool {
        false
    }
}

impl IsChValue for ChNone {
    fn get_desc(&self) -> String {
        String::from("None")
    }

    fn into_type(self) -> ChType {
        ChType::None(self)
    }
}

impl ConvertType for ChNone {
    fn convert_to_number(self) -> Result<NumberType, Error> {
        Err(Error::new(
            ErrType::Runtime,
            &self.get_start(),
            &self.get_end(),
            String::from("can not convert 'none' to Numbertype"),
            None,
        ))
    }
}

impl Display for ChNone {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "none")
    }
}

pub trait ChOperators {
    fn add(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        Err(Error::new(
            ErrType::UndefinedOperator,
            &self.get_start(),
            &self.get_end(),
            format!("operation 'add' not defined for type: {}", self.get_desc()),
            None,
        ))
    }
    fn add_equal(self, other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        self.add(other)
    }
    fn sub_equal(self, other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        self.sub(other)
    }
    fn sub(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        Err(Error::new(
            ErrType::UndefinedOperator,
            &self.get_start(),
            &self.get_end(),
            format!("operation 'sub' not defined for type: {}", self.get_desc()),
            None,
        ))
    }
    fn mult(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        Err(Error::new(
            ErrType::UndefinedOperator,
            &self.get_start(),
            &self.get_end(),
            format!("operation 'mult' not defined for type: {}", self.get_desc()),
            None,
        ))
    }
    fn div(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        Err(Error::new(
            ErrType::UndefinedOperator,
            &self.get_start(),
            &self.get_end(),
            format!("operation 'div' not defined for type: {}", self.get_desc()),
            None,
        ))
    }
    fn pow(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        Err(Error::new(
            ErrType::UndefinedOperator,
            &self.get_start(),
            &self.get_end(),
            format!("operation 'pow' not defined for type: {}", self.get_desc()),
            None,
        ))
    }
    fn equal(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        Err(Error::new(
            ErrType::UndefinedOperator,
            &self.get_start(),
            &self.get_end(),
            format!(
                "operation 'equal' not defined for type: {}",
                self.get_desc()
            ),
            None,
        ))
    }
    fn not_equal(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        Err(Error::new(
            ErrType::UndefinedOperator,
            &self.get_start(),
            &self.get_end(),
            format!(
                "operation 'not equal' not defined for type: {}",
                self.get_desc()
            ),
            None,
        ))
    }
    fn less(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        Err(Error::new(
            ErrType::UndefinedOperator,
            &self.get_start(),
            &self.get_end(),
            format!("operation 'less' not defined for type: {}", self.get_desc()),
            None,
        ))
    }
    fn less_equal(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        Err(Error::new(
            ErrType::UndefinedOperator,
            &self.get_start(),
            &self.get_end(),
            format!(
                "operation 'less equal' not defined for type: {}",
                self.get_desc()
            ),
            None,
        ))
    }
    fn greater(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        Err(Error::new(
            ErrType::UndefinedOperator,
            &self.get_start(),
            &self.get_end(),
            format!(
                "operation 'greater' not defined for type: {}",
                self.get_desc()
            ),
            None,
        ))
    }
    fn greater_equal(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        Err(Error::new(
            ErrType::UndefinedOperator,
            &self.get_start(),
            &self.get_end(),
            format!(
                "operation 'greater equal' not defined for type: {}",
                self.get_desc()
            ),
            None,
        ))
    }
    fn and(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        Err(Error::new(
            ErrType::UndefinedOperator,
            &self.get_start(),
            &self.get_end(),
            format!("operation 'and' not defined for type: {}", self.get_desc()),
            None,
        ))
    }
    fn or(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        Err(Error::new(
            ErrType::UndefinedOperator,
            &self.get_start(),
            &self.get_end(),
            format!("operation 'or' not defined for type: {}", self.get_desc()),
            None,
        ))
    }
    fn not(self) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        Err(Error::new(
            ErrType::UndefinedOperator,
            &self.get_start(),
            &self.get_end(),
            format!("operation 'not' not defined for type: {}", self.get_desc()),
            None,
        ))
    }

    fn is_true(&self) -> bool
    where
        Self: IsChValue + Sized,
    {
        false
    }

    fn negate(self) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        Err(Error::new(
            ErrType::UndefinedOperator,
            &self.get_start(),
            &self.get_end(),
            format!(
                "operation 'negate' not defined for type: {}",
                self.get_desc()
            ),
            None,
        ))
    }
}

trait IsFunction {
    fn execute(&mut self, args: Vec<ChType>, name: Option<String>) -> Result<ChType, Error>;
}

#[allow(clippy::large_enum_variant)]
#[derive(Clone, Debug)]
enum FuncType {
    RustFunc(RustFunc),
    ChronFunc(ChronosFunc),
}

#[derive(Clone)]
pub struct RustFunc {
    name: String,
    function: fn(args: Vec<ChType>, name: Option<String>) -> Result<ChType, Error>,
}

impl HasContext for RustFunc {
    fn set_context(&mut self, _context: Rc<RefCell<Context>>) {}
}

impl IsFunction for RustFunc {
    fn execute(&mut self, args: Vec<ChType>, name: Option<String>) -> Result<ChType, Error> {
        (self.function)(args, name)
    }
}

impl Display for RustFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "rust function<{}>", self.name)
    }
}

impl Debug for RustFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "function implemented in rust")
    }
}

#[derive(Clone, Debug)]
pub struct ChronosFunc {
    name: String,
    args_name: Vec<Token>,
    body: Node,
    start_pos: Position,
    end_pos: Position,
    context: Rc<RefCell<Context>>,
}

impl IsFunction for ChronosFunc {
    fn execute(&mut self, mut args: Vec<ChType>, name: Option<String>) -> Result<ChType, Error> {
        let mut n_context = Context::from_parent(
            format!("<function: {}>", name.unwrap_or_else(|| self.name.clone())),
            self.context.clone(),
            self.start_pos.clone(),
        );

        if args.len() != self.args_name.len() {
            return Err(Error::new(
                ErrType::Runtime,
                &self.start_pos,
                &self.end_pos,
                format!(
                    "expected {} arguments, found {} in function '{}'",
                    self.args_name.len(),
                    args.len(),
                    self.name,
                ),
                Some(self.context.clone()),
            ));
        }

        for i in 0..args.len() {
            let value = args.get_mut(i).unwrap();
            let n = &self.args_name[i];
            let name = match &n.token_type {
                TokenType::Id(s) => s,
                _ => {
                    return Err(Error::new(
                        ErrType::Runtime,
                        &n.start_pos,
                        &n.end_pos,
                        format!("could not resolve {:?} as an argument", n),
                        Some(n_context.clone()),
                    ))
                }
            };
            value.set_context(self.context.clone());
            n_context.borrow_mut().set_mut(name, value.clone());
        }

        visit_node(&mut self.body, &mut n_context)
    }
}

impl Display for ChronosFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "function<{}, {:?}>", self.name, self.args_name)
    }
}

impl HasContext for ChronosFunc {
    fn set_context(&mut self, context: Rc<RefCell<Context>>) {
        self.context = context;
    }
}

impl HasPosition for ChronosFunc {
    fn get_start(&self) -> Position {
        self.start_pos.clone()
    }

    fn get_end(&self) -> Position {
        self.end_pos.clone()
    }

    fn set_position(&mut self, start_pos: Position, end_pos: Position) {
        self.start_pos = start_pos;
        self.end_pos = end_pos;
    }
}

#[derive(Clone, Debug)]
pub struct ChFunction {
    func_type: FuncType,
}

impl Display for ChFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.func_type {
            FuncType::ChronFunc(func) => write!(f, "{}", func),
            FuncType::RustFunc(func) => write!(f, "{}", func),
        }
    }
}

impl HasContext for ChFunction {
    fn set_context(&mut self, context: Rc<RefCell<Context>>) {
        match &mut self.func_type {
            FuncType::ChronFunc(func) => func.set_context(context),
            FuncType::RustFunc(func) => func.set_context(context),
        }
    }
}

impl ConvertType for ChFunction {}

impl HasPosition for ChFunction {
    fn get_start(&self) -> Position {
        match &self.func_type {
            FuncType::ChronFunc(func) => func.get_start(),
            FuncType::RustFunc(func) => Position::from_name(func.name.clone()),
        }
    }

    fn get_end(&self) -> Position {
        match &self.func_type {
            FuncType::ChronFunc(func) => func.get_end(),
            FuncType::RustFunc(func) => Position::from_name(func.name.clone()),
        }
    }

    fn set_position(&mut self, start_pos: Position, end_pos: Position) {
        match &mut self.func_type {
            FuncType::ChronFunc(func) => func.set_position(start_pos, end_pos),
            FuncType::RustFunc(_) => (),
        }
    }
}

impl IsChValue for ChFunction {
    fn get_desc(&self) -> String {
        String::from("function")
    }

    fn into_type(self) -> ChType {
        ChType::Function(self.into())
    }
}

impl ChOperators for ChFunction {
    fn is_true(&self) -> bool {
        true
    }
}

impl IsFunction for ChFunction {
    fn execute(&mut self, args: Vec<ChType>, name: Option<String>) -> Result<ChType, Error> {
        match &mut self.func_type {
            FuncType::ChronFunc(func) => func.execute(args, name),
            FuncType::RustFunc(func) => func.execute(args, name),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ChBool {
    value: bool,
    start_pos: Position,
    end_pos: Position,
}

impl ChBool {
    fn from(v: bool) -> Self {
        ChBool {
            value: v,
            start_pos: Position::default(),
            end_pos: Position::default(),
        }
    }
}

impl HasContext for ChBool {}

impl Display for ChBool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", if self.value { "true" } else { "false" })
    }
}

impl HasPosition for ChBool {
    fn get_start(&self) -> Position {
        self.start_pos.clone()
    }

    fn get_end(&self) -> Position {
        self.end_pos.clone()
    }

    fn set_position(&mut self, start_pos: Position, end_pos: Position) {
        self.start_pos = start_pos;
        self.end_pos = end_pos;
    }
}

impl ChOperators for ChBool {
    fn equal(self, other: ChType) -> Result<ChType, Error> {
        Ok(ChBool {
            value: match other {
                ChType::Bool(b) => self.value == b.value,
                _ => false,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn not_equal(self, other: ChType) -> Result<ChType, Error> {
        Ok(ChBool {
            value: match other {
                ChType::Bool(b) => self.value != b.value,
                _ => true,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn not(mut self) -> Result<ChType, Error> {
        self.value = !self.value;
        Ok(self.into_type())
    }

    fn is_true(&self) -> bool {
        self.value
    }

    fn and(self, other: ChType) -> Result<ChType, Error> {
        Ok(ChBool {
            value: self.value && other.is_true(),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn or(self, other: ChType) -> Result<ChType, Error> {
        Ok(ChBool {
            value: self.value || other.is_true(),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }
}

impl ConvertType for ChBool {
    fn into_number_type(self) -> NumberType {
        NumberType::Int(self.value as i32)
    }

    fn convert_to_number(self) -> Result<NumberType, Error> {
        Ok(NumberType::Int(self.value as i32))
    }

    fn get_number_type(&self) -> NumberType {
        NumberType::Int(self.value as i32)
    }
}

impl IsChValue for ChBool {
    fn get_desc(&self) -> String {
        String::from("Bool")
    }

    fn into_type(self) -> ChType {
        ChType::Bool(self)
    }
}

#[derive(Debug, Clone)]
pub struct ChNumber {
    value: NumberType,
    start_pos: Position,
    end_pos: Position,
}

impl HasContext for ChNumber {}

pub trait IsChValue: Display + HasPosition + HasContext + ChOperators + ConvertType {
    fn get_desc(&self) -> String;
    fn into_type(self) -> ChType;
}

impl IsChValue for ChNumber {
    fn get_desc(&self) -> String {
        String::from("Number")
    }

    fn into_type(self) -> ChType {
        ChType::Number(self)
    }
}

impl Display for ChNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.value {
            NumberType::Int(v) => write!(f, "{}", v),
            NumberType::Float(v) => write!(f, "{}", v),
        }
    }
}

pub trait ConvertType {
    fn into_number_type(self) -> NumberType
    where
        Self: Sized,
    {
        panic!("value can't be converted");
    }

    fn convert_to_number(self) -> Result<NumberType, Error>
    where
        Self: Sized + HasPosition,
    {
        Err(Error::new(
            ErrType::Runtime,
            &self.get_start(),
            &self.get_end(),
            String::from("could not convert to Number"),
            None,
        ))
    }

    fn get_number_type(&self) -> NumberType
    where
        Self: Sized,
    {
        panic!("value can't be converted");
    }
}

pub trait HasPosition {
    fn get_start(&self) -> Position;
    fn get_end(&self) -> Position;
    fn set_position(&mut self, start_pos: Position, end_pos: Position);
}

pub trait HasContext {
    fn set_context(&mut self, _context: Rc<RefCell<Context>>) {}
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

impl ConvertType for ChNumber {
    fn into_number_type(self) -> NumberType {
        self.value
    }

    fn get_number_type(&self) -> NumberType {
        self.value.clone()
    }

    fn convert_to_number(self) -> Result<NumberType, Error> {
        Ok(self.value)
    }
}

impl HasPosition for ChNumber {
    fn get_start(&self) -> Position {
        self.start_pos.clone()
    }

    fn get_end(&self) -> Position {
        self.end_pos.clone()
    }

    fn set_position(&mut self, start_pos: Position, end_pos: Position) {
        self.start_pos = start_pos;
        self.end_pos = end_pos;
    }
}

impl ChNumber {
    fn from(value: NumberType, _context: &mut Rc<RefCell<Context>>) -> Self {
        ChNumber {
            value,
            start_pos: Position::default(),
            end_pos: Position::default(),
        }
    }

    fn into_token(self) -> Token {
        match self.value {
            NumberType::Int(v) => Token::new(TokenType::Int(v), self.start_pos, Some(self.end_pos)),
            NumberType::Float(v) => {
                Token::new(TokenType::Float(v), self.start_pos, Some(self.end_pos))
            }
        }
    }

    fn operate_on(
        mut self,
        other: NumberType,
        int_op: fn(ChInt, ChInt) -> ChInt,
        float_op: fn(ChFloat, ChFloat) -> ChFloat,
    ) -> Self {
        self.value = match (self.value, other) {
            (NumberType::Int(v1), NumberType::Int(v2)) => NumberType::Int(int_op(v1, v2)),
            (NumberType::Float(v1), NumberType::Int(v2)) => {
                NumberType::Float(float_op(v1, v2 as ChFloat))
            }
            (NumberType::Int(v1), NumberType::Float(v2)) => {
                NumberType::Float(float_op(v1 as ChFloat, v2))
            }
            (NumberType::Float(v1), NumberType::Float(v2)) => NumberType::Float(float_op(v1, v2)),
        };

        self
    }
}

impl ChOperators for ChNumber {
    fn add(self, other: ChType) -> Result<ChType, Error> {
        Ok(self
            .operate_on(
                other.convert_to_number()?,
                |v1: ChInt, v2: ChInt| v1 + v2,
                |v1: ChFloat, v2: ChFloat| v1 + v2,
            )
            .into_type())
    }

    fn sub(self, other: ChType) -> Result<ChType, Error> {
        Ok(self
            .operate_on(
                other.convert_to_number()?,
                |v1: ChInt, v2: ChInt| v1 - v2,
                |v1: ChFloat, v2: ChFloat| v1 - v2,
            )
            .into_type())
    }

    fn mult(self, other: ChType) -> Result<ChType, Error> {
        Ok(self
            .operate_on(
                other.convert_to_number()?,
                |v1: ChInt, v2: ChInt| v1 * v2,
                |v1: ChFloat, v2: ChFloat| v1 * v2,
            )
            .into_type())
    }

    fn div(self, other: ChType) -> Result<ChType, Error> {
        if match other.clone().convert_to_number()? {
            NumberType::Int(v) => v == 0,
            NumberType::Float(v) => v == 0.0,
        } {
            Err(Error::new(
                ErrType::Runtime,
                &self.start_pos,
                &self.end_pos,
                String::from("Division by 0"),
                None,
            ))
        } else {
            Ok(self
                .operate_on(
                    other.convert_to_number()?,
                    |v1: ChInt, v2: ChInt| v1 / v2,
                    |v1: ChFloat, v2: ChFloat| v1 / v2,
                )
                .into_type())
        }
    }

    fn pow(mut self, other: ChType) -> Result<ChType, Error> {
        if match other.clone().convert_to_number()? {
            NumberType::Int(v) => v == 0,
            _ => false,
        } {
            self.value = 0.into_number_type();
            Ok(self.into_type())
        } else {
            Ok(self
                .operate_on(
                    other.convert_to_number()?,
                    |v1: ChInt, v2: ChInt| v1.pow(v2.try_into().unwrap_or(0)),
                    |v1: ChFloat, v2: ChFloat| v1.powf(v2),
                )
                .into_type())
        }
    }

    fn equal(self, other: ChType) -> Result<ChType, Error> {
        let value = other.convert_to_number();

        Ok(ChBool {
            value: match value {
                Ok(v) => match (self.value, v) {
                    (NumberType::Int(v1), NumberType::Int(v2)) => v1 == v2,
                    (NumberType::Float(v1), NumberType::Float(v2)) => v1 == v2,
                    (NumberType::Int(v1), NumberType::Float(v2)) => v1 as ChFloat == v2,
                    (NumberType::Float(v1), NumberType::Int(v2)) => v1 == v2 as ChFloat,
                },
                Err(_) => false,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn not_equal(self, other: ChType) -> Result<ChType, Error> {
        let value = other.convert_to_number();

        Ok(ChBool {
            value: match value {
                Ok(v) => match (self.value, v) {
                    (NumberType::Int(v1), NumberType::Int(v2)) => v1 != v2,
                    (NumberType::Float(v1), NumberType::Float(v2)) => v1 != v2,
                    (NumberType::Int(v1), NumberType::Float(v2)) => v1 as ChFloat != v2,
                    (NumberType::Float(v1), NumberType::Int(v2)) => v1 != v2 as ChFloat,
                },
                Err(_) => false,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn less(self, other: ChType) -> Result<ChType, Error> {
        let value = other.convert_to_number()?;

        Ok(ChBool {
            value: match (self.value, value) {
                (NumberType::Int(v1), NumberType::Int(v2)) => v1 < v2,
                (NumberType::Float(v1), NumberType::Float(v2)) => v1 < v2,
                (NumberType::Int(v1), NumberType::Float(v2)) => (v1 as ChFloat) < v2,
                (NumberType::Float(v1), NumberType::Int(v2)) => v1 < v2 as ChFloat,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn less_equal(self, other: ChType) -> Result<ChType, Error> {
        let value = other.convert_to_number()?;

        Ok(ChBool {
            value: match (self.value, value) {
                (NumberType::Int(v1), NumberType::Int(v2)) => v1 <= v2,
                (NumberType::Float(v1), NumberType::Float(v2)) => v1 <= v2,
                (NumberType::Int(v1), NumberType::Float(v2)) => (v1 as ChFloat) <= v2,
                (NumberType::Float(v1), NumberType::Int(v2)) => v1 <= v2 as ChFloat,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn greater(self, other: ChType) -> Result<ChType, Error> {
        let value = other.convert_to_number()?;

        Ok(ChBool {
            value: match (self.value, value) {
                (NumberType::Int(v1), NumberType::Int(v2)) => v1 > v2,
                (NumberType::Float(v1), NumberType::Float(v2)) => v1 > v2,
                (NumberType::Int(v1), NumberType::Float(v2)) => (v1 as ChFloat) > v2,
                (NumberType::Float(v1), NumberType::Int(v2)) => v1 > v2 as ChFloat,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn greater_equal(self, other: ChType) -> Result<ChType, Error> {
        let value = other.convert_to_number()?;

        Ok(ChBool {
            value: match (self.value, value) {
                (NumberType::Int(v1), NumberType::Int(v2)) => v1 >= v2,
                (NumberType::Float(v1), NumberType::Float(v2)) => v1 >= v2,
                (NumberType::Int(v1), NumberType::Float(v2)) => (v1 as ChFloat) >= v2,
                (NumberType::Float(v1), NumberType::Int(v2)) => v1 >= v2 as ChFloat,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn and(self, other: ChType) -> Result<ChType, Error> {
        let value = other.convert_to_number()?;

        Ok(ChBool {
            value: match (self.value, value) {
                (NumberType::Int(v1), NumberType::Int(v2)) => v1 >= 1 && v2 >= 1,
                (NumberType::Float(v1), NumberType::Float(v2)) => v1 >= 1.0 && v2 >= 1.0,
                (NumberType::Int(v1), NumberType::Float(v2)) => v1 >= 1 && v2 >= 1.0,
                (NumberType::Float(v1), NumberType::Int(v2)) => v1 >= 1.0 && v2 >= 1,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn or(self, other: ChType) -> Result<ChType, Error> {
        let value = other.convert_to_number()?;

        Ok(ChBool {
            value: match (self.value, value) {
                (NumberType::Int(v1), NumberType::Int(v2)) => v1 >= 1 || v2 >= 1,
                (NumberType::Float(v1), NumberType::Float(v2)) => v1 >= 1.0 || v2 >= 1.0,
                (NumberType::Int(v1), NumberType::Float(v2)) => v1 >= 1 || v2 >= 1.0,
                (NumberType::Float(v1), NumberType::Int(v2)) => v1 >= 1.0 || v2 >= 1,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn not(mut self) -> Result<ChType, Error> {
        self.value = match self.value {
            NumberType::Int(value) => if value != 0 { 0 } else { 1 }.into_number_type(),
            NumberType::Float(value) => if value != 0.0 { 0.0 } else { 1.0 }.into_number_type(),
        };
        Ok(ChBool {
            value: self.is_true(),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn is_true(&self) -> bool {
        match self.value {
            NumberType::Int(value) => value != 0,
            NumberType::Float(value) => value != 0.0,
        }
    }

    fn negate(mut self) -> Result<ChType, Error> {
        match self.value {
            NumberType::Int(v) => self.value = NumberType::Int(-v),
            NumberType::Float(v) => self.value = NumberType::Float(-v),
        }
        Ok(self.into_type())
    }
}

//#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone)]
pub struct ChString {
    string: String,
    start_pos: Position,
    end_pos: Position,
}

impl Display for ChString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.string)
    }
}

impl HasPosition for ChString {
    fn get_start(&self) -> Position {
        self.start_pos.clone()
    }

    fn get_end(&self) -> Position {
        self.end_pos.clone()
    }

    fn set_position(&mut self, start_pos: Position, end_pos: Position) {
        self.start_pos = start_pos;
        self.end_pos = end_pos;
    }
}

impl HasContext for ChString {}

impl ChOperators for ChString {
    fn is_true(&self) -> bool {
        !self.string.is_empty()
    }

    fn add(mut self, other: ChType) -> Result<ChType, Error> {
        let other_string = match other {
            ChType::Number(n) => format!("{}", n),
            ChType::String(s) => s.to_string(),
            _ => {
                return Err(Error::new(
                    ErrType::Runtime,
                    &self.start_pos,
                    &self.end_pos,
                    format!("can't add {:?} to String", other),
                    None,
                ))
            }
        };

        self.string += &other_string;
        Ok(ChType::String(self))
    }

    fn mult(mut self, other: ChType) -> Result<ChType, Error>
    {
        let n = other.into_number_type();
        
        match n {
            NumberType::Int(v) => {
                self.string = self.string.repeat(v.try_into().unwrap());
                Ok(self.into_type())
            },
            _ => Err(Error::new(
                    ErrType::UndefinedOperator,
                    &self.start_pos,
                    &self.end_pos,
                    String::from("multiply is only defined for type Int"),
                    None)),
        }
    }
}

impl ConvertType for ChString {}

impl IsChValue for ChString {
    fn into_type(self) -> ChType {
        ChType::String(self)
    }

    fn get_desc(&self) -> String {
        String::from("String")
    }
}

#[derive(Debug, Clone)]
pub enum ChType {
    Number(ChNumber),
    String(ChString),
    Function(Box<ChFunction>),
    Bool(ChBool),
    None(ChNone),
}

impl HasContext for ChType {
    fn set_context(&mut self, context: Rc<RefCell<Context>>) {
        match self {
            ChType::Function(func) => func.set_context(context),
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
pub struct Context {
    pub display_name: String,
    pub parent: Option<Rc<RefCell<Context>>>,
    pub position: Option<Position>,
    pub symbol_table: SymbolTable,
}

impl Context {
    fn empty(display_name: String) -> Self {
        Context {
            display_name,
            parent: None,
            position: None,
            symbol_table: SymbolTable::default(),
        }
    }

    fn from_parent(
        display_name: String,
        parent: Rc<RefCell<Context>>,
        position: Position,
    ) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Context {
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

    fn get(&self, key: &str) -> Option<ChType> {
        match self.symbol_table.get(key) {
            Some(v) => Some(v),
            None => match &self.parent {
                Some(p) => p.borrow().get(key),
                None => None,
            },
        }
    }

    fn set_mut(&mut self, key: &str, value: ChType) -> bool {
        self.symbol_table.set_mut(key, value)
    }

    fn set(&mut self, key: &str, value: ChType) -> bool {
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

fn visit_node(node: &mut Node, context: &mut Rc<RefCell<Context>>) -> Result<ChType, Error> {
    match node {
        Node::Num(token) => visit_numb_node(token, context),
        Node::String(token) => visit_string_node(token, context),
        Node::UnryOp(op, node) => visit_unryop_node(op, node, context),
        Node::BinOp(left, op, right) => visit_binop_node(left, op, right, context),
        Node::Access(id) => visit_access_node(id, context),
        Node::Assign(id, value) => visit_assign_node(id, value, context),
        Node::If(cases, else_case) => visit_if_node(cases, else_case, context),
        Node::While(cond, body, start, end) => visit_while_node(cond, body, context, start, end),
        Node::For(c1, c2, c3, body, start, end) => {
            visit_for_node(c1, c2, c3, body, context, start, end)
        }
        Node::FuncDef(name, args, body, start, end) => {
            visit_funcdef_node(name, args, body, start, end, context)
        }
        Node::Call(name, args) => visit_call_node(name, args, context),
    }
}

fn visit_numb_node(
    token: &mut Token,
    _context: &mut Rc<RefCell<Context>>,
) -> Result<ChType, Error> {
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

fn visit_string_node(
    token: &mut Token,
    _context: &mut Rc<RefCell<Context>>,
) -> Result<ChType, Error> {
    match &token.token_type {
        TokenType::String(s) => Ok(ChType::String(ChString {
            string: s.to_string(),
            start_pos: token.start_pos.clone(),
            end_pos: token.end_pos.clone(),
        })),
        _ => panic!("called visit_string_node on a string node that has a non string token"),
    }
}

fn visit_access_node(
    token: &mut Token,
    context: &mut Rc<RefCell<Context>>,
) -> Result<ChType, Error> {
    let var = &token.token_type;
    match var {
        TokenType::Id(var_name) => {
            let mut entry = context.borrow().get(var_name);

            match &mut entry {
                Some(num) => {
                    num.set_position(token.start_pos.clone(), token.end_pos.clone());
                    num.set_context(context.clone());
                    Ok(num.clone())
                }
                None => Err(Error::new(
                    ErrType::Runtime,
                    &token.start_pos,
                    &token.end_pos,
                    format!("{:?} is not defined", var_name),
                    Some(context.clone()),
                )),
            }
        }
        _ => panic!("called visit_access_node on a non ID token"),
    }
}

fn visit_assign_node(
    id: &mut Token,
    value: &mut Node,
    context: &mut Rc<RefCell<Context>>,
) -> Result<ChType, Error> {
    let t = id.clone();
    let ch_type = visit_node(value, context)?;

    match t.token_type {
        TokenType::Id(var_name) => {
            if !context.borrow_mut().set_mut(&var_name, ch_type.clone()) {
                return Err(Error::new(
                    ErrType::Runtime,
                    &ch_type.get_start(),
                    &ch_type.get_end(),
                    format!("cannot assign {} to const {:?}", ch_type, var_name),
                    Some(context.clone()),
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
    context: &mut Rc<RefCell<Context>>,
) -> Result<ChType, Error> {
    let mut ch_type = visit_node(node, context)?;
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
    context: &mut Rc<RefCell<Context>>,
) -> Result<ChType, Error> {
    if matches!(op.token_type, TokenType::AddEq) || matches!(op.token_type, TokenType::SubEq) {
        return in_de_crement(left, op, right, context);
    }
    let mut left = visit_node(left, context)?;
    let right = visit_node(right, context)?;

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
            e.set_context(context.clone());
            Err(e)
        }
    }
}

fn in_de_crement(
    left_node: &mut Node,
    op: &mut Token,
    right_node: &mut Node,
    context: &mut Rc<RefCell<Context>>,
) -> Result<ChType, Error> {
    let mut left = visit_node(left_node, context)?;
    let right = visit_node(right_node, context)?;
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

                    context.borrow_mut().set_mut(name, res.clone());
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

                    context.borrow_mut().set_mut(name, res.clone());
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
            Some(context.clone()),
        )),
    }
}

fn visit_if_node(
    cases: &mut Vec<(Node, Node)>,
    else_case: &mut Option<Box<Node>>,
    context: &mut Rc<RefCell<Context>>,
) -> Result<ChType, Error> {
    let mut start = Position::default();
    let mut end = Position::default();
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
    context: &mut Rc<RefCell<Context>>,
    start: &mut Position,
    end: &mut Position,
) -> Result<ChType, Error> {
    let mut n_context = Context::from_parent(String::from("<for>"), context.clone(), start.clone());

    if let Some(c) = c1 {
        visit_node(c, &mut n_context)?;
    }

    while visit_node(c2, &mut n_context)?.is_true() {
        visit_node(body, &mut n_context)?;
        if let Some(c) = c3 {
            visit_node(c, &mut n_context)?;
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
    context: &mut Rc<RefCell<Context>>,
    start: &mut Position,
    end: &mut Position,
) -> Result<ChType, Error> {
    let mut n_context =
        Context::from_parent(String::from("<while>"), context.clone(), start.clone());

    while visit_node(condition, context)?.is_true() {
        visit_node(body, &mut n_context)?;
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
    context: &mut Rc<RefCell<Context>>,
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
                    Some(context.clone()),
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
            context: context.clone(),
        }),
    }));

    if func_name.is_some() {
        context.borrow_mut().set_mut(&name, func.clone());
    }

    Ok(func)
}

fn visit_call_node(
    func_name: &mut Node,
    args: &mut Vec<Node>,
    context: &mut Rc<RefCell<Context>>,
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

    let c = visit_node(func_name, context)?;

    let mut call = match c {
        ChType::Function(func) => func,
        _ => {
            return Err(Error::new(
                ErrType::Runtime,
                &c.get_start(),
                &c.get_end(),
                format!("expected FUNCTION found {}", c),
                Some(context.clone()),
            ))
        }
    };

    let mut arg_values: Vec<ChType> = Vec::new();

    for arg in args {
        arg_values.push(visit_node(arg, context)?);
    }

    call.set_context(context.clone());
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
    pub global_context: Rc<RefCell<Context>>,
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
            global_context: Rc::new(RefCell::new(Context {
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

        visit_node(&mut ast, &mut self.global_context)
    }
}
