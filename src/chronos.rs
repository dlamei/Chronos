use std::fmt;

use crate::errors::*;

const DIGITS: &str = "0123456789";
type ChInt = i32;
type ChFloat = f32;

fn match_tokens(t1: &TokenType, t2: &TokenType) -> bool {
    match (t1, t2) {
        (&TokenType::ADD, &TokenType::ADD)
        | (&TokenType::SUB, &TokenType::SUB)
        | (&TokenType::MUL, &TokenType::MUL)
        | (&TokenType::DIV, &TokenType::DIV)
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
pub enum TokenType {
    INT(ChInt),
    FLOAT(ChFloat),
    ADD,
    SUB,
    MUL,
    DIV,
    LPAREN,
    RPAREN,
    EOF,
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
                '(' => {
                    tokens.push(Token::new(TokenType::LPAREN, &self.position, None));
                    true
                }
                ')' => {
                    tokens.push(Token::new(TokenType::RPAREN, &self.position, None));
                    true
                }
                _ => false,
            } {
                self.advance();
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
                ));
            }
        }

        tokens.push(Token::new(TokenType::EOF, &self.position, None));
        Ok(tokens)
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

#[derive(Debug)]
pub enum Node {
    NUM(Token),
    BINOP(Box<Node>, Token, Box<Node>),
    UNRYOP(Token, Box<Node>),
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
                    "Parser expected '+', '-', '*' or '/' found {:?}",
                    self.current_token.token_type
                ),
            )),
        }
    }

    fn advance(&mut self) {
        self.token_index += 1;

        if self.token_index < self.tokens.len() {
            self.current_token = self.tokens[self.token_index].clone();
        }
    }

    fn factor(&mut self) -> Result<Node, Error> {
        let t = self.current_token.clone();

        return match t.token_type {
            TokenType::INT(_) | TokenType::FLOAT(_) => {
                self.advance();
                Ok(Node::NUM(t))
            }

            TokenType::SUB => {
                self.advance();
                let factor = self.factor()?;
                Ok(Node::UNRYOP(t, factor.into()))
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
                    )),
                }
            }
            _ => Err(Error::new(
                ErrType::InvalidSyntaxError,
                &t.start_pos,
                &t.end_pos,
                format!("Parser expected INT or FLOAT found {:?}", t.token_type),
            )),
        };
    }

    fn binary_operation(
        &mut self,
        func: fn(parser: &mut Parser) -> Result<Node, Error>,
        ops: (TokenType, TokenType),
    ) -> Result<Node, Error> {
        let mut left_node = func(self)?;

        while match_tokens(&self.current_token.token_type, &ops.0)
            || match_tokens(&self.current_token.token_type, &ops.1)
        {
            let op_token = self.current_token.clone();
            self.advance();
            let right_node = func(self)?;

            left_node = Node::BINOP(left_node.into(), op_token, right_node.into());
        }

        Ok(left_node)
    }

    fn term(&mut self) -> Result<Node, Error> {
        self.binary_operation(Parser::factor, (TokenType::MUL, TokenType::DIV))
    }

    fn expression(&mut self) -> Result<Node, Error> {
        self.binary_operation(Parser::term, (TokenType::ADD, TokenType::SUB))
    }
}

#[derive(Clone, Debug)]
pub enum NumberType {
    INT(ChInt),
    FLOAT(ChFloat),
}

#[derive(Debug)]
pub struct Number {
    value: NumberType,
    start_pos: Position,
    end_pos: Position,
}

trait AsNumberType {
    fn as_number_type(self) -> NumberType;
}

impl AsNumberType for ChInt {
    fn as_number_type(self) -> NumberType {
        NumberType::INT(self)
    }
}

impl AsNumberType for ChFloat {
    fn as_number_type(self) -> NumberType {
        NumberType::FLOAT(self)
    }
}

impl Number {
    fn set_position(&mut self, start_pos: Position, end_pos: Position) {
        self.start_pos = start_pos;
        self.end_pos = end_pos;
    }

    pub fn get_value<'a>(&'a self) -> &'a NumberType {
        &self.value
    }

    fn operate_on(
        mut self,
        other: Number,
        int_op: fn(ChInt, ChInt) -> ChInt,
        float_op: fn(ChFloat, ChFloat) -> ChFloat,
    ) -> Self {
        self.value = match (self.value, other.value) {
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

    fn add(self, other: Number) -> Self {
        self.operate_on(
            other,
            |v1: ChInt, v2: ChInt| v1 + v2,
            |v1: ChFloat, v2: ChFloat| v1 + v2,
        )
    }

    fn sub(self, other: Number) -> Self {
        self.operate_on(
            other,
            |v1: ChInt, v2: ChInt| v1 - v2,
            |v1: ChFloat, v2: ChFloat| v1 - v2,
        )
    }

    fn mult(self, other: Number) -> Self {
        self.operate_on(
            other,
            |v1: ChInt, v2: ChInt| v1 * v2,
            |v1: ChFloat, v2: ChFloat| v1 * v2,
        )
    }

    fn div(self, other: Number) -> Self {
        self.operate_on(
            other,
            |v1: ChInt, v2: ChInt| v1 / v2,
            |v1: ChFloat, v2: ChFloat| v1 / v2,
        )
    }
}

fn visit_node(node: Node) -> Result<Number, Error> {
    match node {
        Node::NUM(token) => visit_numb_node(token),
        Node::UNRYOP(op, node) => visit_unryop_node(op, *node),
        Node::BINOP(left, op, right) => visit_binop_node(*left, op, *right),
    }
}

fn visit_numb_node(token: Token) -> Result<Number, Error> {
    match token.token_type {
        TokenType::INT(value) => Ok(Number {
            value: value.as_number_type(),
            start_pos: token.start_pos,
            end_pos: token.end_pos,
        }),
        TokenType::FLOAT(value) => Ok(Number {
            value: value.as_number_type(),
            start_pos: token.start_pos,
            end_pos: token.end_pos,
        }),
        _ => panic!("called visit_numb_node on a number node that has a non number token"),
    }
}

fn visit_unryop_node(op: Token, node: Node) -> Result<Number, Error> {
    let mut number = visit_node(node)?;
    number.set_position(op.start_pos, number.end_pos.clone());

    match op.token_type {
        TokenType::SUB => Ok(number.mult(Number {
            value: NumberType::INT(-1),
            start_pos: Position::empty(),
            end_pos: Position::empty(),
        })),
        TokenType::ADD => Ok(number),

        _ => panic!("called visit_unryop_node on a binop node that has a non Operation token"),
    }
}

fn visit_binop_node(left: Node, op: Token, right: Node) -> Result<Number, Error> {
    let mut left = visit_node(left)?;
    let right = visit_node(right)?;
    left.set_position(left.start_pos.clone(), right.end_pos.clone());

    match op.token_type {
        TokenType::ADD => Ok(left.add(right)),
        TokenType::SUB => Ok(left.sub(right)),
        TokenType::MUL => Ok(left.mult(right)),
        TokenType::DIV => {
            if match right.value {
                NumberType::INT(v) => v == 0,
                NumberType::FLOAT(v) => v == 0.0,
            } {
                return Err(Error::new(
                    ErrType::RuntimeError,
                    &left.start_pos,
                    &right.end_pos,
                    String::from("Division by 0"),
                ));
            }
            Ok(left.div(right))
        }
        _ => panic!("called visit_binop_node on a binop node that has a non Operation token"),
    }
}

pub fn interpret(file_name: String, text: String) -> Result<Number, Error> {
    let mut lexer = Lexer::new(file_name, text);
    let tokens = lexer.parse_tokens()?;

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    visit_node(ast)
}
