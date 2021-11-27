use crate::errors::*;

use std::fmt;

const DIGITS: &str = "0123456789";

fn match_tokens(t1: &TokenType, t2: &TokenType) -> bool {
    match (t1, t2) {
        (&TokenType::ADD, &TokenType::ADD)
        | (&TokenType::SUB, &TokenType::SUB)
        | (&TokenType::MUL, &TokenType::MUL)
        | (&TokenType::DIV, &TokenType::DIV)
        | (&TokenType::LPAREN, &TokenType::LPAREN)
        | (&TokenType::RPAREN, &TokenType::RPAREN) => true,
        (&TokenType::EOF, &TokenType::EOF) => true,
        _ => false,
    }
}

#[derive(Debug, Clone)]
pub struct Position {
    pub file_name: String,
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

impl Position {
    fn new(file_name: String, index: usize, line: usize, column: usize) -> Self {
        Position {
            file_name,
            index,
            line,
            column,
        }
    }

    fn advance(&mut self, current_char: &Option<char>) {
        match *current_char {
            Some('\n') => {
                self.line += 1;
                self.index += 1;
                self.column = 0;
            }

            Some(_) => {
                self.index += 1;
                self.column += 1;
            }
            _ => (),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TokenType {
    INT(i32),
    FLOAT(f32),
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

        Token {
            token_type,
            start_pos: start_pos.clone(),
            end_pos: start_pos.clone(),
        }
    }
}

#[derive(Debug)]
pub enum Node {
    NUM(Token),
    BINOP(Box<Node>, Token, Box<Node>),
}

pub fn run(file_name: String, text: String) -> Result<Node, Error> {
    let mut lexer = Lexer::new(file_name, text);
    let tokens = lexer.parse_tokens()?;

    let mut parser = Parser::new(tokens);
    parser.parse()
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
                    tokens.push(Token::new(TokenType::MUL, &self.position, None));
                    true
                }
                ')' => {
                    tokens.push(Token::new(TokenType::MUL, &self.position, None));
                    true
                }
                _ => false,
            } {
                self.advance();
            } else if DIGITS.contains(c) {
                tokens.push(self.make_number());
            } else {
                self.advance();
                return Err(Error::new(
                    ErrTypes::IllegalCharError,
                    &self.position,
                    &self.position,
                    format!("'{}'", c),
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
                TokenType::INT(num.parse::<i32>().unwrap()),
                &self.position,
                None,
            );
        }

        Token::new(
            TokenType::FLOAT(num.parse::<f32>().unwrap()),
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

        if !match_tokens(&self.current_token.token_type, &TokenType::EOF) {
            return Err(Error::new(
                ErrTypes::InvalidSyntaxError,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                "Expected '+', '-', '*' or '/'".to_string(),
            ));
        }

        Ok(nodes)
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
            _ => Err(Error::new(
                ErrTypes::InvalidSyntaxError,
                &t.start_pos,
                &t.end_pos,
                format!("Expected INT or FLOAT"),
            )),
        };
    }

    fn binary_operation(
        &mut self,
        func: fn(parser: &mut Parser) -> Result<Node, Error>,
        ops: (TokenType, TokenType),
    ) -> Result<Node, Error> {
        let mut left_node = self.factor()?;

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
