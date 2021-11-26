use crate::errors::*;

const DIGITS: &str = "0123456789";

#[derive(Debug, Clone)]
pub enum Token {
    INT(i32),
    FLOAT(f32),
    ADD,
    SUB,
    MUL,
    DIV,
    LPAREN,
    RPAREN,
}

fn match_tokens(t1: &Token, t2: &Token) -> bool {
    match (t1, t2) {
        (&Token::ADD, &Token::ADD)
        | (&Token::SUB, &Token::SUB)
        | (&Token::MUL, &Token::MUL)
        | (&Token::DIV, &Token::DIV)
        | (&Token::LPAREN, &Token::LPAREN)
        | (&Token::RPAREN, &Token::RPAREN) => true,
        _ => false,
    }
}

#[derive(Debug)]
pub enum Node {
    NUM(Token),
    BINOP(Box<Node>, Token, Box<Node>),
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

pub fn run(file_name: String, text: String) -> Result<Node, IllegalCharError> {
    let mut lexer = Lexer::new(file_name, text);
    let tokens = lexer.parse_tokens()?;

    let mut parser = Parser::new(tokens);
    Ok(parser.parse())
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

    fn parse_tokens(&mut self) -> Result<Vec<Token>, IllegalCharError> {
        let mut tokens: Vec<Token> = Vec::new();

        while self.current_char != None {
            let c = self.current_char.unwrap();

            if " \t\n".contains(c) {
                self.advance();
            } else if match c {
                '+' => {
                    tokens.push(Token::ADD);
                    true
                }
                '-' => {
                    tokens.push(Token::SUB);
                    true
                }
                '/' => {
                    tokens.push(Token::DIV);
                    true
                }
                '*' => {
                    tokens.push(Token::MUL);
                    true
                }
                '(' => {
                    tokens.push(Token::MUL);
                    true
                }
                ')' => {
                    tokens.push(Token::MUL);
                    true
                }
                _ => false,
            } {
                self.advance();
            } else if DIGITS.contains(c) {
                tokens.push(self.make_number());
            } else {
                self.advance();
                return Err(IllegalCharError(self.position.clone(), format!("'{}'", c)));
            }
        }

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
            return Token::INT(num.parse::<i32>().unwrap());
        }
        Token::FLOAT(num.parse::<f32>().unwrap())
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

    fn parse(&mut self) -> Node {
        self.expression()
    }

    fn advance(&mut self) {
        self.token_index += 1;

        if self.token_index < self.tokens.len() {
            self.current_token = self.tokens[self.token_index].clone();
        }
    }

    fn factor(&mut self) -> Node {
        let t = self.current_token.clone();

        return match t {
            Token::INT(_) | Token::FLOAT(_) => {
                self.advance();
                Node::NUM(t)
            }
            _ => panic!("test"),
        };
    }

    fn binary_operation(
        &mut self,
        func: fn(parser: &mut Parser) -> Node,
        ops: (Token, Token),
    ) -> Node {
        let mut left_node = self.factor();

        while match_tokens(&self.current_token, &ops.0) || match_tokens(&self.current_token, &ops.1)
        {
            let op_token = self.current_token.clone();
            self.advance();
            let right_node = func(self);

            left_node = Node::BINOP(left_node.into(), op_token, right_node.into());
        }

        left_node
    }

    fn term(&mut self) -> Node {
        self.binary_operation(Parser::factor, (Token::MUL, Token::DIV))
    }

    fn expression(&mut self) -> Node {
        self.binary_operation(Parser::term, (Token::ADD, Token::SUB))
    }
}
