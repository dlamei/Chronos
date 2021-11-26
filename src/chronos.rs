use std::{error::Error, fmt};

#[derive(Debug)]
pub struct IllegalCharError(Position, String);

impl fmt::Display for IllegalCharError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Illegal Character: {}\n File: {}, Line: {}", self.1, self.0.file_name, self.0.line)
    }
}

impl Error for IllegalCharError {}

const DIGITS: &str = "0123456789";

#[derive(Debug)]
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

#[derive(Debug)]
enum Node {
    NUMB(Token),
    BINOP(Token, Token)
}

#[derive(Debug, Clone)]
struct Position {
    file_name: String,
    index: usize,
    line: usize,
    column: usize,
}

impl Position {
    fn new(file_name: String, index: usize, line: usize, column: usize) -> Self {
        Position { file_name, index, line, column }
    }

    fn advance(&mut self, current_char: &Option<char>)
    {
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
            _ => ()
        }
    }
}

struct Lexer {
    text: Box<[u8]>,
    position: Position,
    current_char: Option<char>,
}

pub fn run(file_name: String, text: String) -> Result<Vec<Token>, IllegalCharError> {
    let mut lexer = Lexer::new(file_name, text);
    lexer.parse_tokens()
}

impl Lexer {
    pub fn new(file_name: String, text: String) -> Self {
        let mut l = Lexer {
            text: (text.as_bytes().into()),
            position: Position {file_name, index: 0, line: 0, column: 0},
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
