use crate::chronos::*;
use crate::errors::*;

use std::rc::Rc;

pub struct Lexer {
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

    pub fn parse_tokens(&mut self) -> Result<Vec<Token>, Error> {
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
                '[' => {
                    tokens.push(Token::new(TokenType::LBrace, self.position.clone(), None));
                    self.advance();
                    true
                }
                ']' => {
                    tokens.push(Token::new(TokenType::RBrace, self.position.clone(), None));
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
