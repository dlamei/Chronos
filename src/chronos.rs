const DIGITS: &str = "0123456789";

#[derive(Debug)]
pub enum Token 
{
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
struct Lexer 
{
    text: Box<[u8]>,
    pos: usize,
    current_char: Option<char>,
}

impl Lexer 
{
    pub fn new(text: String) -> Lexer
    {
        let mut l = Lexer{ text: (text.as_bytes().into()), pos: 0, current_char: None };
        l.current_char = Some(l.text[l.pos] as char);
        l
    }

    fn advance(self: &mut Lexer)
    {
        self.pos += 1;
        self.current_char = if self.pos < self.text.len() {
            Some(self.text[self.pos] as char)
        } else {
            None
        };
    }

    fn make_token(self: &mut Lexer)
    {
        let mut tokens: Vec<Token> = Vec::new();

        while self.current_char != None
        {
            let c = self.current_char.unwrap();

            if " \t".contains(c)
            {
                self.advance();
            }

            match c
            {
                '+' => { tokens.push(Token::ADD); self.advance() },
                '-' => { tokens.push(Token::SUB); self.advance() },
                '/' => { tokens.push(Token::DIV); self.advance() },
                '*' => { tokens.push(Token::MUL); self.advance() },
                '(' => { tokens.push(Token::MUL); self.advance() },
                ')' => { tokens.push(Token::MUL); self.advance() },
                _ => (),
            }

            if DIGITS.contains(c) { tokens.push(self.make_number()); }
        }

    }

    fn make_number(self: &mut Lexer) -> Token
    {
        let mut num: String = String::new();
        let mut dotCount: u8 = 0;

        let s = DIGITS.to_owned() + ".";

        while self.current_char != None && (s).contains(self.current_char.unwrap())
        {
            if self.current_char.unwrap() == '.'
            {
                if (dotCount >= 1) { break; }
                dotCount += 1;
                num += ".";
            }
        }

        Token::INT(10)
    }
}
