use crate::lexer::*;

pub enum NodeType {
    IntLiteral(i32),
    FloatLiteral(f32),
    StringLiteral(String),

    Error(),
}

type Node = (NodeType, std::ops::Range<usize>);

fn atom<'a, I>(iter: &mut I) -> NodeType
where
    I: DoubleEndedIterator<Item = &'a Token>,
{
    if let Some(tok) = iter.next() {
        match &tok.typ {
            TokenType::IntLiteral(val) => NodeType::IntLiteral(*val),
            TokenType::FloatLiteral(val) => NodeType::FloatLiteral(*val),
            TokenType::StringLiteral(val) => NodeType::StringLiteral(val.to_string()),
            _ => todo!(),
        }
    } else {
        todo!()
    }
}

pub fn parse_tokens(tokens: &Vec<Token>) {
    if tokens.len() == 0 {
        return;
    }

    let mut iter = tokens.iter();
    atom(&mut iter);
}
