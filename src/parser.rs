use crate::lexer::*;

pub enum NodeType {
    IntLiteral(i32),
    FloatLiteral(f32),
    StringLiteral(String),

    Error(),
}

struct Node {
    typ: NodeType,
    range: Position,
}

fn atom<I>(mut iter: I) -> Node
where
    I: DoubleEndedIterator<Item = Token>,
{
    if let Some(tok) = iter.next() {
        Node {
            typ: match tok.typ {
                TokenType::IntLiteral(val) => NodeType::IntLiteral(val),
                TokenType::FloatLiteral(val) => NodeType::FloatLiteral(val),
                TokenType::StringLiteral(val) => NodeType::StringLiteral(val),
                _ => todo!(),
            },
            range: tok.range,
        }
    } else {
        todo!()
    }
}

pub fn parse_tokens(tokens: Vec<Token>) {
    if tokens.len() == 0 {
        return;
    }

    let iter = tokens.into_iter();
    atom(iter);
}
