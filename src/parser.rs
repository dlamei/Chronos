use crate::lexer::*;

pub trait PeekableIterator: std::iter::Iterator {
    fn peek(&mut self) -> Option<&Self::Item>;
}

impl<I: std::iter::Iterator> PeekableIterator for std::iter::Peekable<I> {
    fn peek(&mut self) -> Option<&Self::Item> {
        std::iter::Peekable::peek(self)
    }
}

#[derive(Debug, Clone)]
pub enum NodeType {
    IntLiteral(i32),
    FloatLiteral(f32),
    StringLiteral(String),

    Add(Box<NodeType>, Box<NodeType>),
    Min(Box<NodeType>, Box<NodeType>),
    Mul(Box<NodeType>, Box<NodeType>),
    Div(Box<NodeType>, Box<NodeType>),

    Error,
}

pub struct Node {
    pub typ: NodeType,
    pub range: Position,
}

fn apply_op(op: TokenType, lhs: Node, rhs: Node) -> Node {
    Node {
        typ: match op {
            TokenType::Add => NodeType::Add(lhs.typ.into(), rhs.typ.into()),
            TokenType::Min => NodeType::Min(lhs.typ.into(), rhs.typ.into()),
            TokenType::Mul => NodeType::Mul(lhs.typ.into(), rhs.typ.into()),
            TokenType::Div => NodeType::Div(lhs.typ.into(), rhs.typ.into()),
            _ => todo!(),
        },

        range: (lhs.range.start..rhs.range.end),
    }
}

fn atom<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    if let Some(tok) = iter.next() {
        Node {
            typ: match tok.typ {
                TokenType::IntLiteral(val) => NodeType::IntLiteral(val),
                TokenType::FloatLiteral(val) => NodeType::FloatLiteral(val),
                TokenType::StringLiteral(val) => NodeType::StringLiteral(val),
                TokenType::Error => NodeType::Error,
                _ => todo!(),
            },
            range: tok.range,
        }
    } else {
        todo!()
    }
}

fn parse_expression<I>(iter: &mut I, mut lhs: Node, precedence: u32) -> Node
where
    I: PeekableIterator<Item = Token>, // I: Iterator<Item = Token>,
{
    let mut lookahead = iter.peek().unwrap().typ.clone();

    println!("lookahead: {:?}", lookahead);

    // TODO: check if operator
    while lookahead.get_precedence() > precedence {
        let op = lookahead;
        println!("op: {:?}", op);

        iter.next();
        let mut rhs = atom(iter);
        println!("rhs: {:?}", rhs.typ);
        lookahead = iter.peek().unwrap().typ.clone();
        println!("lookahead: {:?}", lookahead);

        while lookahead.get_precedence() > op.get_precedence() {
            rhs = parse_expression(iter, rhs, op.get_precedence());
            println!("rhs: {:?}", rhs.typ);
            lookahead = iter.peek().unwrap().typ.clone();
            println!("lookahead: {:?}", lookahead);
        }

        lhs = apply_op(op, lhs, rhs);
        println!("lhs: {:?}", lhs.typ);
    }

    return lhs;
}

pub fn parse_tokens(tokens: Vec<Token>) {
    if tokens.is_empty() {
        return;
    }

    let mut iter = tokens.into_iter().peekable();

    let lhs = atom(&mut iter);
    parse_expression(&mut iter, lhs, 0);
}
