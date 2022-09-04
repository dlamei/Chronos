use crate::lexer::*;
use std::fmt;
use paste::paste;

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

    Add(Box<Node>, Box<Node>),
    Min(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),

    Equal(Box<Node>, Box<Node>),
    Greater(Box<Node>, Box<Node>),
    GreaterEq(Box<Node>, Box<Node>),
    Less(Box<Node>, Box<Node>),
    LessEq(Box<Node>, Box<Node>),

    Error,
}

macro_rules! token_to_node {
    ($tok:expr, $panic:expr, $($lhs:pat => $rhs:expr)*) => {
        match $tok {
            $(paste!(TokenType::$lhs) => paste!(NodeType::$rhs),)*
            _ => $panic,
        }
    }
}

#[derive(Clone)]
pub struct Node {
    pub typ: NodeType,
    pub range: Position,
}

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.typ)
    }
}

fn apply_op(op: TokenType, lhs: Node, rhs: Node) -> Node {
    let start = lhs.range.start;
    let end = lhs.range.end;

    Node {
        typ: token_to_node!(op, panic!("apply_op for {:?} not implemented", op),
            Add => Add(lhs.into(), rhs.into())
            Min => Min(lhs.into(), rhs.into())
            Mul => Mul(lhs.into(), rhs.into())
            Div => Div(lhs.into(), rhs.into())

            Equal => Equal(lhs.into(), rhs.into())
            Greater => Greater(lhs.into(), rhs.into())
            GreaterEq => GreaterEq(lhs.into(), rhs.into())
            Less => Less(lhs.into(), rhs.into())
            LessEq => LessEq(lhs.into(), rhs.into())
        ),
        range: (start..end),
    }
}

fn atom<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    if let Some(tok) = iter.next() {
        if tok.typ == TokenType::LParen {
            let lhs = atom(iter);
            let node = parse_expression(iter, lhs, -1);
            iter.next(); //TODO: error check
            return node;
        }

        Node {
            typ: token_to_node!(tok.typ, panic!("atom for {:?} not implemented", tok.typ),
                IntLiteral(val) => IntLiteral(val)
                FloatLiteral(val) => FloatLiteral(val)
                StringLiteral(val) => StringLiteral(val)
                Error => Error
            ),
            range: tok.range,
        }
    } else {
        todo!()
    }
}

fn parse_expression<I>(iter: &mut I, mut lhs: Node, precedence: i32) -> Node
where
    I: PeekableIterator<Item = Token>, // I: Iterator<Item = Token>,
{
    let mut lookahead = iter.peek().unwrap().typ.clone();

    println!("lookahead: {:?}", lookahead);

    // TODO: check if operator
    while lookahead.is_op() && lookahead.precedence() > precedence {
        let op = lookahead;
        println!("\top: {:?}", op);

        iter.next();
        let mut rhs = atom(iter);
        println!("\trhs: {:?}", rhs);
        lookahead = iter.peek().unwrap().typ.clone();
        println!("\tlookahead: {:?}", lookahead);

        while lookahead.precedence() > op.precedence() {
            rhs = parse_expression(iter, rhs, op.precedence());
            println!("\t\trhs: {:?}", rhs.typ);
            lookahead = iter.peek().unwrap().typ.clone();
            println!("\t\tlookahead: {:?}", lookahead);
        }

        lhs = apply_op(op, lhs, rhs);
        println!("\tlhs: {:?}", lhs.typ);
    }

    lhs
}

pub fn parse_tokens(tokens: Vec<Token>) -> Option<Node> {
    if tokens.is_empty() {
        return None;
    }

    let mut iter = tokens.into_iter().peekable();

    let lhs = atom(&mut iter);
    Some(parse_expression(&mut iter, lhs, -1))
}
