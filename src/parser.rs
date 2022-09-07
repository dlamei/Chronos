use crate::{error, lexer::*};
use bitflags::bitflags;
use paste::paste;
use std::fmt;

macro_rules! unwrap_ret {
    ($e:expr) => {{
        let n = $e;
        if let Some(err) = n {
            return err;
        }
    }};
}

macro_rules! prop_node {
    ($e:expr $(,$func:ident)?) => {{
        let n = $e;
        if let NodeType::Error(_) = n.typ {
            return n;
        }
        n
    }};
}

// macro_rules! get_option {
//     ($e:expr, $val: ident, $res:expr) => {{
//         if let Some($val) = $e {
//             Some($res)
//         } else {
//             None
//         }
//     }};
// }

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
    I32Lit(i32),
    F32Lit(f32),
    StringLit(String),

    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),

    UnryAdd(Box<Node>),
    UnryMin(Box<Node>),

    Equal(Box<Node>, Box<Node>),
    Greater(Box<Node>, Box<Node>),
    GreaterEq(Box<Node>, Box<Node>),
    Less(Box<Node>, Box<Node>),
    LessEq(Box<Node>, Box<Node>),

    Error(String),
}

macro_rules! token_to_node {
    ($tok:expr, $panic:expr, $($lhs:pat => $rhs:expr)*) => {
        match $tok {
            $(paste!(TokenType::$lhs) => paste!(NodeType::$rhs),)*
            _ => $panic,
        }
    }
}

bitflags! {
    pub struct NodeFlags: u32 {
        const ERROR = 0b00000001;
    }
}

#[derive(Clone)]
pub struct Node {
    pub typ: NodeType,
    pub range: Position,
    pub flags: NodeFlags,
}

impl Node {
    fn new(typ: NodeType, range: Position) -> Self {
        Self {
            typ,
            range,
            flags: NodeFlags::empty(),
        }
    }

    fn error(msg: &str, range: Position) -> Self {
        Self {
            typ: NodeType::Error(msg.to_string()),
            range,
            flags: NodeFlags::ERROR,
        }
    }
}

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{:?}, {:?}]{:?}", self.range, self.flags, self.typ)
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use NodeType::*;
        match &self.typ {
            I32Lit(v) => write!(f, "{}", v),
            F32Lit(v) => write!(f, "{}", v),
            StringLit(v) => write!(f, "{}", v),

            Add(lhs, rhs) => write!(f, "({} + {})", lhs, rhs),
            Sub(lhs, rhs) => write!(f, "({} - {})", lhs, rhs),
            Mul(lhs, rhs) => write!(f, "({} * {})", lhs, rhs),
            Div(lhs, rhs) => write!(f, "({} / {})", lhs, rhs),

            UnryAdd(node) => write!(f, "(+ {})", node),
            UnryMin(node) => write!(f, "(- {})", node),

            Equal(lhs, rhs) => write!(f, "({} == {})", lhs, rhs),
            Greater(lhs, rhs) => write!(f, "({} > {})", lhs, rhs),
            GreaterEq(lhs, rhs) => write!(f, "({} >= {})", lhs, rhs),
            Less(lhs, rhs) => write!(f, "({} < {})", lhs, rhs),
            LessEq(lhs, rhs) => write!(f, "({} <= {})", lhs, rhs),
            Error(msg) => write!(f, "Err: {}", msg),
        }
    }
}

fn apply_op(op: TokenType, lhs: Node, rhs: Node) -> Node {
    let start = lhs.range.start;
    let end = rhs.range.end;

    let l_flags = lhs.flags;
    let r_flags = rhs.flags;

    Node {
        typ: token_to_node!(op, panic!("apply_op for {:?} not implemented", op),
            Add => Add(lhs.into(), rhs.into())
            Sub => Sub(lhs.into(), rhs.into())
            Mul => Mul(lhs.into(), rhs.into())
            Div => Div(lhs.into(), rhs.into())

            Equal => Equal(lhs.into(), rhs.into())
            Greater => Greater(lhs.into(), rhs.into())
            GreaterEq => GreaterEq(lhs.into(), rhs.into())
            Less => Less(lhs.into(), rhs.into())
            LessEq => LessEq(lhs.into(), rhs.into())
        ),
        range: (start..end),
        flags: l_flags | r_flags,
    }
}

fn expect_tok<I>(iter: &mut I, typs: Vec<TokenType>) -> Option<Node>
where
    I: PeekableIterator<Item = Token>,
{
    if let Some(tok) = iter.next() {
        for typ in &typs {
            if &tok.typ == typ {
                return None;
            }
        }

        Some(Node {
            typ: NodeType::Error(format!("Expected: {:?}, found {:?}", typs, tok.typ)),
            range: tok.range,
            flags: NodeFlags::ERROR,
        })
        // None
        // if matches!(tok.typ, typ.clon) {
        // if {
        //     for typ in typs
        // }
        //     None
        // } else {
        //     Some(Node {
        //         typ: NodeType::Error(format!("Expected: {:?}, found {:?}", typ, tok.typ)),
        //         range: tok.range,
        //         flags: NodeFlags::ERROR,
        //     })
        // }
    } else {
        panic!();
    }
}

fn parse_paren<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    unwrap_ret!(expect_tok(iter, vec!(TokenType::LParen)));

    let node = prop_node!(parse_expression(iter));
    // let lhs = atom(iter);
    // let node = prop_node!(parse_sub_expression(iter, lhs, -1));

    unwrap_ret!(expect_tok(iter, vec!(TokenType::RParen)));
    node
}

fn parse_lit<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    let tok = iter.next().unwrap();
    Node {
        typ: token_to_node!(tok.typ, panic!("expected literal, found: {:?}", tok.typ),
            I32Lit(val) => I32Lit(val)
            F32Lit(val) => F32Lit(val)
            StringLit(val) => StringLit(val)
        ),
        range: tok.range,
        flags: NodeFlags::empty(),
    }
}

fn parse_unry<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    use TokenType::*;
    let op = iter.peek().unwrap().clone();

    unwrap_ret!(expect_tok(iter, vec!(TokenType::Add, TokenType::Sub)));
    let node = parse_expression(iter);
    let range = op.range.start..node.range.end;

    Node::new(
        match op.typ {
            Add => NodeType::UnryAdd(node.into()),
            Sub => NodeType::UnryMin(node.into()),
            _ => panic!("expected Add or Sub, found: {:?}", op),
        },
        range,
    )
}

fn atom<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    use TokenType::*;
    if let Some(tok) = iter.peek() {
        match tok.typ {
            LParen => parse_paren(iter),
            I32Lit(_) | F32Lit(_) | StringLit(_) => parse_lit(iter),
            Add | Sub => parse_unry(iter),

            Error => {
                let tok = iter.next().unwrap();
                Node::error("Could not lex token", tok.range)
            }
            _ => {
                let tok = iter.next().unwrap();
                Node::error(
                    &format!("Expected expression, found [{:?}]", tok.typ),
                    tok.range,
                )
            }
        }
    } else {
        todo!()
    }
}

fn parse_sub_expression<I>(iter: &mut I, mut lhs: Node, precedence: i32) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    if iter.peek().is_none() || iter.peek().unwrap().typ == TokenType::Eof {
        return lhs;
    }

    if iter.peek().is_none() {
        return lhs;
    }
    let mut lookahead = iter.peek().unwrap().typ.clone();
    println!("lookahead: {:?}", lookahead);

    while lookahead.is_op() && lookahead.precedence() > precedence {
        let op = lookahead;
        println!("\top: {:?}", op);

        iter.next();
        if iter.peek().is_none() {
            return lhs;
        }

        let mut rhs = atom(iter);
        println!("\tnext rhs: {:?}", rhs);

        if iter.peek().is_none() {
            lhs = apply_op(op, lhs, rhs);
            break;
        }
        lookahead = iter.peek().unwrap().typ.clone();
        println!("\tlookahead: {:?}", lookahead);

        while lookahead.is_op() && lookahead.precedence() > op.precedence() {
            rhs = parse_sub_expression(iter, rhs, op.precedence());
            println!("\t\trhs: {:?}", rhs.typ);

            if iter.peek().is_none() {
                break;
            }
            lookahead = iter.peek().unwrap().typ.clone();
            println!("\t\tlookahead: {:?}", lookahead);
        }

        lhs = apply_op(op, lhs, rhs);
        println!("\tlhs: {:?}", lhs.typ);
    }

    lhs
}

fn parse_expression<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    let lhs = atom(iter);
    parse_sub_expression(iter, lhs, -1)
}

pub fn parse_tokens(tokens: Vec<Token>) -> Node {
    if tokens.is_empty() {
        return Node {
            typ: NodeType::Error("No Tokens found".to_string()),
            range: (0..0),
            flags: NodeFlags::ERROR,
        };
    }

    let mut iter = tokens.into_iter().peekable();

    parse_expression(&mut iter)
    // let lhs = atom(&mut iter);
    // parse_sub_expression(&mut iter, lhs, -1)
}

pub fn print_errors(n: &Node, code: &str) {
    use NodeType::*;
    match &n.typ {
        I32Lit(_) | F32Lit(_) | StringLit(_) => {}

        Add(lhs, rhs) => {
            print_errors(&lhs, code);
            print_errors(&rhs, code);
        }
        Sub(lhs, rhs) => {
            print_errors(&lhs, code);
            print_errors(&rhs, code);
        }
        Mul(lhs, rhs) => {
            print_errors(&lhs, code);
            print_errors(&rhs, code);
        }
        Div(lhs, rhs) => {
            print_errors(&lhs, code);
            print_errors(&rhs, code);
        }

        UnryAdd(node) => print_errors(&node, code),
        UnryMin(node) => print_errors(&node, code),

        Equal(lhs, rhs) => {
            print_errors(&lhs, code);
            print_errors(&rhs, code);
        }
        Greater(lhs, rhs) => {
            print_errors(&lhs, code);
            print_errors(&rhs, code);
        }
        GreaterEq(lhs, rhs) => {
            print_errors(&lhs, code);
            print_errors(&rhs, code);
        }
        Less(lhs, rhs) => {
            print_errors(&lhs, code);
            print_errors(&rhs, code);
        }
        LessEq(lhs, rhs) => {
            print_errors(&lhs, code);
            print_errors(&rhs, code);
        }

        Error(msg) => {
            println!("Parser: {}", msg);
            println!("{}", error::underline_code(code, &n.range));
        } // _ => panic!("print_errors for {:?} not implemented", n.typ),
    }
}
