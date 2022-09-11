use crate::{
    error,
    lexer::{Position, Token, TokenType},
};
use bitflags::bitflags;
use paste::paste;
use std::{collections::LinkedList, fmt};

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
    BoolLit(bool),
    I32Lit(i32),
    F32Lit(f32),
    StringLit(String),

    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),

    UnryAdd(Box<Node>),
    UnryMin(Box<Node>),
    UnryNot(Box<Node>),

    Equal(Box<Node>, Box<Node>),
    NotEqual(Box<Node>, Box<Node>),
    Greater(Box<Node>, Box<Node>),
    GreaterEq(Box<Node>, Box<Node>),
    Less(Box<Node>, Box<Node>),
    LessEq(Box<Node>, Box<Node>),

    Assign(Box<Node>, Box<Node>),
    Id(String),

    Error(String),

    Expresssion(LinkedList<Node>, bool), //return last
    Eval(Box<Node>),
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
            typ: NodeType::Error(msg.to_owned()),
            range,
            flags: NodeFlags::ERROR,
        }
    }
}

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // write!(f, "[{:?}, {:?}]{:?}", self.range, self.flags, self.typ)
        write!(f, "[{:?}]{:?}", self.range, self.typ)
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use NodeType::*;
        match &self.typ {
            BoolLit(v) => write!(f, "{}", v),
            I32Lit(v) => write!(f, "{}", v),
            F32Lit(v) => write!(f, "{}", v),
            StringLit(v) => write!(f, "{}", v),

            Add(lhs, rhs) => write!(f, "({} + {})", lhs, rhs),
            Sub(lhs, rhs) => write!(f, "({} - {})", lhs, rhs),
            Mul(lhs, rhs) => write!(f, "({} * {})", lhs, rhs),
            Div(lhs, rhs) => write!(f, "({} / {})", lhs, rhs),

            UnryAdd(node) => write!(f, "(+ {})", node),
            UnryMin(node) => write!(f, "(- {})", node),
            UnryNot(node) => write!(f, "(! {})", node),

            Equal(lhs, rhs) => write!(f, "({} == {})", lhs, rhs),
            NotEqual(lhs, rhs) => write!(f, "({} != {})", lhs, rhs),
            Greater(lhs, rhs) => write!(f, "({} > {})", lhs, rhs),
            GreaterEq(lhs, rhs) => write!(f, "({} >= {})", lhs, rhs),
            Less(lhs, rhs) => write!(f, "({} < {})", lhs, rhs),
            LessEq(lhs, rhs) => write!(f, "({} <= {})", lhs, rhs),

            Assign(lhs, rhs) => write!(f, "({} = {})", lhs, rhs),
            Id(v) => write!(f, "{}", v),

            Error(msg) => write!(f, "Err: {}", msg),

            Expresssion(expr, last_ret) => {
                let mut res = String::new();
                let mut it = expr.iter().peekable();

                while let Some(e) = it.next() {
                    if it.peek().is_some() || !last_ret {
                        res.push_str(&format!("{};", e));
                    } else {
                        res.push_str(&format!("{}", e));
                    }
                }
                write!(f, "{{{}}}", res)
            }
            Eval(expr) => write!(f, "{:?}()", expr),
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
            NotEqual => NotEqual(lhs.into(), rhs.into())
            Greater => Greater(lhs.into(), rhs.into())
            GreaterEq => GreaterEq(lhs.into(), rhs.into())
            Less => Less(lhs.into(), rhs.into())
            LessEq => LessEq(lhs.into(), rhs.into())

            Assign => Assign(lhs.into(), rhs.into())
        ),
        range: (start..end),
        flags: l_flags | r_flags,
    }
}

fn expect_tok_peek<I>(iter: &mut I, typs: Vec<TokenType>) -> Option<Node>
where
    I: PeekableIterator<Item = Token>,
{
    if let Some(tok) = iter.peek() {
        for typ in &typs {
            if &tok.typ == typ {
                return None;
            }
        }

        Some(Node {
            typ: NodeType::Error(format!("Expected: {:?}, found {:?}", typs, tok.typ)),
            range: tok.range.clone(),
            flags: NodeFlags::ERROR,
        })
    } else {
        Some(Node {
            typ: NodeType::Error(format!("Expected: {:?}, found NONE", typs)),
            range: 0..0,
            flags: NodeFlags::ERROR,
        })
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
    } else {
        Some(Node {
            typ: NodeType::Error(format!("Expected: {:?}, found NONE", typs)),
            range: 0..0,
            flags: NodeFlags::ERROR,
        })
    }
}

fn parse_paren<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    unwrap_ret!(expect_tok(iter, vec!(TokenType::LParen)));

    let node = prop_node!(parse_expression(iter));

    unwrap_ret!(expect_tok(iter, vec!(TokenType::RParen)));
    node
}

fn parse_expr<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    let start = iter.peek().unwrap().range.start;
    unwrap_ret!(expect_tok(iter, vec!(TokenType::LBrace)));

    let mut expr: LinkedList<Node> = LinkedList::new();
    let mut ret_last = false;

    while iter.peek().is_some() && iter.peek().unwrap().typ != TokenType::RBrace {
        if let TokenType::Semicln = iter.peek().unwrap().typ {
            iter.next();
            continue;
        }

        let node = parse_expression(iter);
        expr.push_back(node);

        if let Some(_) = expect_tok_peek(iter, vec![TokenType::Semicln]) {
            ret_last = true;
            break;
        } else {
            iter.next();
        }
    }

    // let node = prop_node!(parse_expression(iter));
    let end = iter.peek().unwrap().range.end;
    unwrap_ret!(expect_tok(iter, vec!(TokenType::RBrace)));

    Node::new(NodeType::Expresssion(expr, ret_last), start..end)
}

fn parse_tok<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    let tok = iter.next().unwrap();
    Node {
        typ: token_to_node!(tok.typ, panic!("expected literal, found: {:?}", tok.typ),
            BoolLit(val) => BoolLit(val)
            I32Lit(val) => I32Lit(val)
            F32Lit(val) => F32Lit(val)
            StringLit(val) => StringLit(val)
            Id(val) => Id(val)
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

    unwrap_ret!(expect_tok(
        iter,
        vec!(TokenType::Add, TokenType::Sub, TokenType::Not)
    ));
    let node = parse_expression(iter);
    let range = op.range.start..node.range.end;

    Node::new(
        match op.typ {
            Add => NodeType::UnryAdd(node.into()),
            Sub => NodeType::UnryMin(node.into()),
            Not => NodeType::UnryNot(node.into()),
            _ => panic!("expected Add or Sub, found: {:?}", op),
        },
        range,
    )
}

fn eval<I>(iter: &mut I, node: Node) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    let start = node.range.start;

    unwrap_ret!(expect_tok(iter, vec!(TokenType::LParen)));
    unwrap_ret!(expect_tok_peek(iter, vec!(TokenType::RParen)));

    let end = iter.next().unwrap().range.end;

    Node::new(NodeType::Eval(node.into()), start..end)
}

fn atom<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    use TokenType::*;
    if let Some(tok) = iter.peek() {
        let node = match tok.typ {
            LParen => parse_paren(iter),
            LBrace => parse_expr(iter),
            BoolLit(_) | I32Lit(_) | F32Lit(_) | StringLit(_) | Id(_) => parse_tok(iter),
            Add | Sub | Not => parse_unry(iter),

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
        };

        if expect_tok_peek(iter, vec![LParen]).is_none() {
            eval(iter, node)
        } else {
            node
        }
    } else {
        panic!()
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
    while lookahead.is_op() && lookahead.precedence() > precedence {
        let op = lookahead;

        iter.next();
        if iter.peek().is_none() {
            return lhs;
        }

        let mut rhs = atom(iter);

        if iter.peek().is_none() {
            lhs = apply_op(op, lhs, rhs);
            break;
        }
        lookahead = iter.peek().unwrap().typ.clone();

        while lookahead.is_op() && lookahead.precedence() > op.precedence() {
            rhs = parse_sub_expression(iter, rhs, op.precedence());
            if iter.peek().is_none() {
                break;
            }
            lookahead = iter.peek().unwrap().typ.clone();
        }

        lhs = apply_op(op, lhs, rhs);
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
            typ: NodeType::Error("No Tokens found".to_owned()),
            range: (0..0),
            flags: NodeFlags::ERROR,
        };
    }

    let mut iter = tokens.into_iter().peekable();

    parse_expression(&mut iter)
}

pub fn print_errors(n: &Node, code: &str) {
    use NodeType::*;
    match &n.typ {
        BoolLit(_) | I32Lit(_) | F32Lit(_) | StringLit(_) | Id(_) => {}

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
        UnryNot(node) => print_errors(&node, code),

        Equal(lhs, rhs) => {
            print_errors(&lhs, code);
            print_errors(&rhs, code);
        }
        NotEqual(lhs, rhs) => {
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

        Assign(lhs, rhs) => {
            print_errors(&lhs, code);
            print_errors(&rhs, code);
        }

        Error(msg) => {
            println!("Parser: {}", msg);
            println!("{}", error::underline_code(code, &n.range));
        }

        Expresssion(list, _) => {
            for e in list.iter() {
                print_errors(e, code);
            }
        }
        Eval(expr) => {
            print_errors(expr, code);
        }
    }
}
