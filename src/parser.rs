use crate::{
    error,
    lexer::{Range, Token, TokenType},
};
use bitflags::bitflags;
use paste::paste;
use std::{collections::LinkedList, fmt};

macro_rules! unwrap_ret {
    ($e:expr) => {{
        let n = $e;
        if let Err(err) = n {
            return err;
        }
    }};
}

macro_rules! prop_node {
    ($e:expr) => {{
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

    I8Lit(i8),
    U8Lit(u8),

    I16Lit(i16),
    U16Lit(u16),

    I32Lit(i32),
    U32Lit(u32),

    I64Lit(i64),
    U64Lit(u64),

    ISizeLit(isize),
    USizeLit(usize),

    I128Lit(i128),
    U128Lit(u128),

    F32Lit(f32),
    F64Lit(f64),

    StringLit(String),

    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),

    AddEq(Box<Node>, Box<Node>),
    SubEq(Box<Node>, Box<Node>),
    MulEq(Box<Node>, Box<Node>),
    DivEq(Box<Node>, Box<Node>),

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
    IdAnnot(String, Box<Node>),

    Expresssion(LinkedList<Node>),
    Eval(Box<Node>),

    Return(Box<Node>),

    Ref(Box<Node>),
    DeRef(Box<Node>),

    Error(String),
}

//TODO: impl drop

macro_rules! binop_token_to_node {
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
    pub range: Range,
    pub flags: NodeFlags,
}

impl Node {
    pub fn new(typ: NodeType, range: Range, flags: NodeFlags) -> Self {
        Self { typ, range, flags }
    }

    pub fn from(typ: NodeType, range: Range) -> Self {
        Self {
            typ,
            range,
            flags: NodeFlags::empty(),
        }
    }

    pub fn error(msg: &str, range: Range) -> Self {
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
            BoolLit(v) => write!(f, "{v}"),

            I8Lit(v) => write!(f, "{v}"),
            U8Lit(v) => write!(f, "{v}"),

            I16Lit(v) => write!(f, "{v}"),
            U16Lit(v) => write!(f, "{v}"),

            I32Lit(v) => write!(f, "{v}"),
            U32Lit(v) => write!(f, "{v}"),

            I64Lit(v) => write!(f, "{v}"),
            U64Lit(v) => write!(f, "{v}"),

            ISizeLit(v) => write!(f, "{v}"),
            USizeLit(v) => write!(f, "{v}"),

            I128Lit(v) => write!(f, "{v}"),
            U128Lit(v) => write!(f, "{v}"),

            F32Lit(v) => write!(f, "{v}"),
            F64Lit(v) => write!(f, "{v}"),

            StringLit(v) => write!(f, "{v}"),

            Add(lhs, rhs) => write!(f, "({lhs} + {rhs})"),
            Sub(lhs, rhs) => write!(f, "({lhs} - {rhs})"),
            Mul(lhs, rhs) => write!(f, "({lhs} * {rhs})"),
            Div(lhs, rhs) => write!(f, "({lhs} / {rhs})"),

            AddEq(lhs, rhs) => write!(f, "({lhs} += {rhs})"),
            SubEq(lhs, rhs) => write!(f, "({lhs} -= {rhs})"),
            MulEq(lhs, rhs) => write!(f, "({lhs} *= {rhs})"),
            DivEq(lhs, rhs) => write!(f, "({lhs} /= {rhs})"),

            UnryAdd(node) => write!(f, "(+ {node})"),
            UnryMin(node) => write!(f, "(- {node})"),
            UnryNot(node) => write!(f, "(! {node})"),

            Equal(lhs, rhs) => write!(f, "({lhs} == {rhs})"),
            NotEqual(lhs, rhs) => write!(f, "({lhs} != {rhs})"),
            Greater(lhs, rhs) => write!(f, "({lhs} > {rhs})"),
            GreaterEq(lhs, rhs) => write!(f, "({lhs} >= {rhs})"),
            Less(lhs, rhs) => write!(f, "({lhs} < {rhs})"),
            LessEq(lhs, rhs) => write!(f, "({lhs} <= {rhs})"),

            Assign(lhs, rhs) => write!(f, "({lhs} = {rhs})"),

            Id(v) => write!(f, "{v}"),
            IdAnnot(v1, v2) => write!(f, "{v1}: {v2}"),

            Return(n) => write!(f, "(return {n})"),

            Ref(n) => write!(f, "(& {n})"),
            DeRef(n) => write!(f, "(* {n})"),

            Error(msg) => write!(f, "Error: {msg}"),

            Expresssion(expr) => {
                use std::fmt::Write;

                let mut res = String::new();
                let mut it = expr.iter().peekable();

                while let Some(e) = it.next() {
                    if it.peek().is_some() {
                        write!(res, "{e};")?;
                    } else {
                        write!(res, "{e}")?;
                    }
                }
                write!(f, "{{{}}}", res)
            }
            Eval(expr) => write!(f, "{expr}()"),
        }
    }
}

fn apply_op(op: TokenType, lhs: Node, rhs: Node) -> Node {
    let start = lhs.range.start;
    let end = rhs.range.end;

    let l_flags = lhs.flags;
    let r_flags = rhs.flags;

    let res_flags = l_flags | r_flags;

    Node {
        typ: binop_token_to_node!(op, panic!("apply_op for {:?} not implemented", op),
            Add => Add(lhs.into(), rhs.into())
            Sub => Sub(lhs.into(), rhs.into())
            Mul => Mul(lhs.into(), rhs.into())
            Div => Div(lhs.into(), rhs.into())

            AddEq => AddEq(lhs.into(), rhs.into())
            SubEq => SubEq(lhs.into(), rhs.into())
            MulEq => MulEq(lhs.into(), rhs.into())
            DivEq => DivEq(lhs.into(), rhs.into())

            Equal => Equal(lhs.into(), rhs.into())
            NotEqual => NotEqual(lhs.into(), rhs.into())
            Greater => Greater(lhs.into(), rhs.into())
            GreaterEq => GreaterEq(lhs.into(), rhs.into())
            Less => Less(lhs.into(), rhs.into())
            LessEq => LessEq(lhs.into(), rhs.into())

            Assign => Assign(lhs.into(), rhs.into())
        ),
        range: (start..end),
        flags: res_flags,
    }
}

fn expect_tok_peek<I>(iter: &mut I, typs: &[TokenType]) -> Result<(), Node>
where
    I: PeekableIterator<Item = Token>,
{
    let mut err_range = 0..0;
    let mut msg = format!("Expected: {:?}, found NONE", typs);

    if let Some(tok) = iter.peek() {
        for typ in typs {
            if &tok.typ == typ {
                return Ok(());
            }
        }

        err_range = tok.range.clone();
        msg = format!("Expected: {:?}, found {:?}", typs, tok.typ);
    }

    Err(Node::error(&msg, err_range))
}

fn expect_tok<I>(iter: &mut I, typs: &[TokenType]) -> Result<(), Node>
where
    I: PeekableIterator<Item = Token>,
{
    let mut err_range = 0..0;
    let mut msg = format!("Expected: {:?}, found NONE", typs);

    if let Some(tok) = iter.next() {
        for typ in typs {
            if &tok.typ == typ {
                return Ok(());
            }
        }

        err_range = tok.range.clone();
        msg = format!("Expected: {:?}, found {:?}", typs, tok.typ);
    }

    Err(Node::error(&msg, err_range))
}

fn parse_paren<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    unwrap_ret!(expect_tok(iter, &[TokenType::LParen]));

    let node = prop_node!(parse_expression(iter));

    unwrap_ret!(expect_tok(iter, &[TokenType::RParen]));
    node
}

fn parse_expr<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    let start = iter.peek().unwrap().range.start;
    let mut flags = NodeFlags::empty();
    unwrap_ret!(expect_tok(iter, &[TokenType::LBrace]));

    let mut expr: LinkedList<Node> = LinkedList::new();

    while iter.peek().is_some() && iter.peek().unwrap().typ != TokenType::RBrace {
        if let TokenType::Semicln = iter.peek().unwrap().typ {
            iter.next();
            continue;
        }

        let node = parse_expression(iter);
        flags |= node.flags;

        if expect_tok_peek(iter, &[TokenType::Semicln]).is_err() {
            let r = node.range.clone();
            expr.push_back(Node::new(NodeType::Return(node.into()), r, flags));
            break;
        }

        expr.push_back(node);
        iter.next();
    }

    // let node = prop_node!(parse_expression(iter));
    let end = iter.peek().unwrap().range.end;
    unwrap_ret!(expect_tok(iter, &[TokenType::RBrace]));

    Node::new(NodeType::Expresssion(expr), start..end, flags)
}

fn parse_tok<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    let tok = iter.next().unwrap();
    Node::from(
        binop_token_to_node!(tok.typ, panic!("expected literal, found: {:?}", tok.typ),
            BoolLit(val) => BoolLit(val)

                I8Lit(val) => I8Lit(val)
                U8Lit(val) => U8Lit(val)

                I16Lit(val) => I16Lit(val)
                U16Lit(val) => U16Lit(val)

                I32Lit(val) => I32Lit(val)
                U32Lit(val) => U32Lit(val)

                I64Lit(val) => I64Lit(val)
                U64Lit(val) => U64Lit(val)

                ISizeLit(val) => ISizeLit(val)
                USizeLit(val) => USizeLit(val)

                I128Lit(val) => I128Lit(val)
                U128Lit(val) => U128Lit(val)

                F32Lit(val) => F32Lit(val)
                F64Lit(val) => F64Lit(val)

                StringLit(val) => StringLit(val)
                Id(val) => Id(val)
        ),
        tok.range,
    )
}

fn parse_id<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    let tok = iter.next().unwrap();

    if let TokenType::Id(name) = tok.typ {
        if expect_tok_peek(iter, &[TokenType::Colon]).is_ok() {
            iter.next();
            let typ = atom(iter);
            let flags = typ.flags;

            let range = tok.range.start..typ.range.end;

            Node::new(NodeType::IdAnnot(name, typ.into()), range, flags)
        } else {
            Node::from(NodeType::Id(name), tok.range.clone())
        }
    } else {
        panic!("expected id, found: {:?}", tok);
    }
}

fn parse_unry<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    use TokenType::*;

    expect_tok_peek(
        iter,
        &[
            TokenType::Add,
            TokenType::Sub,
            TokenType::Not,
            TokenType::And,
            TokenType::Mul,
        ],
    )
    .expect("Expected: UnryOp");

    let op = iter.next().unwrap();

    let node: Box<Node> =  //parse_expression(iter).into();
    match op.typ {
        And | Mul => atom(iter),
        _ => parse_expression(iter),
    }.into();

    let range = op.range.start..node.range.end;

    Node::from(
        match op.typ {
            Add => NodeType::UnryAdd(node),
            Sub => NodeType::UnryMin(node),
            Not => NodeType::UnryNot(node),
            And => NodeType::Ref(node),
            Mul => NodeType::DeRef(node),
            _ => panic!("expected UnryOp, found: {:?}", op),
        },
        range,
    )
}

fn parse_eval<I>(iter: &mut I, node: Node) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    if expect_tok_peek(iter, &[TokenType::LParen]).is_ok() {
        iter.next();
        let start = node.range.start;
        let flags = node.flags;

        unwrap_ret!(expect_tok_peek(iter, &[TokenType::RParen]));

        let end = iter.next().unwrap().range.end;
        let n = Node {
            typ: NodeType::Eval(node.into()),
            range: start..end,
            flags,
        };

        parse_eval(iter, n)
    } else {
        node
    }
}

fn parse_ret<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    if expect_tok_peek(iter, &[TokenType::Return]).is_ok() {
        let mut range = iter.next().unwrap().range;

        let expr = parse_expression(iter);
        range.end = expr.range.end;
        Node::from(NodeType::Return(expr.into()), range)
    } else {
        panic!("Expected return token, found: {:?}", iter.peek());
    }
}

fn parse_ref<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    if expect_tok_peek(iter, &[TokenType::And]).is_ok() {
        let mut range = iter.next().unwrap().range;

        let expr = atom(iter);
        range.end = expr.range.end;

        Node::from(NodeType::Ref(expr.into()), range)
    } else {
        panic!("Expected ref token, found: {:?}", iter.peek());
    }
}

fn parse_deref<I>(iter: &mut I) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    if expect_tok_peek(iter, &[TokenType::Mul]).is_ok() {
        let mut range = iter.next().unwrap().range;

        let expr = atom(iter);
        range.end = expr.range.end;

        Node::from(NodeType::DeRef(expr.into()), range)
    } else {
        panic!("Expected deref token, found: {:?}", iter.peek());
    }
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
            Return => parse_ret(iter),
            BoolLit(_) | I8Lit(_) | U8Lit(_) | I16Lit(_) | U16Lit(_) | I32Lit(_) | U32Lit(_)
            | I64Lit(_) | U64Lit(_) | ISizeLit(_) | USizeLit(_) | I128Lit(_) | U128Lit(_)
            | F32Lit(_) | F64Lit(_) | StringLit(_) => parse_tok(iter),
            Mul | And | Add | Sub | Not => parse_unry(iter),

            Id(_) => parse_id(iter),

            //And => parse_ref(iter),
            //Mul => parse_deref(iter),
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
        parse_eval(iter, node)
    } else {
        panic!()
    }
}

fn parse_sub_expression<I>(iter: &mut I, mut lhs: Node, precedence: i32) -> Node
where
    I: PeekableIterator<Item = Token>,
{
    //if iter.peek().is_none() || iter.peek().unwrap().typ == TokenType::Eof {
    if iter.peek().is_none() {
        panic!("no EOF at the end of token stream!");
    }

    if iter.peek().unwrap().typ == TokenType::Eof {
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
    let err = print_errors;

    if !n.flags.contains(NodeFlags::ERROR) {
        return;
    }

    use NodeType::*;
    match &n.typ {
        I8Lit(_) | U8Lit(_) | I16Lit(_) | U16Lit(_) | I32Lit(_) | U32Lit(_) | I64Lit(_)
        | U64Lit(_) | ISizeLit(_) | USizeLit(_) | I128Lit(_) | U128Lit(_) | BoolLit(_)
        | F32Lit(_) | F64Lit(_) | StringLit(_) | Id(_) => {}

        Add(lhs, rhs) => {
            err(lhs, code);
            err(rhs, code);
        }
        Sub(lhs, rhs) => {
            err(lhs, code);
            err(rhs, code);
        }
        Mul(lhs, rhs) => {
            err(lhs, code);
            err(rhs, code);
        }
        Div(lhs, rhs) => {
            err(lhs, code);
            err(rhs, code);
        }

        AddEq(lhs, rhs) => {
            err(lhs, code);
            err(rhs, code);
        }
        SubEq(lhs, rhs) => {
            err(lhs, code);
            err(rhs, code);
        }
        MulEq(lhs, rhs) => {
            err(lhs, code);
            err(rhs, code);
        }
        DivEq(lhs, rhs) => {
            err(lhs, code);
            err(rhs, code);
        }

        UnryAdd(node) => err(node, code),
        UnryMin(node) => err(node, code),
        UnryNot(node) => err(node, code),

        Equal(lhs, rhs) => {
            err(lhs, code);
            err(rhs, code);
        }
        NotEqual(lhs, rhs) => {
            err(lhs, code);
            err(rhs, code);
        }
        Greater(lhs, rhs) => {
            err(lhs, code);
            err(rhs, code);
        }
        GreaterEq(lhs, rhs) => {
            err(lhs, code);
            err(rhs, code);
        }
        Less(lhs, rhs) => {
            err(lhs, code);
            err(rhs, code);
        }
        LessEq(lhs, rhs) => {
            err(lhs, code);
            err(rhs, code);
        }

        Assign(lhs, rhs) => {
            err(lhs, code);
            err(rhs, code);
        }

        Expresssion(list) => {
            for e in list {
                err(e, code);
            }
        }
        Eval(expr) => err(expr, code),

        IdAnnot(_, v) => err(v, code),

        Ref(expr) => err(expr, code),
        DeRef(expr) => err(expr, code),

        Return(n) => err(n, code),

        Error(msg) => {
            println!("Parser: {}", msg);
            println!("{}", error::underline_code(code, &n.range));
        }
    }
}
