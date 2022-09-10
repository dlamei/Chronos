use crate::{
    chtype::ChType,
    error,
    lexer::Position,
    parser::{Node, NodeType},
};
use core::result::Result;
use std::{
    collections::{HashMap, LinkedList},
    ops,
};

#[derive(Debug, PartialEq)]
pub enum ErrType {
    ZeroDivision,
    UnsupportedOperand(String),
    UnAllowedAssign,
    Undefinded(String),
}

#[derive(Debug, PartialEq)]
pub struct RuntimeErr {
    typ: ErrType,
    range: Position,
}

impl RuntimeErr {
    fn new(typ: ErrType, range: Position) -> Self {
        Self { typ, range }
    }
}

type Scope = HashMap<String, ChType>;

pub fn print_error(err: RuntimeErr, code: &str) {
    println!("Interpreter: {:?}", err.typ);
    println!("{}", error::underline_code(code, &err.range));
}

fn visit_bin_op<F>(op: F, lhs: &Node, rhs: &Node, scope: &mut Scope) -> Result<ChType, RuntimeErr>
where
    F: Fn(ChType, ChType) -> Result<ChType, ErrType>,
{
    let range = lhs.range.start..rhs.range.end;

    let left_val = visit_node(lhs, scope)?;
    let right_val = visit_node(rhs, scope)?;

    match op(left_val, right_val) {
        Err(e) => Err(RuntimeErr::new(e, range)),
        Ok(e) => Ok(e),
    }
}

fn visit_unry_op<F>(op: F, n: &Node, scope: &mut Scope) -> Result<ChType, RuntimeErr>
where
    F: Fn(ChType) -> Result<ChType, ErrType>,
{
    let range = n.range.clone();
    let val = visit_node(n, scope)?;

    match op(val) {
        Err(e) => Err(RuntimeErr::new(e, range)),
        Ok(e) => Ok(e),
    }
}

fn visit_assign(lhs: &Node, rhs: &Node, scope: &mut Scope) -> Result<ChType, RuntimeErr> {
    let range = lhs.range.start..rhs.range.end;

    let rhs_val = visit_node(rhs, scope)?;
    let ret = rhs_val.clone();

    match &lhs.typ {
        NodeType::Id(name) => scope.insert(name.to_string(), rhs_val.clone()),
        _ => return Err(RuntimeErr::new(ErrType::UnAllowedAssign, range)),
    };

    Ok(ret)
}

fn visit_access(n: &Node, scope: &mut Scope) -> Result<ChType, RuntimeErr> {
    if let NodeType::Id(name) = &n.typ {
        if let Some(val) = scope.get(name) {
            Ok(val.clone())
        } else {
            Err(RuntimeErr::new(
                ErrType::Undefinded(name.to_owned()),
                n.range.clone(),
            ))
        }
    } else {
        panic!("access is only defined for Id, found: {:?}", n)
    }
}

fn visit_expr(
    exprs: &LinkedList<Node>,
    ret_last: bool,
    scope: &mut Scope,
) -> Result<ChType, RuntimeErr> {
    if exprs.is_empty() {
        return Ok(ChType::Void);
    }

    let mut it = exprs.into_iter();
    let mut val = visit_node(it.next().unwrap(), scope)?;

    while let Some(n) = it.next() {
        val = visit_node(n, scope)?;
    }

    if ret_last {
        Ok(val)
    } else {
        Ok(ChType::Void)
    }
}

fn visit_node(node: &Node, scope: &mut Scope) -> Result<ChType, RuntimeErr> {
    use NodeType::*;
    match &node.typ {
        BoolLit(val) => Ok(ChType::Bool(*val)),
        I32Lit(val) => Ok(ChType::I32(*val)),
        F32Lit(val) => Ok(ChType::F32(*val)),

        Add(lhs, rhs) => visit_bin_op(ops::Add::add, lhs, rhs, scope),
        Sub(lhs, rhs) => visit_bin_op(ops::Sub::sub, lhs, rhs, scope),
        Mul(lhs, rhs) => visit_bin_op(ops::Mul::mul, lhs, rhs, scope),
        Div(lhs, rhs) => visit_bin_op(ops::Div::div, lhs, rhs, scope),

        Equal(lhs, rhs) => visit_bin_op(
            |v1: ChType, v2: ChType| Ok(ChType::Bool(v1 == v2)),
            lhs,
            rhs,
            scope,
        ),
        NotEqual(lhs, rhs) => visit_bin_op(
            |v1: ChType, v2: ChType| Ok(ChType::Bool(v1 != v2)),
            lhs,
            rhs,
            scope,
        ),
        Greater(lhs, rhs) => visit_bin_op(
            |v1: ChType, v2: ChType| Ok(ChType::Bool(v1 > v2)),
            lhs,
            rhs,
            scope,
        ),
        GreaterEq(lhs, rhs) => visit_bin_op(
            |v1: ChType, v2: ChType| Ok(ChType::Bool(v1 >= v2)),
            lhs,
            rhs,
            scope,
        ),
        Less(lhs, rhs) => visit_bin_op(
            |v1: ChType, v2: ChType| Ok(ChType::Bool(v1 < v2)),
            lhs,
            rhs,
            scope,
        ),
        LessEq(lhs, rhs) => visit_bin_op(
            |v1: ChType, v2: ChType| Ok(ChType::Bool(v1 <= v2)),
            lhs,
            rhs,
            scope,
        ),

        UnryAdd(n) => visit_unry_op(|v: ChType| Ok(v), n, scope),
        UnryMin(n) => visit_unry_op(|v: ChType| v * ChType::I8(-1), n, scope),
        UnryNot(n) => visit_unry_op(|v: ChType| Ok(ChType::Bool(!v.as_bool())), n, scope),

        Assign(id, n) => visit_assign(id, n, scope),
        Id(_) => visit_access(node, scope),

        Expresssion(exprs, ret_last) => visit_expr(exprs, *ret_last, scope),
        _ => panic!("visit_node for {:?} not implemented", node.typ),
    }
}

pub fn interpret(root: &Node) -> Result<ChType, RuntimeErr> {
    visit_node(root, &mut Scope::new())
}
