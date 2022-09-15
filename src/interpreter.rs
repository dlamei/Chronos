use crate::{
    chvalue::{self, ChValue, Scope},
    error,
    lexer::Position,
    parser::{Node, NodeType},
};
use core::result::Result;
use std::{cell::RefCell, collections::LinkedList, ops, rc::Rc};

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

pub fn print_error(err: RuntimeErr, code: &str) {
    println!("Interpreter: {:?}", err.typ);
    println!("{}", error::underline_code(code, &err.range));
}

fn visit_bin_op<F>(
    op: F,
    lhs: &Node,
    rhs: &Node,
    scope: &Rc<RefCell<Scope>>,
) -> Result<ChValue, RuntimeErr>
where
    F: Fn(ChValue, ChValue) -> Result<ChValue, ErrType>,
{
    let range = lhs.range.start..rhs.range.end;

    let left_val = visit_node(lhs, scope)?;
    let right_val = visit_node(rhs, scope)?;

    match op(left_val, right_val) {
        Err(e) => Err(RuntimeErr::new(e, range)),
        Ok(e) => Ok(e),
    }
}

fn visit_unry_op<F>(op: F, n: &Node, scope: &Rc<RefCell<Scope>>) -> Result<ChValue, RuntimeErr>
where
    F: Fn(ChValue) -> Result<ChValue, ErrType>,
{
    let range = n.range.clone();
    let val = visit_node(n, scope)?;

    match op(val) {
        Err(e) => Err(RuntimeErr::new(e, range)),
        Ok(e) => Ok(e),
    }
}

fn visit_assign(lhs: &Node, rhs: &Node, scope: &Rc<RefCell<Scope>>) -> Result<ChValue, RuntimeErr> {
    let range = lhs.range.start..rhs.range.end;

    let rhs_val = visit_node(rhs, scope)?;
    let ret = rhs_val.clone();

    match &lhs.typ {
        NodeType::Id(name) => scope.borrow_mut().insert(name.to_string(), rhs_val),
        _ => return Err(RuntimeErr::new(ErrType::UnAllowedAssign, range)),
    };

    Ok(ret)
}

fn visit_access(n: &Node, scope: &Rc<RefCell<Scope>>) -> Result<ChValue, RuntimeErr> {
    if let NodeType::Id(name) = &n.typ {
        if let Some(val) = scope.borrow().get(name) {
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

fn visit_eval(node: &Node, scope: &Rc<RefCell<Scope>>) -> Result<ChValue, RuntimeErr> {
    let range = node.range.clone();

    let val = visit_node(node, scope)?;

    if let ChValue::Expression(expr) = val {
        if expr.nodes.is_empty() {
            return Ok(ChValue::Void);
        }

        let mut it = expr.nodes.iter();
        let mut val = visit_node(it.next().unwrap(), &expr.scope)?;

        for n in it {
            val = visit_node(n, &expr.scope)?;
        }

        if expr.ret_last {
            Ok(val)
        } else {
            Ok(ChValue::Void)
        }
    } else {
        Err(RuntimeErr::new(
            ErrType::UnsupportedOperand(format!("Can't evaluate type: {:?}", val.get_type())),
            range,
        ))
    }
}

fn visit_expr(
    nodes: &LinkedList<Node>,
    ret_last: bool,
    scope: &Rc<RefCell<Scope>>,
) -> Result<ChValue, RuntimeErr> {
    let expr = chvalue::ExpressionData {
        nodes: nodes.clone(),
        ret_last,
        scope: Rc::new(RefCell::new(Scope::new())),
        parent: scope.clone(),
    };
    Ok(ChValue::Expression(expr))
}

fn visit_node(node: &Node, scope: &Rc<RefCell<Scope>>) -> Result<ChValue, RuntimeErr> {
    use NodeType::*;
    match &node.typ {
        BoolLit(val) => Ok(ChValue::Bool(*val)),
        I32Lit(val) => Ok(ChValue::I32(*val)),
        F32Lit(val) => Ok(ChValue::F32(*val)),

        Add(lhs, rhs) => visit_bin_op(ops::Add::add, lhs, rhs, scope),
        Sub(lhs, rhs) => visit_bin_op(ops::Sub::sub, lhs, rhs, scope),
        Mul(lhs, rhs) => visit_bin_op(ops::Mul::mul, lhs, rhs, scope),
        Div(lhs, rhs) => visit_bin_op(ops::Div::div, lhs, rhs, scope),

        Equal(lhs, rhs) => visit_bin_op(
            |v1: ChValue, v2: ChValue| Ok(ChValue::Bool(v1 == v2)),
            lhs,
            rhs,
            scope,
        ),
        NotEqual(lhs, rhs) => visit_bin_op(
            |v1: ChValue, v2: ChValue| Ok(ChValue::Bool(v1 != v2)),
            lhs,
            rhs,
            scope,
        ),
        Greater(lhs, rhs) => visit_bin_op(
            |v1: ChValue, v2: ChValue| Ok(ChValue::Bool(v1 > v2)),
            lhs,
            rhs,
            scope,
        ),
        GreaterEq(lhs, rhs) => visit_bin_op(
            |v1: ChValue, v2: ChValue| Ok(ChValue::Bool(v1 >= v2)),
            lhs,
            rhs,
            scope,
        ),
        Less(lhs, rhs) => visit_bin_op(
            |v1: ChValue, v2: ChValue| Ok(ChValue::Bool(v1 < v2)),
            lhs,
            rhs,
            scope,
        ),
        LessEq(lhs, rhs) => visit_bin_op(
            |v1: ChValue, v2: ChValue| Ok(ChValue::Bool(v1 <= v2)),
            lhs,
            rhs,
            scope,
        ),

        UnryAdd(n) => visit_unry_op(Ok, n, scope),
        UnryMin(n) => visit_unry_op(|v: ChValue| v * ChValue::I8(-1), n, scope),
        UnryNot(n) => visit_unry_op(|v: ChValue| Ok(ChValue::Bool(!v.as_bool())), n, scope),

        Assign(id, n) => visit_assign(id, n, scope),
        Id(_) => visit_access(node, scope),

        Expresssion(exprs, ret_last) => visit_expr(exprs, *ret_last, scope),
        Eval(expr) => visit_eval(expr, scope),
        _ => panic!("visit_node for {:?} not implemented", node.typ),
    }
}

pub fn interpret(root: &Node) -> Result<ChValue, RuntimeErr> {
    // visit_node(root, &mut Scope::new())
    visit_node(root, &Rc::new(RefCell::new(Scope::new())))
}
