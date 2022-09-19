use crate::{
    primitive::{self, Primitive, Scope},
    error,
    lexer::{Position, TokenType},
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
) -> Result<Primitive, RuntimeErr>
where
    F: Fn(Primitive, Primitive) -> Result<Primitive, ErrType>,
{
    let range = lhs.range.start..rhs.range.end;

    let left_val = visit_node(lhs, scope)?;
    let right_val = visit_node(rhs, scope)?;

    match op(left_val, right_val) {
        Err(e) => Err(RuntimeErr::new(e, range)),
        Ok(e) => Ok(e),
    }
}

fn visit_assign_op(
    op: TokenType,
    lhs: &Node,
    rhs: &Node,
    scope: &Rc<RefCell<Scope>>,
) -> Result<Primitive, RuntimeErr>
{
    let range = lhs.range.start..rhs.range.end;

    let typ = match op {
        TokenType::Add => {
            NodeType::Add(lhs.clone().into(), rhs.clone().into())
        }
        TokenType::Sub => {
            NodeType::Sub(lhs.clone().into(), rhs.clone().into())
        }
        TokenType::Mul => {
            NodeType::Mul(lhs.clone().into(), rhs.clone().into())
        }
        TokenType::Div => {
            NodeType::Div(lhs.clone().into(), rhs.clone().into())
        }
        _ => panic!("expected add, sub, mul, div, found: {:?}", op),
    };
    
    visit_assign(lhs, &Node::from(typ, range), scope)
}

fn visit_unry_op<F>(op: F, n: &Node, scope: &Rc<RefCell<Scope>>) -> Result<Primitive, RuntimeErr>
where
    F: Fn(Primitive) -> Result<Primitive, ErrType>,
{
    let range = n.range.clone();
    let val = visit_node(n, scope)?;

    match op(val) {
        Err(e) => Err(RuntimeErr::new(e, range)),
        Ok(e) => Ok(e),
    }
}

fn get_scope_with_key(name: String, scope: &Rc<RefCell<Scope>>) -> Option<Rc<RefCell<Scope>>> {
    let mut scp = scope.clone();

    if scp.borrow().map.contains_key(&name) {
        return Some(scp);
    }

    while let Some(parent) = &scp.clone().borrow().parent {
        if parent.borrow().map.contains_key(&name) {
            return Some(parent.clone());
        }
        scp = parent.clone();
    }
    None
}

fn visit_assign(lhs: &Node, rhs: &Node, scope: &Rc<RefCell<Scope>>) -> Result<Primitive, RuntimeErr> {
    let range = lhs.range.start..rhs.range.end;

    match &lhs.typ {
        NodeType::Id(name) => {
            let name = name.to_string();

            let val = visit_node(rhs, scope)?;

            let ret = val.clone();

            if let Some(s) = get_scope_with_key(name.clone(), scope) {
                // s.borrow_mut().map.insert(name, Rc::new(RefCell::new(val)));
                let ref_val = s.borrow_mut().map.get(&name).unwrap().clone();
                ref_val.replace(val);

            } else {
                scope.borrow_mut().map.insert(name, Rc::new(RefCell::new(val)));
            }

            Ok(ret)
        }
        NodeType::DeRef(ref_node) => {
            let ref_val = visit_node(ref_node, scope)?;
            let val = visit_node(rhs, scope)?;

            if let Primitive::Ref(v) = ref_val {
                v.replace(val.clone());
                Ok(val)
            } else {
                Err(RuntimeErr::new(ErrType::UnAllowedAssign, range))
            }
        }
        _ => Err(RuntimeErr::new(ErrType::UnAllowedAssign, range)),
    }
}

fn visit_access(n: &Node, scope: &Rc<RefCell<Scope>>) -> Result<Primitive, RuntimeErr> {
    if let NodeType::Id(name) = &n.typ {
        if let Some(val) = scope.borrow().map.get(name) {
            Ok(val.borrow().clone())
        } else if let Some(parent) = &scope.borrow().parent {
            visit_access(n, &parent)
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

fn visit_ref(n: &Node, scope: &Rc<RefCell<Scope>>) -> Result<Primitive, RuntimeErr> {
    let range = n.range.clone();

    if let NodeType::Id(name) = &n.typ {
        if let Some(s) = get_scope_with_key(name.to_owned(), scope) {
            Ok(Primitive::Ref(s.borrow().map.get(name).unwrap().clone()))
        } else {
            Err(RuntimeErr::new(
                ErrType::Undefinded(format!("{}", name)),
                range,
            ))
        }
    } else {
        let val = visit_node(n, scope)?;
        Ok(Primitive::Ref(Rc::new(RefCell::new(val))))
    }
}

fn visit_deref(n: &Node, scope: &Rc<RefCell<Scope>>) -> Result<Primitive, RuntimeErr> {
    let range = n.range.clone();

    let val = visit_node(n, scope)?;

    if let Primitive::Ref(expr) = val {
        Ok(expr.borrow().clone())
        // Ok(ChValue::DeRef(expr.into()))
    } else {
        Err(RuntimeErr::new(
            ErrType::UnsupportedOperand(format!("Can't deref type: {:?}", val.get_type())),
            range,
        ))
    }
}

fn visit_eval(node: &Node, scope: &Rc<RefCell<Scope>>) -> Result<Primitive, RuntimeErr> {
    let range = node.range.clone();

    let val = visit_node(node, scope)?;

    if let Primitive::Expression(expr) = val {
        expr.scope.borrow_mut().parent = Some(scope.clone());

        if expr.nodes.is_empty() {
            return Ok(Primitive::Void);
        }

        let mut ret_val = false;

        let mut it = expr.nodes.iter().peekable();

        if let NodeType::Return(_) = it.peek().unwrap().typ {
            ret_val = true;
        }

        let mut val = visit_node(it.next().unwrap(), &expr.scope)?;
        if ret_val {
            return Ok(val);
        }

        while let Some(n) = it.next() {
            if let NodeType::Return(_) = n.typ {
                ret_val = true;
            }

            val = visit_node(n, &expr.scope)?;

            if ret_val {
                return Ok(val);
            }
        }

        if expr.ret_last {
            Ok(val)
        } else {
            Ok(Primitive::Void)
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
) -> Result<Primitive, RuntimeErr> {
    let expr = primitive::ExpressionData {
        nodes: nodes.clone(), //TODO: move nodes
        ret_last,
        scope: Rc::new(RefCell::new(Scope::from(scope.clone()))),
        // parent: scope.clone(),
    };
    Ok(Primitive::Expression(expr))
}

fn visit_node(node: &Node, scope: &Rc<RefCell<Scope>>) -> Result<Primitive, RuntimeErr> {
    use NodeType::*;
    match &node.typ {
        BoolLit(val) => Ok(Primitive::Bool(*val)),

        I8Lit(val) => Ok(Primitive::I8(*val)),
        U8Lit(val) => Ok(Primitive::U8(*val)),

        I16Lit(val) => Ok(Primitive::I16(*val)),
        U16Lit(val) => Ok(Primitive::U16(*val)),

        I32Lit(val) => Ok(Primitive::I32(*val)),
        U32Lit(val) => Ok(Primitive::U32(*val)),

        I64Lit(val) => Ok(Primitive::I64(*val)),
        U64Lit(val) => Ok(Primitive::U64(*val)),

        ISizeLit(val) => Ok(Primitive::ISize(*val)),
        USizeLit(val) => Ok(Primitive::USize(*val)),

        I128Lit(val) => Ok(Primitive::I128(*val)),
        U128Lit(val) => Ok(Primitive::U128(*val)),

        F32Lit(val) => Ok(Primitive::F32(*val)),
        F64Lit(val) => Ok(Primitive::F64(*val)),

        StringLit(val) => Ok(Primitive::String(val.to_owned())),

        Add(lhs, rhs) => visit_bin_op(ops::Add::add, lhs, rhs, scope),
        Sub(lhs, rhs) => visit_bin_op(ops::Sub::sub, lhs, rhs, scope),
        Mul(lhs, rhs) => visit_bin_op(ops::Mul::mul, lhs, rhs, scope),
        Div(lhs, rhs) => visit_bin_op(ops::Div::div, lhs, rhs, scope),

        AddEq(lhs, rhs) => visit_assign_op(TokenType::Add, lhs, rhs, scope),
        SubEq(lhs, rhs) => visit_assign_op(TokenType::Sub, lhs, rhs, scope),
        MulEq(lhs, rhs) => visit_assign_op(TokenType::Mul, lhs, rhs, scope),
        DivEq(lhs, rhs) => visit_assign_op(TokenType::Div, lhs, rhs, scope),

        Equal(lhs, rhs) => visit_bin_op(
            |v1: Primitive, v2: Primitive| Ok(Primitive::Bool(v1 == v2)),
            lhs,
            rhs,
            scope,
        ),
        NotEqual(lhs, rhs) => visit_bin_op(
            |v1: Primitive, v2: Primitive| Ok(Primitive::Bool(v1 != v2)),
            lhs,
            rhs,
            scope,
        ),
        Greater(lhs, rhs) => visit_bin_op(
            |v1: Primitive, v2: Primitive| Ok(Primitive::Bool(v1 > v2)),
            lhs,
            rhs,
            scope,
        ),
        GreaterEq(lhs, rhs) => visit_bin_op(
            |v1: Primitive, v2: Primitive| Ok(Primitive::Bool(v1 >= v2)),
            lhs,
            rhs,
            scope,
        ),
        Less(lhs, rhs) => visit_bin_op(
            |v1: Primitive, v2: Primitive| Ok(Primitive::Bool(v1 < v2)),
            lhs,
            rhs,
            scope,
        ),
        LessEq(lhs, rhs) => visit_bin_op(
            |v1: Primitive, v2: Primitive| Ok(Primitive::Bool(v1 <= v2)),
            lhs,
            rhs,
            scope,
        ),

        UnryAdd(n) => visit_unry_op(Ok, n, scope),
        UnryMin(n) => visit_unry_op(|v: Primitive| v * Primitive::I8(-1), n, scope),
        UnryNot(n) => visit_unry_op(|v: Primitive| Ok(Primitive::Bool(!v.as_bool())), n, scope),

        Assign(id, n) => visit_assign(id, n, scope),
        Id(_) => visit_access(node, scope),

        Expresssion(exprs, ret_last) => visit_expr(exprs, *ret_last, scope),
        Eval(expr) => visit_eval(expr, scope),

        Return(expr) => visit_node(expr, scope),

        Ref(expr) => visit_ref(expr, scope),
        DeRef(expr) => visit_deref(expr, scope),

        Error(e) => panic!("found error from lexer: {}", e),

        _ => panic!("visit_node for {:?} not implemented", node.typ),
    }
}

pub fn interpret(root: &Node) -> Result<Primitive, RuntimeErr> {
    // visit_node(root, &mut Scope::new())
    visit_node(root, &Rc::new(RefCell::new(Scope::new())))
}
