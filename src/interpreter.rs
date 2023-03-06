use crate::{
    chvalue::{ChValue, Scope},
    error,
    lexer::{Range, TokenType},
    parser::{Node, NodeType},
    primitive::{self, Primitive},
};
use core::result::Result;
use std::{cell::RefCell, collections::LinkedList, ops, rc::Rc};

#[derive(Debug, PartialEq)]
pub enum ErrType {
    ZeroDivision,
    UnsupportedOperand(String),
    UnAllowedAssign,
    Undefinded(String),

    UnInitialized(String),
    AlreadyDefined(String),
}

#[derive(Debug, PartialEq)]
pub struct RuntimeErr {
    typ: ErrType,
    range: Range,
}

impl RuntimeErr {
    fn new(typ: ErrType, range: Range) -> Self {
        Self { typ, range }
    }
}

pub fn print_error(err: RuntimeErr, code: &str) {
    println!("Interpreter: {:?}", err.typ);
    println!("{}", error::underline_code(code, &err.range));
}

type ChResult = Result<ChValue, RuntimeErr>;

fn visit_bin_op<F>(
    op: F,
    lhs: &Node,
    rhs: &Node,
    scope: &Rc<RefCell<Scope>>,
) -> ChResult
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

fn visit_assign_op(
    op: TokenType,
    lhs: &Node,
    rhs: &Node,
    scope: &Rc<RefCell<Scope>>,
) -> ChResult {
    let range = lhs.range.start..rhs.range.end;

    let typ = match op {
        TokenType::Add => NodeType::Add(lhs.clone().into(), rhs.clone().into()),
        TokenType::Sub => NodeType::Sub(lhs.clone().into(), rhs.clone().into()),
        TokenType::Mul => NodeType::Mul(lhs.clone().into(), rhs.clone().into()),
        TokenType::Div => NodeType::Div(lhs.clone().into(), rhs.clone().into()),
        _ => panic!("expected add, sub, mul, div, found: {:?}", op),
    };

    visit_assign(lhs, &Node::from(typ, range), scope)
}

fn visit_unry_op<F>(op: F, n: &Node, scope: &Rc<RefCell<Scope>>) -> ChResult
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

//fn get_scope_with_key(name: &str, scope: &Rc<RefCell<Scope>>) -> Option<Rc<RefCell<Scope>>> {
//    let mut scp = scope.clone();
//
//    if scp.borrow().map.contains_key(name) {
//        return Some(scp);
//    }
//
//    while let Some(parent) = &scp.clone().borrow().parent {
//        if parent.borrow().map.contains_key(name) {
//            return Some(parent.clone());
//        }
//        scp = parent.clone();
//    }
//    None
//}

fn visit_assign(lhs: &Node, rhs: &Node, scope: &Rc<RefCell<Scope>>) -> ChResult {
    let range = lhs.range.start..rhs.range.end;

    match &lhs.typ {
        NodeType::Id(name) => {
            let name = name.to_string();

            let mut val = visit_node(rhs, scope)?;
            let ret = val.clone();

            let key = scope.borrow().find_key(&name);

            if let Some(ref_val) = key {
                if let Some(typ) = &ref_val.borrow().cast {
                    val.value = val.value.as_transformed_num(&typ.get_type());
                }

                ref_val.replace(val);
            } else {
                scope
                    .borrow_mut()
                    .map
                    .insert(name, Rc::new(RefCell::new(val)));
            }

            Ok(ret)
        }
        NodeType::IdAnnot(name, typ) => {
            let name = name.to_string();

            if let Some(_) = scope.borrow().find_key(&name) {
                return Err(RuntimeErr::new(
                    ErrType::AlreadyDefined(name.clone()),
                    range,
                ));
            }

            let mut val = visit_node(rhs, scope)?;
            let typ = visit_node(typ, scope)?;

            val.value = val.value.as_transformed_num(&typ.value.get_type());
            val.cast = Some(typ.value);

            let ret = val.clone();

            scope
                .borrow_mut()
                .map
                .insert(name, Rc::new(RefCell::new(val)));

            Ok(ret)
        }
        NodeType::DeRef(ref_node) => {
            let ref_val = visit_node(ref_node, scope)?;
            let val = visit_node(rhs, scope)?;

            if let Primitive::Ref(v) = ref_val.value {
                v.replace(val.clone());
                Ok(val)
            } else {
                Err(RuntimeErr::new(ErrType::UnAllowedAssign, range))
            }
        }
        _ => Err(RuntimeErr::new(ErrType::UnAllowedAssign, range)),
    }
}

fn visit_access(n: &Node, scope: &Rc<RefCell<Scope>>) -> ChResult {
    let name = if let NodeType::Id(name) = &n.typ {
        name
    } else {
        panic!("access is only defined for Id, found: {:?}", n)
    };

    let val = if let Some(v) = scope.borrow().find_key(&name) {
        v.borrow().clone()
    } else {
        return Err(RuntimeErr::new(
            ErrType::Undefinded(name.to_owned()),
            n.range.clone(),
        ));
    };

    if let Primitive::UnInit = val.value {
        Err(RuntimeErr::new(
            ErrType::UnInitialized(format!("{}", n)),
            n.range.clone(),
        ))
    } else {
        Ok(val)
    }
}

fn visit_define(n: &Node, scope: &Rc<RefCell<Scope>>) -> ChResult {
    let range = n.range.clone();

    let (name, val) = if let NodeType::IdAnnot(name, val) = &n.typ {
        (name, val)
    } else {
        panic!("access is only defined for IdAnnot, found: {:?}", n)
    };

    if scope.borrow().find_key(name).is_some() {
        Err(RuntimeErr::new(
            ErrType::AlreadyDefined(name.to_owned()),
            range,
        ))
    } else {
        let v: ChValue = visit_node(val, scope)?;
        let value = ChValue::new(Primitive::UnInit, Some(v.value));
        scope
            .borrow_mut()
            .map
            .insert(name.to_owned(), Rc::new(RefCell::new(value)));
        Ok(ChValue::from(Primitive::Void))
    }
}

fn visit_ref(n: &Node, scope: &Rc<RefCell<Scope>>) -> ChResult {
    let range = n.range.clone();

    let name = if let NodeType::Id(name) = &n.typ {
        name
    } else {
        return Err(RuntimeErr::new(
            ErrType::UnsupportedOperand("trying to get address of temporary".to_owned()),
            range
        ));
        //let val = visit_node(n, scope)?;
        //return Ok(ChValue::from(Primitive::Ref(Rc::new(RefCell::new(val)))));
    };

    if let Some(v) = scope.borrow().find_key(&name) {
        Ok(ChValue::from(Primitive::Ref(v)))
    } else {
        Err(RuntimeErr::new(
            ErrType::Undefinded(name.to_string()),
            range,
        ))
    }
}

fn visit_deref(n: &Node, scope: &Rc<RefCell<Scope>>) -> ChResult {
    let range = n.range.clone();

    let val = visit_node(n, scope)?;

    if let Primitive::Ref(expr) = val.value {
        Ok(expr.borrow().clone())
        // Ok(ChValue::DeRef(expr.into()))
    } else {
        Err(RuntimeErr::new(
            ErrType::UnsupportedOperand(format!("Can't deref type: {:?}", val.value.get_type())),
            range,
        ))
    }
}

fn visit_eval(node: &Node, scope: &Rc<RefCell<Scope>>) -> ChResult {
    let range = node.range.clone();

    let val = visit_node(node, scope)?;

    if let Primitive::Expression(expr) = val.value {
        expr.scope.borrow_mut().parent = Some(scope.clone());

        if expr.nodes.is_empty() {
            return Ok(ChValue::from(Primitive::Void));
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

        for n in it {
            if let NodeType::Return(_) = n.typ {
                ret_val = true;
            }

            val = visit_node(n, &expr.scope)?;

            if ret_val {
                return Ok(val);
            }
        }

        Ok(ChValue::from(Primitive::Void))
    } else {
        Err(RuntimeErr::new(
            ErrType::UnsupportedOperand(format!("Can't evaluate type: {:?}", val.value.get_type())),
            range,
        ))
    }
}

fn visit_expr(nodes: &LinkedList<Node>, scope: &Rc<RefCell<Scope>>) -> ChResult {
    let expr = primitive::ExpressionData {
        nodes: nodes.clone(), //TODO: move nodes
        scope: Rc::new(RefCell::new(Scope::from(scope.clone()))),
        // parent: scope.clone(),
    };
    Ok(ChValue::from(Primitive::Expression(expr)))
}

fn visit_node(node: &Node, scope: &Rc<RefCell<Scope>>) -> ChResult {
    use NodeType::*;
    match &node.typ {
        BoolLit(val) => Ok(ChValue::from(Primitive::Bool(*val))),

        I8Lit(val) => Ok(ChValue::from(Primitive::I8(*val))),
        U8Lit(val) => Ok(ChValue::from(Primitive::U8(*val))),

        I16Lit(val) => Ok(ChValue::from(Primitive::I16(*val))),
        U16Lit(val) => Ok(ChValue::from(Primitive::U16(*val))),

        I32Lit(val) => Ok(ChValue::from(Primitive::I32(*val))),
        U32Lit(val) => Ok(ChValue::from(Primitive::U32(*val))),

        I64Lit(val) => Ok(ChValue::from(Primitive::I64(*val))),
        U64Lit(val) => Ok(ChValue::from(Primitive::U64(*val))),

        ISizeLit(val) => Ok(ChValue::from(Primitive::ISize(*val))),
        USizeLit(val) => Ok(ChValue::from(Primitive::USize(*val))),

        I128Lit(val) => Ok(ChValue::from(Primitive::I128(*val))),
        U128Lit(val) => Ok(ChValue::from(Primitive::U128(*val))),

        F32Lit(val) => Ok(ChValue::from(Primitive::F32(*val))),
        F64Lit(val) => Ok(ChValue::from(Primitive::F64(*val))),

        StringLit(val) => Ok(ChValue::from(Primitive::String(val.to_owned()))),

        Add(lhs, rhs) => visit_bin_op(ops::Add::add, lhs, rhs, scope),
        Sub(lhs, rhs) => visit_bin_op(ops::Sub::sub, lhs, rhs, scope),
        Mul(lhs, rhs) => visit_bin_op(ops::Mul::mul, lhs, rhs, scope),
        Div(lhs, rhs) => visit_bin_op(ops::Div::div, lhs, rhs, scope),

        AddEq(lhs, rhs) => visit_assign_op(TokenType::Add, lhs, rhs, scope),
        SubEq(lhs, rhs) => visit_assign_op(TokenType::Sub, lhs, rhs, scope),
        MulEq(lhs, rhs) => visit_assign_op(TokenType::Mul, lhs, rhs, scope),
        DivEq(lhs, rhs) => visit_assign_op(TokenType::Div, lhs, rhs, scope),

        Equal(lhs, rhs) => visit_bin_op(
            |v1: ChValue, v2: ChValue| Ok(ChValue::from(Primitive::Bool(v1 == v2))),
            lhs,
            rhs,
            scope,
        ),
        NotEqual(lhs, rhs) => visit_bin_op(
            |v1: ChValue, v2: ChValue| Ok(ChValue::from(Primitive::Bool(v1 != v2))),
            lhs,
            rhs,
            scope,
        ),
        Greater(lhs, rhs) => visit_bin_op(
            |v1: ChValue, v2: ChValue| Ok(ChValue::from(Primitive::Bool(v1 > v2))),
            lhs,
            rhs,
            scope,
        ),
        GreaterEq(lhs, rhs) => visit_bin_op(
            |v1: ChValue, v2: ChValue| Ok(ChValue::from(Primitive::Bool(v1 >= v2))),
            lhs,
            rhs,
            scope,
        ),
        Less(lhs, rhs) => visit_bin_op(
            |v1: ChValue, v2: ChValue| Ok(ChValue::from(Primitive::Bool(v1 < v2))),
            lhs,
            rhs,
            scope,
        ),
        LessEq(lhs, rhs) => visit_bin_op(
            |v1: ChValue, v2: ChValue| Ok(ChValue::from(Primitive::Bool(v1 <= v2))),
            lhs,
            rhs,
            scope,
        ),

        UnryAdd(n) => visit_unry_op(Ok, n, scope),
        UnryMin(n) => visit_unry_op(|v: ChValue| v * ChValue::from(Primitive::I8(-1)), n, scope),
        UnryNot(n) => visit_unry_op(
            |v: ChValue| Ok(ChValue::from(Primitive::Bool(!v.value.as_bool()))),
            n,
            scope,
        ),

        Assign(id, n) => visit_assign(id, n, scope),
        Id(_) => visit_access(node, scope),
        IdAnnot(..) => visit_define(node, scope),

        Expresssion(exprs) => visit_expr(exprs, scope),
        Eval(expr) => visit_eval(expr, scope),

        Return(expr) => visit_node(expr, scope),

        Ref(expr) => visit_ref(expr, scope),
        DeRef(expr) => visit_deref(expr, scope),

        Error(e) => panic!("found error from lexer: {}", e),
        // _ => panic!("visit_node for {:?} not implemented", node.typ),
    }
}

pub fn interpret(root: &Node) -> ChResult {
    visit_node(root, &Rc::new(RefCell::new(Scope::default())))
}
