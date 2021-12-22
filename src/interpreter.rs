use std::cell::RefCell;
use std::rc::Rc;

use crate::chronos::*;
use crate::datatypes::*;
use crate::errors::*;

pub fn visit_node(node: &mut Node, scope: &mut Rc<RefCell<Scope>>) -> Result<ChType, Error> {
    match node {
        Node::Num(token) => visit_numb_node(token, scope),
        Node::String(token) => visit_string_node(token, scope),
        Node::Array(array, start, end) => visit_array_node(array, start, end, scope),
        Node::UnryOp(op, node) => visit_unryop_node(op, node, scope),
        Node::BinOp(left, op, right) => visit_binop_node(left, op, right, scope),
        Node::Access(id) => visit_access_node(id, scope),
        Node::Assign(id, value) => visit_assign_node(id, value, scope),
        Node::If(cases, else_case) => visit_if_node(cases, else_case, scope),
        Node::While(cond, body, start, end) => visit_while_node(cond, body, scope, start, end),
        Node::For(c1, c2, c3, body, start, end) => {
            visit_for_node(c1, c2, c3, body, scope, start, end)
        }
        Node::FuncDef(name, args, body, start, end) => {
            visit_funcdef_node(name, args, body, start, end, scope)
        }
        Node::Call(name, args) => visit_call_node(name, args, scope),
    }
}

fn visit_numb_node(token: &mut Token, _scope: &mut Rc<RefCell<Scope>>) -> Result<ChType, Error> {
    match token.token_type {
        TokenType::Int(value) => Ok(ChType::Number(ChNumber {
            value: value.into_number_type(),
            start_pos: token.start_pos.clone(),
            end_pos: token.end_pos.clone(),
        })),
        TokenType::Float(value) => Ok(ChType::Number(ChNumber {
            value: value.into_number_type(),
            start_pos: token.start_pos.clone(),
            end_pos: token.end_pos.clone(),
        })),
        _ => panic!("called visit_numb_node on a number node that has a non number token"),
    }
}

fn visit_string_node(token: &mut Token, _scope: &mut Rc<RefCell<Scope>>) -> Result<ChType, Error> {
    match &token.token_type {
        TokenType::String(s) => Ok(ChType::String(ChString {
            string: s.to_string(),
            start_pos: token.start_pos.clone(),
            end_pos: token.end_pos.clone(),
        })),
        _ => panic!("called visit_string_node on a string node that has a non string token"),
    }
}

fn visit_access_node(token: &mut Token, scope: &mut Rc<RefCell<Scope>>) -> Result<ChType, Error> {
    let var = &token.token_type;
    match var {
        TokenType::Id(var_name) => {
            let mut entry = scope.borrow().get(var_name);

            match &mut entry {
                Some(num) => {
                    num.set_position(token.start_pos.clone(), token.end_pos.clone());
                    num.set_scope(scope.clone());
                    Ok(num.clone())
                }
                None => Err(Error::new(
                    ErrType::Runtime,
                    &token.start_pos,
                    &token.end_pos,
                    format!("{:?} is not defined", var_name),
                    Some(scope.clone()),
                )),
            }
        }
        _ => panic!("called visit_access_node on a non ID token"),
    }
}

fn visit_assign_node(
    id: &mut Token,
    value: &mut Node,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChType, Error> {
    let t = id.clone();
    let ch_type = visit_node(value, scope)?;

    match t.token_type {
        TokenType::Id(var_name) => {
            if !scope.borrow_mut().set_mut(&var_name, ch_type.clone()) {
                return Err(Error::new(
                    ErrType::Runtime,
                    &ch_type.get_start(),
                    &ch_type.get_end(),
                    format!("cannot assign {} to const {:?}", ch_type, var_name),
                    Some(scope.clone()),
                ));
            }
            Ok(ch_type)
        }
        _ => panic!("called visit_assign_node on {:?}", value),
    }
}

fn unryop_chvalue<T: IsChValue>(op_token: &Token, value: T) -> Result<ChType, Error> {
    match op_token.token_type {
        TokenType::Sub => value.negate(),
        TokenType::Keywrd(Keyword::Not) => value.not(),
        _ => panic!("called unryop_self on {:?}", op_token),
    }
}

fn visit_unryop_node(
    op: &mut Token,
    node: &mut Node,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChType, Error> {
    let mut ch_type = visit_node(node, scope)?;
    ch_type.set_position(op.start_pos.clone(), ch_type.get_end());

    match ch_type {
        ChType::Number(n) => unryop_chvalue(op, n),
        ChType::String(s) => unryop_chvalue(op, s),
        ChType::Bool(n) => unryop_chvalue(op, n),
        ChType::Function(n) => unryop_chvalue(op, *n),
        ChType::Array(a) => unryop_chvalue(op, a),
        ChType::None(n) => unryop_chvalue(op, n),
    }
}

fn binop_chvalue<T: IsChValue>(left: T, op_token: &Token, right: ChType) -> Result<ChType, Error> {
    match op_token.token_type {
        TokenType::Add => left.add(right),
        TokenType::Sub => left.sub(right),
        TokenType::Mul => left.mult(right),
        TokenType::Div => left.div(right),
        TokenType::Pow => left.pow(right),
        TokenType::Less => left.less(right),
        TokenType::Equal => left.equal(right),
        TokenType::NEqual => left.not_equal(right),
        TokenType::LessEq => left.less_equal(right),
        TokenType::Greater => left.greater(right),
        TokenType::GreaterEq => left.greater_equal(right),
        TokenType::Keywrd(Keyword::And) => left.and(right),
        TokenType::Keywrd(Keyword::Or) => left.or(right),
        _ => panic!("called binop_bool on {:?}", op_token.token_type),
    }
}

fn visit_binop_node(
    left: &mut Node,
    op: &mut Token,
    right: &mut Node,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChType, Error> {
    if matches!(op.token_type, TokenType::AddEq) || matches!(op.token_type, TokenType::SubEq) {
        return in_de_crement(left, op, right, scope);
    }
    let mut left = visit_node(left, scope)?;
    let right = visit_node(right, scope)?;

    left.set_position(left.get_start(), right.get_end());

    match match left {
        ChType::Number(n) => binop_chvalue(n, op, right),
        ChType::String(s) => binop_chvalue(s, op, right),
        ChType::None(n) => binop_chvalue(n, op, right),
        ChType::Function(n) => binop_chvalue(*n, op, right),
        ChType::Bool(n) => binop_chvalue(n, op, right),
        ChType::Array(n) => binop_chvalue(n, op, right),
    } {
        Ok(res) => Ok(res),
        Err(mut e) => {
            e.set_scope(scope.clone());
            Err(e)
        }
    }
}

fn in_de_crement(
    left_node: &mut Node,
    op: &mut Token,
    right_node: &mut Node,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChType, Error> {
    let mut left = visit_node(left_node, scope)?;
    let right = visit_node(right_node, scope)?;
    let start = left.get_start();
    let end = left.get_end();

    match left_node {
        Node::Access(var_name) => {
            left.set_position(left.get_start(), right.get_end());

            match op.token_type {
                TokenType::AddEq => {
                    let res = match left {
                        ChType::Number(n) => n.add_equal(right),
                        ChType::String(s) => s.add_equal(right),
                        ChType::Array(s) => s.add_equal(right),
                        ChType::None(n) => n.add_equal(right),
                        ChType::Function(func) => func.add_equal(right),
                        ChType::Bool(b) => b.add_equal(right),
                    }?;

                    let name = match &var_name.token_type {
                        TokenType::Id(n) => n,
                        _ => panic!("could not resolve name"),
                    };

                    scope.borrow_mut().set_mut(name, res.clone());
                    Ok(res)
                }
                TokenType::SubEq => {
                    let res = match left {
                        ChType::Number(n) => n.sub_equal(right),
                        ChType::String(s) => s.sub_equal(right),
                        ChType::Array(s) => s.sub_equal(right),
                        ChType::None(n) => n.sub_equal(right),
                        ChType::Function(func) => func.sub_equal(right),
                        ChType::Bool(b) => b.sub_equal(right),
                    }?;

                    let name = match &var_name.token_type {
                        TokenType::Id(n) => n,
                        _ => panic!("could not resolve name"),
                    };

                    scope.borrow_mut().set_mut(name, res.clone());
                    Ok(res)
                }
                _ => panic!("called in/decrement on {:?}", op),
            }
        }
        _ => Err(Error::new(
            ErrType::Runtime,
            &start,
            &end,
            format!("expected LVALUE, found {:?}", left_node),
            Some(scope.clone()),
        )),
    }
}

fn visit_if_node(
    cases: &mut Vec<(Node, Node)>,
    else_case: &mut Option<Box<Node>>,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChType, Error> {
    let mut start = Position::default();
    let mut end = Position::default();
    let mut first_cond = true;

    for (condition, expr) in cases {
        let cond = visit_node(condition, scope)?;

        if first_cond {
            start = cond.get_start();
            end = cond.get_end();
            first_cond = false;
        }

        if cond.is_true() {
            return visit_node(expr, scope);
        }
    }

    match else_case {
        Some(node) => visit_node(node, scope),
        _ => Ok(ChType::None(ChNone {
            start_pos: start,
            end_pos: end,
        })),
    }
}

fn visit_for_node(
    c1: &mut Option<Box<Node>>,
    c2: &mut Box<Node>,
    c3: &mut Option<Box<Node>>,
    body: &mut Node,
    scope: &mut Rc<RefCell<Scope>>,
    start: &mut Position,
    end: &mut Position,
) -> Result<ChType, Error> {
    let mut n_scope = Scope::from_parent(String::from("<for>"), scope.clone(), start.clone());

    if let Some(c) = c1 {
        visit_node(c, &mut n_scope)?;
    }

    while visit_node(c2, &mut n_scope)?.is_true() {
        visit_node(body, &mut n_scope)?;
        if let Some(c) = c3 {
            visit_node(c, &mut n_scope)?;
        }
    }

    Ok(ChType::None(ChNone {
        start_pos: start.clone(),
        end_pos: end.clone(),
    }))
}

fn visit_while_node(
    condition: &mut Node,
    body: &mut Node,
    scope: &mut Rc<RefCell<Scope>>,
    start: &mut Position,
    end: &mut Position,
) -> Result<ChType, Error> {
    let mut n_scope = Scope::from_parent(String::from("<while>"), scope.clone(), start.clone());

    while visit_node(condition, scope)?.is_true() {
        visit_node(body, &mut n_scope)?;
    }

    Ok(ChType::None(ChNone {
        start_pos: start.clone(),
        end_pos: end.clone(),
    }))
}

fn visit_funcdef_node(
    func_name: &mut Option<Token>,
    args: &mut Vec<Token>,
    body: &mut Node,
    start: &mut Position,
    end: &mut Position,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChType, Error> {
    let name = match func_name {
        Some(tok) => match &tok.token_type {
            TokenType::Id(s) => s,
            _ => {
                return Err(Error::new(
                    ErrType::InvalidSyntax,
                    start,
                    end,
                    format!("expected ID found '{:?}'", tok),
                    Some(scope.clone()),
                ))
            }
        },
        _ => "lambda",
    }
    .to_string();

    let func = ChType::Function(Box::new(ChFunction {
        func_type: FuncType::ChronFunc(ChronosFunc {
            name: name.clone(),
            args_name: args.clone(),
            body: body.clone(),
            start_pos: start.clone(),
            end_pos: end.clone(),
            scope: scope.clone(),
        }),
    }));

    if func_name.is_some() {
        scope.borrow_mut().set_mut(&name, func.clone());
    }

    Ok(func)
}

fn visit_call_node(
    func_name: &mut Node,
    args: &mut Vec<Node>,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChType, Error> {
    let name = match func_name {
        Node::Access(tok) => {
            if let TokenType::Id(s) = &tok.token_type {
                Some(s.to_string())
            } else {
                None
            }
        }
        _ => None,
    };

    let c = visit_node(func_name, scope)?;

    let mut call = match c {
        ChType::Function(func) => func,
        _ => {
            return Err(Error::new(
                ErrType::Runtime,
                &c.get_start(),
                &c.get_end(),
                format!("expected FUNCTION found {}", c),
                Some(scope.clone()),
            ))
        }
    };

    let mut arg_values: Vec<ChType> = Vec::new();

    for arg in args {
        arg_values.push(visit_node(arg, scope)?);
    }

    call.set_scope(scope.clone());
    call.execute(arg_values, name)
}

fn visit_array_node(
    vec: &mut Vec<Node>,
    start: &mut Position,
    end: &mut Position,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChType, Error> {
    let mut array: Vec<ChType> = Vec::new();

    for v in vec {
        array.push(visit_node(v, scope)?);
    }

    Ok(ChType::Array(ChArray {
        data: array,
        start_pos: start.clone(),
        end_pos: end.clone(),
    }))
}
