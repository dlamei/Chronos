use std::cell::RefCell;
use std::rc::Rc;

use crate::chronos::*;
use crate::datatypes::*;
use crate::errors::*;

pub fn visit_node(node: &mut Node, scope: &mut Rc<RefCell<Scope>>) -> Result<ChValue, Error> {
    use Node::*;
    match node {
        Num(token) => visit_numb_node(token, scope),
        String(token) => visit_string_node(token, scope),
        Array(array, start, end) => visit_array_node(array, start, end, scope),
        UnryOp(op, node) => visit_unryop_node(op, node, scope),
        BinOp(left, op, right) => visit_binop_node(left, op, right, scope),
        Access(id) => visit_access_node(id, scope),
        Assign(id, value) => visit_assign_node(id, value, scope),
        If(cases, else_case) => visit_if_node(cases, else_case, scope),
        While(cond, body, start, end) => visit_while_node(cond, body, scope, start, end),
        For(c1, c2, c3, body, start, end) => visit_for_node(c1, c2, c3, body, scope, start, end),
        FuncDef(name, args, body, start, end) => {
            visit_funcdef_node(name, args, body, start, end, scope)
        }
        Call(name, args) => visit_call_node(name, args, scope),
        ArrAccess(name, indx) => visit_arraccess_node(name, indx, scope),
    }
}

fn visit_numb_node(token: &mut Token, _scope: &mut Rc<RefCell<Scope>>) -> Result<ChValue, Error> {
    match token.token_type {
        TokenType::Int(value) => Ok(ChValue::Number(ChNumber {
            value: value.into_number_type(),
            start_pos: Some(token.start_pos),
            end_pos: Some(token.end_pos),
        })),
        TokenType::Float(value) => Ok(ChValue::Number(ChNumber {
            value: value.into_number_type(),
            start_pos: Some(token.start_pos),
            end_pos: Some(token.end_pos),
        })),
        _ => panic!("called visit_numb_node on a number node that has a non number token"),
    }
}

fn visit_string_node(token: &mut Token, _scope: &mut Rc<RefCell<Scope>>) -> Result<ChValue, Error> {
    match &token.token_type {
        TokenType::String(s) => Ok(ChValue::String(ChString {
            string: s.to_string(),
            start_pos: Some(token.start_pos),
            end_pos: Some(token.end_pos),
        })),
        _ => panic!("called visit_string_node on a string node that has a non string token"),
    }
}

fn visit_access_node(token: &mut Token, scope: &mut Rc<RefCell<Scope>>) -> Result<ChValue, Error> {
    let var = &token.token_type;
    match var {
        TokenType::Id(var_name) => {
            let mut entry = scope.borrow().get(var_name);

            match &mut entry {
                Some(num) => {
                    num.set_position(Some(token.start_pos), Some(token.end_pos));
                    num.set_scope(scope.clone());
                    Ok(num.clone())
                }
                None => Err(Error::new(
                    ErrType::Runtime,
                    Some(token.start_pos),
                    Some(token.end_pos),
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
) -> Result<ChValue, Error> {
    let t = id.clone();
    let ch_type = visit_node(value, scope)?;

    match t.token_type {
        TokenType::Id(var_name) => {
            if !scope.borrow_mut().set_mut(&var_name, ch_type.clone()) {
                return Err(Error::new(
                    ErrType::Runtime,
                    ch_type.get_start(),
                    ch_type.get_end(),
                    format!("cannot assign {} to const {:?}", ch_type, var_name),
                    Some(scope.clone()),
                ));
            }
            Ok(ch_type)
        }
        _ => panic!("called visit_assign_node on {:?}", value),
    }
}

fn unryop_chvalue<T: IsChValue>(op_token: &Token, value: T) -> Result<ChValue, Error> {
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
) -> Result<ChValue, Error> {
    let mut ch_value = visit_node(node, scope)?;
    ch_value.set_position(Some(op.start_pos), ch_value.get_end());

    unryop_chvalue(op, ch_value)
}

fn binop_chvalue<T: IsChValue>(
    left: T,
    op_token: &Token,
    right: ChValue,
) -> Result<ChValue, Error> {
    use TokenType::*;
    match op_token.token_type {
        Add => left.add(right),
        Sub => left.sub(right),
        Mul => left.mult(right),
        Div => left.div(right),
        Pow => left.pow(right),
        Less => left.less(right),
        Equal => left.equal(right),
        NEqual => left.not_equal(right),
        LessEq => left.less_equal(right),
        Greater => left.greater(right),
        GreaterEq => left.greater_equal(right),
        Keywrd(Keyword::And) => left.and(right),
        Keywrd(Keyword::Or) => left.or(right),
        _ => panic!("called binop_bool on {:?}", op_token.token_type),
    }
}

fn visit_binop_node(
    left: &mut Node,
    op: &mut Token,
    right: &mut Node,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChValue, Error> {
    if matches!(op.token_type, TokenType::AddEq) || matches!(op.token_type, TokenType::SubEq) {
        return add_sub_equal(left, op, right, scope);
    }
    let mut left = visit_node(left, scope)?;
    let right = visit_node(right, scope)?;

    left.set_position(left.get_start(), right.get_end());

    let ret = binop_chvalue(left, op, right);
    if let Err(mut e) = ret {
        e.set_scope(scope.clone());
        Err(e)
    } else {
        ret
    }
}

fn add_sub_equal(
    left_node: &mut Node,
    op: &mut Token,
    right_node: &mut Node,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChValue, Error> {
    let mut left = visit_node(left_node, scope)?;
    let right = visit_node(right_node, scope)?;
    let start = left.get_start();
    let end = left.get_end();

    match left_node {
        Node::Access(var_name) => {
            left.set_position(left.get_start(), right.get_end());

            let res = match op.token_type {
                TokenType::AddEq => left.add_equal(right)?,
                TokenType::SubEq => left.sub_equal(right)?,
                _ => panic!("called add/sub_equal on {:?}", op),
            };

            let name = match &var_name.token_type {
                TokenType::Id(n) => n,
                _ => panic!("could not resolve name"),
            };

            scope.borrow_mut().set_mut(name, res.clone());
            Ok(res)
        }
        _ => Err(Error::new(
            ErrType::Runtime,
            start,
            end,
            format!("expected LVALUE, found {:?}", left_node),
            Some(scope.clone()),
        )),
    }
}

fn visit_if_node(
    cases: &mut Vec<(Node, Node)>,
    else_case: &mut Option<Box<Node>>,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChValue, Error> {
    let mut start: Option<Position> = None;
    let mut end: Option<Position> = None;
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
        _ => Ok(ChValue::None(ChNone {
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
) -> Result<ChValue, Error> {
    let mut n_scope = Scope::from_parent(String::from("<for>"), scope.clone(), Some(*start));

    if let Some(c) = c1 {
        visit_node(c, &mut n_scope)?;
    }

    while visit_node(c2, &mut n_scope)?.is_true() {
        visit_node(body, &mut n_scope)?;
        if let Some(c) = c3 {
            visit_node(c, &mut n_scope)?;
        }
    }

    Ok(ChValue::None(ChNone {
        start_pos: Some(*start),
        end_pos: Some(*end),
    }))
}

fn visit_while_node(
    condition: &mut Node,
    body: &mut Node,
    scope: &mut Rc<RefCell<Scope>>,
    start: &mut Position,
    end: &mut Position,
) -> Result<ChValue, Error> {
    let mut n_scope = Scope::from_parent(String::from("<while>"), scope.clone(), Some(*start));

    while visit_node(condition, scope)?.is_true() {
        visit_node(body, &mut n_scope)?;
    }

    Ok(ChValue::None(ChNone {
        start_pos: Some(*start),
        end_pos: Some(*end),
    }))
}

fn visit_funcdef_node(
    func_name: &mut Option<Token>,
    args: &mut Vec<Token>,
    body: &mut Node,
    start: &mut Position,
    end: &mut Position,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChValue, Error> {
    let name = match func_name {
        Some(tok) => match &tok.token_type {
            TokenType::Id(s) => s,
            _ => {
                return Err(Error::new(
                    ErrType::InvalidSyntax,
                    Some(*start),
                    Some(*end),
                    format!("expected ID found '{:?}'", tok),
                    Some(scope.clone()),
                ))
            }
        },
        _ => "lambda",
    }
    .to_string();

    let func = ChValue::Function(ChFunction {
        func_type: FuncType::ChronFunc(Box::new(ChronosFunc {
            name: name.clone(),
            args_name: args.clone(),
            body: body.clone(),
            start_pos: Some(*start),
            end_pos: Some(*end),
            scope: scope.clone(),
        })),
    });

    if func_name.is_some() {
        scope.borrow_mut().set_mut(&name, func.clone());
    }

    Ok(func)
}

fn visit_call_node(
    func_name: &mut Node,
    args: &mut Vec<Node>,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChValue, Error> {
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
        ChValue::Function(func) => func,
        _ => {
            return Err(Error::new(
                ErrType::Runtime,
                c.get_start(),
                c.get_end(),
                format!("expected Function found {}", c.get_desc()),
                Some(scope.clone()),
            ))
        }
    };

    let mut arg_values: Vec<ChValue> = Vec::new();

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
) -> Result<ChValue, Error> {
    let mut array: Vec<ChValue> = Vec::new();

    for v in vec {
        array.push(visit_node(v, scope)?);
    }

    Ok(ChValue::Array(ChArray {
        data: array,
        start_pos: Some(*start),
        end_pos: Some(*end),
    }))
}

fn visit_arraccess_node(
    arr_name: &mut Node,
    indx: &mut Node,
    scope: &mut Rc<RefCell<Scope>>,
) -> Result<ChValue, Error> {
    let name = visit_node(arr_name, scope)?;
    let index = visit_node(indx, scope)?;

    name.access(index)
}
