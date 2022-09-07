use crate::parser::*;
use crate::chtype::ChType;
use core::result::Result;

#[derive(Debug, PartialEq)]
pub enum RuntimeErr {
    ZeroDivisionError,
    UnsupportedOperand(String),
}

pub type RuntimeRes = Result<ChType, RuntimeErr>;

fn visit_bin_op<F>(op: F, lhs: &Node, rhs: &Node) -> RuntimeRes
where
    F: Fn(ChType, ChType) -> RuntimeRes,
{
    let left_val = visit_node(lhs)?;
    let right_val = visit_node(rhs)?;

    op(left_val, right_val)
}

fn visit_unry_op<F>(op: F, n: &Node) -> RuntimeRes
where
    F: Fn(ChType) -> RuntimeRes,
{
    let val = visit_node(n)?;
    op(val)
}

pub fn visit_node(node: &Node) ->  RuntimeRes {
    use NodeType::*;
    match &node.typ {
        I32Lit(val) => Ok(ChType::I32(*val)),
        Add(lhs, rhs) => visit_bin_op(|v1: ChType, v2: ChType| v1 + v2, lhs, rhs),
        Sub(lhs, rhs) => visit_bin_op(|v1: ChType, v2: ChType| v1 - v2, lhs, rhs),
        Mul(lhs, rhs) => visit_bin_op(|v1: ChType, v2: ChType| v1 * v2, lhs, rhs),
        Div(lhs, rhs) => visit_bin_op(|v1: ChType, v2: ChType| v1 / v2, lhs, rhs),

        Equal(lhs, rhs) => visit_bin_op(|v1: ChType, v2: ChType| Ok(ChType::Bool(v1 == v2)), lhs, rhs),
        Greater(lhs, rhs) => visit_bin_op(|v1: ChType, v2: ChType| Ok(ChType::Bool(v1 > v2)), lhs, rhs),
        GreaterEq(lhs, rhs) => visit_bin_op(|v1: ChType, v2: ChType| Ok(ChType::Bool(v1 >= v2)), lhs, rhs),
        Less(lhs, rhs) => visit_bin_op(|v1: ChType, v2: ChType| Ok(ChType::Bool(v1 < v2)), lhs, rhs),
        LessEq(lhs, rhs) => visit_bin_op(|v1: ChType, v2: ChType| Ok(ChType::Bool(v1 <= v2)), lhs, rhs),

        UnryAdd(n) => visit_unry_op(|v: ChType| Ok(v), n),
        UnryMin(n) => visit_unry_op(|v: ChType| v * ChType::I8(-1), n),
        _ => todo!(),
    }
}
