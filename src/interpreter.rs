use crate::parser::*;

fn visit_bin_op<F>(op: F, lhs: &Node, rhs: &Node) -> i32
where
    F: Fn(i32, i32) -> i32,
{
    let left_val = visit_node(lhs);
    let right_val = visit_node(rhs);

    op(left_val, right_val)
}

pub fn visit_node(node: &Node) -> i32 {
    use NodeType::*;
    match &node.typ {
        I32Lit(val) => *val,
        Add(lhs, rhs) => visit_bin_op(|v1: i32, v2: i32| v1 + v2, lhs, rhs),
        Min(lhs, rhs) => visit_bin_op(|v1: i32, v2: i32| v1 - v2, lhs, rhs),
        Mul(lhs, rhs) => visit_bin_op(|v1: i32, v2: i32| v1 * v2, lhs, rhs),
        Div(lhs, rhs) => visit_bin_op(|v1: i32, v2: i32| v1 / v2, lhs, rhs),

        Equal(lhs, rhs) => visit_bin_op(|v1: i32, v2: i32| (v1 == v2) as i32, lhs, rhs),
        Greater(lhs, rhs) => visit_bin_op(|v1: i32, v2: i32| (v1 > v2) as i32, lhs, rhs),
        GreaterEq(lhs, rhs) => visit_bin_op(|v1: i32, v2: i32| (v1 >= v2) as i32, lhs, rhs),
        Less(lhs, rhs) => visit_bin_op(|v1: i32, v2: i32| (v1 < v2) as i32, lhs, rhs),
        LessEq(lhs, rhs) => visit_bin_op(|v1: i32, v2: i32| (v1 <= v2) as i32, lhs, rhs),

        _ => todo!(),
    }
}
