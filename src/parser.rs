use crate::tokens::*;

pub enum NodeType {
    IntLiteral(i32),
    FloatLiteral(f32),

    Error(),
}

type Node = (NodeType, std::ops::Range<usize>);

fn atom(token: &Token) {
    match token.0 {
        TokenType::IntLiteral(val) => NodeType::IntLiteral(val),
        TokenType::FloatLiteral(val) => NodeType::FloatLiteral(val),

        _ => todo!()
    };
}

pub fn parse_tokens(tokens: &Vec<Token>) {
    if tokens.len() == 0 {
        return;
    }

    atom(&tokens[0])
}
