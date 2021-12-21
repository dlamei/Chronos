use crate::chronos::*;
use crate::errors::*;

pub struct Parser {
    tokens: Vec<Token>,
    token_index: usize,
    current_token: Token,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let t = tokens[0].clone();
        Parser {
            tokens,
            token_index: 0,
            current_token: t,
        }
    }

    pub fn parse(&mut self) -> Result<Node, Error> {
        let nodes = self.expression()?;

        match self.current_token.token_type {
            TokenType::Eof => Ok(nodes),

            _ => Err(Error::new(
                ErrType::InvalidSyntax,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!(
                    "Parser: expected EOF found {:?}",
                    self.current_token.token_type
                ),
                None,
            )),
        }
    }

    fn advance(&mut self) {
        self.token_index += 1;

        if self.token_index < self.tokens.len() {
            self.current_token = self.tokens[self.token_index].clone();
        }
    }

    fn retreat(&mut self) {
        self.token_index -= 1;
        self.current_token = self.tokens[self.token_index].clone();
    }

    fn atom(&mut self) -> Result<Node, Error> {
        let t = self.current_token.clone();

        return match t.token_type {
            TokenType::Int(_) | TokenType::Float(_) => {
                self.advance();
                Ok(Node::Num(t))
            }
            TokenType::String(_) => {
                self.advance();
                Ok(Node::String(t))
            }
            TokenType::Id(_) => {
                self.advance();
                Ok(Node::Access(t))
            }
            TokenType::LRound => {
                self.advance();
                let expr = self.expression()?;
                match self.current_token.token_type {
                    TokenType::RRound => {
                        self.advance();
                        Ok(expr)
                    }
                    _ => Err(Error::new(
                        ErrType::InvalidSyntax,
                        &t.start_pos,
                        &self.current_token.end_pos,
                        format!(
                            "Parser: expected ')' found {:?}",
                            self.current_token.token_type
                        ),
                        None,
                    )),
                }
            }
            TokenType::Keywrd(Keyword::If) => self.if_expression(),
            TokenType::Keywrd(Keyword::While) => self.while_expression(),
            TokenType::Keywrd(Keyword::For) => self.for_expression(),
            TokenType::Keywrd(Keyword::Func) => self.func_expression(),
            _ => Err(Error::new(
                ErrType::InvalidSyntax,
                &t.start_pos,
                &t.end_pos,
                format!(
                    "Parser: expected INT, FLOAT, IDENTIFIER, '+', '-' or '(, found: {:?}",
                    t.token_type
                ),
                None,
            )),
        };
    }

    fn power(&mut self) -> Result<Node, Error> {
        self.binary_operation(
            Parser::call,
            vec![TokenType::Pow],
            Vec::new(),
            Parser::factor,
        )
    }

    fn call(&mut self) -> Result<Node, Error> {
        let res = self.atom()?;

        if matches!(self.current_token.token_type, TokenType::LRound) {
            self.advance();
            let mut arg_nodes: Vec<Node> = Vec::new();

            if !matches!(self.current_token.token_type, TokenType::RRound) {
                arg_nodes.push(self.expression()?);

                while matches!(self.current_token.token_type, TokenType::Comma) {
                    self.advance();
                    arg_nodes.push(self.expression()?);
                }

                if !matches!(self.current_token.token_type, TokenType::RRound,) {
                    return Err(Error::new(
                        ErrType::InvalidSyntax,
                        &self.current_token.start_pos,
                        &self.current_token.end_pos,
                        format!("Parser: expected RROUND found '{:?}'", self.current_token),
                        None,
                    ));
                }
            }

            self.advance();
            Ok(Node::Call(res.into(), arg_nodes))
        } else {
            Ok(res)
        }
    }

    fn factor(&mut self) -> Result<Node, Error> {
        let t = self.current_token.clone();

        match t.token_type {
            TokenType::Sub | TokenType::Add => {
                self.advance();
                let factor = self.factor()?;
                Ok(Node::UnryOp(t, factor.into()))
            }
            _ => self.power(),
        }
    }

    fn binary_operation(
        &mut self,
        func_a: fn(parser: &mut Parser) -> Result<Node, Error>,
        ops: Vec<TokenType>,
        keywords: Vec<Keyword>,
        func_b: fn(parser: &mut Parser) -> Result<Node, Error>,
    ) -> Result<Node, Error> {
        let mut left_node = func_a(self)?;

        let res = {
            let mut found = false;
            for t in &ops {
                if match_enum_type(t, &self.current_token.token_type) {
                    found = true;
                    break;
                }
            }
            if !found {
                if let TokenType::Keywrd(k) = &self.current_token.token_type {
                    for key in &keywords {
                        if match_enum_type(key, k) {
                            found = true;
                            break;
                        }
                    }
                }
            }
            found
        };
        if res {
            let op_token = self.current_token.clone();
            self.advance();
            let right_node = func_b(self)?;

            left_node = Node::BinOp(left_node.into(), op_token, right_node.into());
        }

        Ok(left_node)
    }

    fn expect_token(&self, token: TokenType) -> Result<(), Error> {
        if !match_enum_type(&self.current_token.token_type, &token) {
            Err(Error::new(
                ErrType::InvalidSyntax,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!(
                    "Parser: expected {:?} found '{:?}'",
                    token, self.current_token
                ),
                None,
            ))
        } else {
            Ok(())
        }
    }

    fn if_expression(&mut self) -> Result<Node, Error> {
        let mut cases: Vec<(Node, Node)> = Vec::new();
        let mut else_case = None;

        if let TokenType::Keywrd(Keyword::If) = self.current_token.token_type {
        } else {
            return Err(Error::new(
                ErrType::InvalidSyntax,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!("Parser: expected IF found '{:?}'", self.current_token),
                None,
            ));
        }

        self.advance();

        let condition = self.expression()?;

        if !match_enum_type(&self.current_token.token_type, &TokenType::LCurly) {
            return Err(Error::new(
                ErrType::InvalidSyntax,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!("Parser: expected LCURLY found '{:?}'", self.current_token),
                None,
            ));
        }

        self.advance();
        let expr = self.expression()?;
        cases.push((condition, expr));

        if !matches!(self.current_token.token_type, TokenType::RCurly) {
            return Err(Error::new(
                ErrType::InvalidSyntax,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!("Parser: expected RCURLY found '{:?}'", self.current_token),
                None,
            ));
        }
        self.advance();

        while matches!(
            self.current_token.token_type,
            TokenType::Keywrd(Keyword::Elif)
        ) {
            self.advance();
            let cond = self.expression()?;

            if !matches!(self.current_token.token_type, TokenType::LCurly) {
                return Err(Error::new(
                    ErrType::InvalidSyntax,
                    &self.current_token.start_pos,
                    &self.current_token.end_pos,
                    format!("Parser: expected LCURLY found '{:?}'", self.current_token),
                    None,
                ));
            }
            self.advance();

            let expr = self.expression()?;
            cases.push((cond, expr));

            if !matches!(self.current_token.token_type, TokenType::RCurly) {
                return Err(Error::new(
                    ErrType::InvalidSyntax,
                    &self.current_token.start_pos,
                    &self.current_token.end_pos,
                    format!("Parser: expected RCURLY found '{:?}'", self.current_token),
                    None,
                ));
            }
            self.advance();
        }

        if matches!(
            self.current_token.token_type,
            TokenType::Keywrd(Keyword::Else)
        ) {
            self.advance();
            if !matches!(self.current_token.token_type, TokenType::LCurly) {
                return Err(Error::new(
                    ErrType::InvalidSyntax,
                    &self.current_token.start_pos,
                    &self.current_token.end_pos,
                    format!("Parser: expected LCURLY found '{:?}'", self.current_token),
                    None,
                ));
            }
            self.advance();

            else_case = Some(Box::new(self.expression()?));

            if !matches!(&self.current_token.token_type, &TokenType::RCurly) {
                return Err(Error::new(
                    ErrType::InvalidSyntax,
                    &self.current_token.start_pos,
                    &self.current_token.end_pos,
                    format!("Parser: expected RCURLY found '{:?}'", self.current_token),
                    None,
                ));
            }

            self.advance();
        }

        Ok(Node::If(cases, else_case))
    }

    fn func_expression(&mut self) -> Result<Node, Error> {
        let mut start: Option<Position> = None;
        let end: Option<Position>;

        if !matches!(
            self.current_token.token_type,
            TokenType::Keywrd(Keyword::Func)
        ) {
            return Err(Error::new(
                ErrType::InvalidSyntax,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!("Parser: expected FUNC found '{:?}'", self.current_token),
                None,
            ));
        }

        self.advance();

        let mut var_name: Option<Token> = None;

        if matches!(self.current_token.token_type, TokenType::Id(_)) {
            var_name = Some(self.current_token.clone());
            start = Some(self.current_token.start_pos.clone());
            self.advance();
        }

        self.expect_token(TokenType::LRound)?;
        self.advance();

        let mut arg_tokens: Vec<Token> = Vec::new();

        if matches!(self.current_token.token_type, TokenType::Id(_),) {
            arg_tokens.push(self.current_token.clone());
            if start.is_none() {
                start = Some(self.current_token.start_pos.clone());
            }

            self.advance();

            while matches!(self.current_token.token_type, TokenType::Comma) {
                self.advance();
                self.expect_token(TokenType::Id(String::from("")))?;

                arg_tokens.push(self.current_token.clone());
                self.advance();
            }
        }
        self.expect_token(TokenType::RRound)?;

        self.advance();

        self.expect_token(TokenType::LCurly)?;

        if start.is_none() {
            start = Some(self.current_token.start_pos.clone());
        }

        self.advance();

        let body = self.expression()?;

        self.expect_token(TokenType::RCurly)?;

        end = Some(self.current_token.end_pos.clone());

        self.advance();

        Ok(Node::FuncDef(
            var_name,
            arg_tokens,
            body.into(),
            start.unwrap_or_default(),
            end.unwrap_or_default(),
        ))
    }

    fn for_expression(&mut self) -> Result<Node, Error> {
        let mut c1: Option<Box<Node>> = None;
        let mut c3: Option<Box<Node>> = None;

        let start: Position;
        let end: Position;

        if !matches!(
            self.current_token.token_type,
            TokenType::Keywrd(Keyword::For)
        ) {
            return Err(Error::new(
                ErrType::InvalidSyntax,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!("Parser: expected FOR found '{:?}'", self.current_token),
                None,
            ));
        }

        self.advance();

        start = self.current_token.start_pos.clone();
        if !match_enum_type(&self.current_token.token_type, &TokenType::Semicln) {
            c1 = Some(self.expression()?.into());
        }

        self.expect_token(TokenType::Semicln)?;
        self.advance();

        let c2 = self.expression()?;

        self.expect_token(TokenType::Semicln)?;
        self.advance();

        if !match_enum_type(&self.current_token.token_type, &TokenType::LCurly) {
            c3 = Some(self.expression()?.into());
        }

        self.expect_token(TokenType::LCurly)?;
        self.advance();

        let body = self.expression()?;

        self.expect_token(TokenType::RCurly)?;
        end = self.current_token.start_pos.clone();
        self.advance();

        Ok(Node::For(c1, c2.into(), c3, body.into(), start, end))
    }

    fn while_expression(&mut self) -> Result<Node, Error> {
        if !matches!(
            self.current_token.token_type,
            TokenType::Keywrd(Keyword::While)
        ) {
            return Err(Error::new(
                ErrType::InvalidSyntax,
                &self.current_token.start_pos,
                &self.current_token.end_pos,
                format!("Parser: expected WHILE found '{:?}'", self.current_token),
                None,
            ));
        }

        let start = self.current_token.start_pos.clone();

        self.advance();
        let cond = self.expression()?;

        self.expect_token(TokenType::LCurly)?;
        self.advance();

        let body = self.expression()?;

        self.expect_token(TokenType::RCurly)?;

        let end = self.current_token.end_pos.clone();

        self.advance();

        Ok(Node::While(cond.into(), body.into(), start, end))
    }

    fn arith_expression(&mut self) -> Result<Node, Error> {
        self.binary_operation(
            Parser::term,
            vec![TokenType::Add, TokenType::Sub],
            Vec::new(),
            Parser::term,
        )
    }

    fn comp_expression(&mut self) -> Result<Node, Error> {
        match self.current_token.token_type {
            TokenType::Keywrd(Keyword::Not) => {
                let op = self.current_token.clone();
                self.advance();
                let node = self.comp_expression()?;
                Ok(Node::UnryOp(op, Box::new(node)))
            }

            _ => match self.binary_operation(
                Parser::arith_expression,
                vec![
                    TokenType::Equal,
                    TokenType::AddEq,
                    TokenType::SubEq,
                    TokenType::NEqual,
                    TokenType::Less,
                    TokenType::LessEq,
                    TokenType::Greater,
                    TokenType::GreaterEq,
                ],
                Vec::new(),
                Parser::arith_expression,
            ) {
                Ok(node) => Ok(node),
                Err(e) => Err(e),
                //TODO: look at error handling again
                //Err(_) => Err(Error::new(
                //    ErrType::InvalidSyntaxError,
                //    &self.current_token.start_pos,
                //    &self.current_token.end_pos,
                //    format!("Parser: expected INT, FLOAT, IDENTIFIER, '+', '-', '(' or '!'"),
                //    None,
                //)),
            },
        }
    }

    fn term(&mut self) -> Result<Node, Error> {
        self.binary_operation(
            Parser::factor,
            vec![TokenType::Mul, TokenType::Div],
            Vec::new(),
            Parser::factor,
        )
    }

    fn expression(&mut self) -> Result<Node, Error> {
        match self.current_token.token_type {
            TokenType::Id(_) => {
                let var = self.current_token.clone();
                self.advance();

                match self.current_token.token_type {
                    TokenType::Assign => {
                        self.advance();
                        Ok(Node::Assign(var, Box::new(self.expression()?)))
                    }
                    _ => {
                        self.retreat();
                        self.binary_operation(
                            Parser::comp_expression,
                            Vec::new(),
                            vec![Keyword::And, Keyword::Or],
                            Parser::comp_expression,
                        )
                    }
                }
            }
            _ => self.binary_operation(
                Parser::comp_expression,
                Vec::new(),
                vec![Keyword::And, Keyword::Or],
                Parser::comp_expression,
            ),
        }
    }
}
