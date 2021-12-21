use crate::chronos::ChType;
use crate::chronos::*;
use crate::errors::*;
use std::{cell::RefCell, fmt, fmt::Debug, fmt::Display, rc::Rc};

pub trait HasPosition {
    fn get_start(&self) -> Position;
    fn get_end(&self) -> Position;
    fn set_position(&mut self, start_pos: Position, end_pos: Position);
}

pub trait HasScope {
    fn set_scope(&mut self, _scope: Rc<RefCell<Scope>>) {}
}

pub trait IsFunction {
    fn execute(&mut self, args: Vec<ChType>, name: Option<String>) -> Result<ChType, Error>;
}

pub trait ConvertType {
    fn into_number_type(self) -> NumberType
    where
        Self: Sized,
    {
        panic!("value can't be converted");
    }

    fn convert_to_number(self) -> Result<NumberType, Error>
    where
        Self: Sized + HasPosition,
    {
        Err(Error::new(
            ErrType::Runtime,
            &self.get_start(),
            &self.get_end(),
            String::from("could not convert to Number"),
            None,
        ))
    }

    fn get_number_type(&self) -> NumberType
    where
        Self: Sized,
    {
        panic!("value can't be converted");
    }
}

fn generate_undefined_op(caller: &dyn IsChValue, op_name: &str) -> Result<ChType, Error> {
        Err(Error::new(
            ErrType::UndefinedOperator,
            &caller.get_start(),
            &caller.get_end(),
            format!("operation '{}' not defined for type: {}", op_name, caller.get_desc()),
            None,
        ))
}

pub trait ChOperators {
    fn add(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "add")
    }
    fn add_equal(self, other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        self.add(other)
    }
    fn sub_equal(self, other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        self.sub(other)
    }
    fn sub(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "sub")
    }
    fn mult(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "mult")
    }
    fn div(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "div")
    }
    fn pow(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "pow")
    }
    fn equal(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "equal")
    }
    fn not_equal(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "not equal")
    }
    fn less(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "less")
    }
    fn less_equal(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "less equal")
    }
    fn greater(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "greater")
    }
    fn greater_equal(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "greater equal")
    }
    fn and(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "and")
    }
    fn or(self, _other: ChType) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "or")
    }
    fn not(self) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "not")
    }

    fn is_true(&self) -> bool
    where
        Self: IsChValue + Sized,
    {
        false
    }

    fn negate(self) -> Result<ChType, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "negate")
    }
}

//--------------------------None------------------------------//

#[derive(Clone, Debug)]
pub struct ChNone {
    pub start_pos: Position,
    pub end_pos: Position,
}

impl HasScope for ChNone {}

impl HasPosition for ChNone {
    fn get_start(&self) -> Position {
        self.start_pos.clone()
    }

    fn get_end(&self) -> Position {
        self.end_pos.clone()
    }

    fn set_position(&mut self, start_pos: Position, end_pos: Position) {
        self.start_pos = start_pos;
        self.end_pos = end_pos;
    }
}

impl ChOperators for ChNone {
    fn equal(self, other: ChType) -> Result<ChType, Error> {
        Ok(ChBool {
            value: matches!(other, ChType::None(_)),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn not_equal(self, other: ChType) -> Result<ChType, Error> {
        Ok(ChBool {
            value: !matches!(other, ChType::None(_)),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn is_true(&self) -> bool {
        false
    }
}

impl IsChValue for ChNone {
    fn get_desc(&self) -> String {
        String::from("None")
    }

    fn into_type(self) -> ChType {
        ChType::None(self)
    }
}

impl ConvertType for ChNone {
    fn convert_to_number(self) -> Result<NumberType, Error> {
        Err(Error::new(
            ErrType::Runtime,
            &self.get_start(),
            &self.get_end(),
            String::from("can not convert 'none' to Numbertype"),
            None,
        ))
    }
}

impl Display for ChNone {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "none")
    }
}

//--------------------------Boolean------------------------------//

#[derive(Clone, Debug)]
pub struct ChBool {
    pub value: bool,
    pub start_pos: Position,
    pub end_pos: Position,
}

impl ChBool {
    pub fn from(v: bool) -> Self {
        ChBool {
            value: v,
            start_pos: Position::default(),
            end_pos: Position::default(),
        }
    }
}

impl ConvertType for ChBool {
    fn into_number_type(self) -> NumberType {
        NumberType::Int(self.value as i32)
    }

    fn convert_to_number(self) -> Result<NumberType, Error> {
        Ok(NumberType::Int(self.value as i32))
    }

    fn get_number_type(&self) -> NumberType {
        NumberType::Int(self.value as i32)
    }
}

impl IsChValue for ChBool {
    fn get_desc(&self) -> String {
        String::from("Bool")
    }

    fn into_type(self) -> ChType {
        ChType::Bool(self)
    }
}

impl HasScope for ChBool {}

impl Display for ChBool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", if self.value { "true" } else { "false" })
    }
}

impl HasPosition for ChBool {
    fn get_start(&self) -> Position {
        self.start_pos.clone()
    }

    fn get_end(&self) -> Position {
        self.end_pos.clone()
    }

    fn set_position(&mut self, start_pos: Position, end_pos: Position) {
        self.start_pos = start_pos;
        self.end_pos = end_pos;
    }
}

impl ChOperators for ChBool {
    fn equal(self, other: ChType) -> Result<ChType, Error> {
        Ok(ChBool {
            value: match other {
                ChType::Bool(b) => self.value == b.value,
                _ => false,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn not_equal(self, other: ChType) -> Result<ChType, Error> {
        Ok(ChBool {
            value: match other {
                ChType::Bool(b) => self.value != b.value,
                _ => true,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn not(mut self) -> Result<ChType, Error> {
        self.value = !self.value;
        Ok(self.into_type())
    }

    fn is_true(&self) -> bool {
        self.value
    }

    fn and(self, other: ChType) -> Result<ChType, Error> {
        Ok(ChBool {
            value: self.value && other.is_true(),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn or(self, other: ChType) -> Result<ChType, Error> {
        Ok(ChBool {
            value: self.value || other.is_true(),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }
}



//--------------------------Number------------------------------//

#[derive(Debug, Clone)]
pub struct ChNumber {
    pub value: NumberType,
    pub start_pos: Position,
    pub end_pos: Position,
}

impl HasScope for ChNumber {}

impl IsChValue for ChNumber {
    fn get_desc(&self) -> String {
        String::from("Number")
    }

    fn into_type(self) -> ChType {
        ChType::Number(self)
    }
}

impl Display for ChNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.value {
            NumberType::Int(v) => write!(f, "{}", v),
            NumberType::Float(v) => write!(f, "{}", v),
        }
    }
}

impl ConvertType for ChNumber {
    fn into_number_type(self) -> NumberType {
        self.value
    }

    fn get_number_type(&self) -> NumberType {
        self.value.clone()
    }

    fn convert_to_number(self) -> Result<NumberType, Error> {
        Ok(self.value)
    }
}

impl HasPosition for ChNumber {
    fn get_start(&self) -> Position {
        self.start_pos.clone()
    }

    fn get_end(&self) -> Position {
        self.end_pos.clone()
    }

    fn set_position(&mut self, start_pos: Position, end_pos: Position) {
        self.start_pos = start_pos;
        self.end_pos = end_pos;
    }
}

impl ChNumber {
    fn from(value: NumberType, _scope: &mut Rc<RefCell<Scope>>) -> Self {
        ChNumber {
            value,
            start_pos: Position::default(),
            end_pos: Position::default(),
        }
    }

    fn into_token(self) -> Token {
        match self.value {
            NumberType::Int(v) => Token::new(TokenType::Int(v), self.start_pos, Some(self.end_pos)),
            NumberType::Float(v) => {
                Token::new(TokenType::Float(v), self.start_pos, Some(self.end_pos))
            }
        }
    }

    fn operate_on(
        mut self,
        other: NumberType,
        int_op: fn(ChInt, ChInt) -> ChInt,
        float_op: fn(ChFloat, ChFloat) -> ChFloat,
    ) -> Self {
        self.value = match (self.value, other) {
            (NumberType::Int(v1), NumberType::Int(v2)) => NumberType::Int(int_op(v1, v2)),
            (NumberType::Float(v1), NumberType::Int(v2)) => {
                NumberType::Float(float_op(v1, v2 as ChFloat))
            }
            (NumberType::Int(v1), NumberType::Float(v2)) => {
                NumberType::Float(float_op(v1 as ChFloat, v2))
            }
            (NumberType::Float(v1), NumberType::Float(v2)) => NumberType::Float(float_op(v1, v2)),
        };

        self
    }
}

impl ChOperators for ChNumber {
    fn add(self, other: ChType) -> Result<ChType, Error> {
        Ok(self
            .operate_on(
                other.convert_to_number()?,
                |v1: ChInt, v2: ChInt| v1 + v2,
                |v1: ChFloat, v2: ChFloat| v1 + v2,
            )
            .into_type())
    }

    fn sub(self, other: ChType) -> Result<ChType, Error> {
        Ok(self
            .operate_on(
                other.convert_to_number()?,
                |v1: ChInt, v2: ChInt| v1 - v2,
                |v1: ChFloat, v2: ChFloat| v1 - v2,
            )
            .into_type())
    }

    fn mult(self, other: ChType) -> Result<ChType, Error> {
        Ok(self
            .operate_on(
                other.convert_to_number()?,
                |v1: ChInt, v2: ChInt| v1 * v2,
                |v1: ChFloat, v2: ChFloat| v1 * v2,
            )
            .into_type())
    }

    fn div(self, other: ChType) -> Result<ChType, Error> {
        if match other.clone().convert_to_number()? {
            NumberType::Int(v) => v == 0,
            NumberType::Float(v) => v == 0.0,
        } {
            Err(Error::new(
                ErrType::Runtime,
                &self.start_pos,
                &self.end_pos,
                String::from("Division by 0"),
                None,
            ))
        } else {
            Ok(self
                .operate_on(
                    other.convert_to_number()?,
                    |v1: ChInt, v2: ChInt| v1 / v2,
                    |v1: ChFloat, v2: ChFloat| v1 / v2,
                )
                .into_type())
        }
    }

    fn pow(mut self, other: ChType) -> Result<ChType, Error> {
        if match other.clone().convert_to_number()? {
            NumberType::Int(v) => v == 0,
            _ => false,
        } {
            self.value = 0.into_number_type();
            Ok(self.into_type())
        } else {
            Ok(self
                .operate_on(
                    other.convert_to_number()?,
                    |v1: ChInt, v2: ChInt| v1.pow(v2.try_into().unwrap_or(0)),
                    |v1: ChFloat, v2: ChFloat| v1.powf(v2),
                )
                .into_type())
        }
    }

    fn equal(self, other: ChType) -> Result<ChType, Error> {
        let value = other.convert_to_number();

        Ok(ChBool {
            value: match value {
                Ok(v) => match (self.value, v) {
                    (NumberType::Int(v1), NumberType::Int(v2)) => v1 == v2,
                    (NumberType::Float(v1), NumberType::Float(v2)) => v1 == v2,
                    (NumberType::Int(v1), NumberType::Float(v2)) => v1 as ChFloat == v2,
                    (NumberType::Float(v1), NumberType::Int(v2)) => v1 == v2 as ChFloat,
                },
                Err(_) => false,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn not_equal(self, other: ChType) -> Result<ChType, Error> {
        let value = other.convert_to_number();

        Ok(ChBool {
            value: match value {
                Ok(v) => match (self.value, v) {
                    (NumberType::Int(v1), NumberType::Int(v2)) => v1 != v2,
                    (NumberType::Float(v1), NumberType::Float(v2)) => v1 != v2,
                    (NumberType::Int(v1), NumberType::Float(v2)) => v1 as ChFloat != v2,
                    (NumberType::Float(v1), NumberType::Int(v2)) => v1 != v2 as ChFloat,
                },
                Err(_) => false,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn less(self, other: ChType) -> Result<ChType, Error> {
        let value = other.convert_to_number()?;

        Ok(ChBool {
            value: match (self.value, value) {
                (NumberType::Int(v1), NumberType::Int(v2)) => v1 < v2,
                (NumberType::Float(v1), NumberType::Float(v2)) => v1 < v2,
                (NumberType::Int(v1), NumberType::Float(v2)) => (v1 as ChFloat) < v2,
                (NumberType::Float(v1), NumberType::Int(v2)) => v1 < v2 as ChFloat,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn less_equal(self, other: ChType) -> Result<ChType, Error> {
        let value = other.convert_to_number()?;

        Ok(ChBool {
            value: match (self.value, value) {
                (NumberType::Int(v1), NumberType::Int(v2)) => v1 <= v2,
                (NumberType::Float(v1), NumberType::Float(v2)) => v1 <= v2,
                (NumberType::Int(v1), NumberType::Float(v2)) => (v1 as ChFloat) <= v2,
                (NumberType::Float(v1), NumberType::Int(v2)) => v1 <= v2 as ChFloat,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn greater(self, other: ChType) -> Result<ChType, Error> {
        let value = other.convert_to_number()?;

        Ok(ChBool {
            value: match (self.value, value) {
                (NumberType::Int(v1), NumberType::Int(v2)) => v1 > v2,
                (NumberType::Float(v1), NumberType::Float(v2)) => v1 > v2,
                (NumberType::Int(v1), NumberType::Float(v2)) => (v1 as ChFloat) > v2,
                (NumberType::Float(v1), NumberType::Int(v2)) => v1 > v2 as ChFloat,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn greater_equal(self, other: ChType) -> Result<ChType, Error> {
        let value = other.convert_to_number()?;

        Ok(ChBool {
            value: match (self.value, value) {
                (NumberType::Int(v1), NumberType::Int(v2)) => v1 >= v2,
                (NumberType::Float(v1), NumberType::Float(v2)) => v1 >= v2,
                (NumberType::Int(v1), NumberType::Float(v2)) => (v1 as ChFloat) >= v2,
                (NumberType::Float(v1), NumberType::Int(v2)) => v1 >= v2 as ChFloat,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn and(self, other: ChType) -> Result<ChType, Error> {
        let value = other.convert_to_number()?;

        Ok(ChBool {
            value: match (self.value, value) {
                (NumberType::Int(v1), NumberType::Int(v2)) => v1 >= 1 && v2 >= 1,
                (NumberType::Float(v1), NumberType::Float(v2)) => v1 >= 1.0 && v2 >= 1.0,
                (NumberType::Int(v1), NumberType::Float(v2)) => v1 >= 1 && v2 >= 1.0,
                (NumberType::Float(v1), NumberType::Int(v2)) => v1 >= 1.0 && v2 >= 1,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn or(self, other: ChType) -> Result<ChType, Error> {
        let value = other.convert_to_number()?;

        Ok(ChBool {
            value: match (self.value, value) {
                (NumberType::Int(v1), NumberType::Int(v2)) => v1 >= 1 || v2 >= 1,
                (NumberType::Float(v1), NumberType::Float(v2)) => v1 >= 1.0 || v2 >= 1.0,
                (NumberType::Int(v1), NumberType::Float(v2)) => v1 >= 1 || v2 >= 1.0,
                (NumberType::Float(v1), NumberType::Int(v2)) => v1 >= 1.0 || v2 >= 1,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn not(mut self) -> Result<ChType, Error> {
        self.value = match self.value {
            NumberType::Int(value) => if value != 0 { 0 } else { 1 }.into_number_type(),
            NumberType::Float(value) => if value != 0.0 { 0.0 } else { 1.0 }.into_number_type(),
        };
        Ok(ChBool {
            value: self.is_true(),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn is_true(&self) -> bool {
        match self.value {
            NumberType::Int(value) => value != 0,
            NumberType::Float(value) => value != 0.0,
        }
    }

    fn negate(mut self) -> Result<ChType, Error> {
        match self.value {
            NumberType::Int(v) => self.value = NumberType::Int(-v),
            NumberType::Float(v) => self.value = NumberType::Float(-v),
        }
        Ok(self.into_type())
    }
}

//--------------------------String------------------------------//

#[derive(Debug, Clone)]
pub struct ChString {
    pub string: String,
    pub start_pos: Position,
    pub end_pos: Position,
}

impl Display for ChString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.string)
    }
}

impl HasScope for ChString {}

impl ConvertType for ChString {}

impl IsChValue for ChString {
    fn into_type(self) -> ChType {
        ChType::String(self)
    }

    fn get_desc(&self) -> String {
        String::from("String")
    }
}

impl HasPosition for ChString {
    fn get_start(&self) -> Position {
        self.start_pos.clone()
    }

    fn get_end(&self) -> Position {
        self.end_pos.clone()
    }

    fn set_position(&mut self, start_pos: Position, end_pos: Position) {
        self.start_pos = start_pos;
        self.end_pos = end_pos;
    }
}

impl ChOperators for ChString {
    fn is_true(&self) -> bool {
        !self.string.is_empty()
    }

    fn add(mut self, other: ChType) -> Result<ChType, Error> {
        let other_string = match other {
            ChType::Number(n) => format!("{}", n),
            ChType::String(s) => s.to_string(),
            _ => {
                return Err(Error::new(
                    ErrType::Runtime,
                    &self.start_pos,
                    &self.end_pos,
                    format!("can't add {:?} to String", other),
                    None,
                ))
            }
        };

        self.string += &other_string;
        Ok(ChType::String(self))
    }

    fn mult(mut self, other: ChType) -> Result<ChType, Error> {
        let n = other.into_number_type();

        match n {
            NumberType::Int(v) => {
                self.string = self.string.repeat(v.try_into().unwrap());
                Ok(self.into_type())
            }
            _ => Err(Error::new(
                ErrType::UndefinedOperator,
                &self.start_pos,
                &self.end_pos,
                String::from("multiply is only defined for type Int"),
                None,
            )),
        }
    }
}

//--------------------------Function------------------------------//

#[derive(Clone, Debug)]
pub struct ChFunction {
    pub func_type: FuncType,
}

impl Display for ChFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.func_type {
            FuncType::ChronFunc(func) => write!(f, "{}", func),
            FuncType::RustFunc(func) => write!(f, "{}", func),
        }
    }
}

impl HasScope for ChFunction {
    fn set_scope(&mut self, scope: Rc<RefCell<Scope>>) {
        match &mut self.func_type {
            FuncType::ChronFunc(func) => func.set_scope(scope),
            FuncType::RustFunc(func) => func.set_scope(scope),
        }
    }
}

impl ConvertType for ChFunction {}

impl HasPosition for ChFunction {
    fn get_start(&self) -> Position {
        match &self.func_type {
            FuncType::ChronFunc(func) => func.get_start(),
            FuncType::RustFunc(func) => Position::from_name(func.name.clone()),
        }
    }

    fn get_end(&self) -> Position {
        match &self.func_type {
            FuncType::ChronFunc(func) => func.get_end(),
            FuncType::RustFunc(func) => Position::from_name(func.name.clone()),
        }
    }

    fn set_position(&mut self, start_pos: Position, end_pos: Position) {
        match &mut self.func_type {
            FuncType::ChronFunc(func) => func.set_position(start_pos, end_pos),
            FuncType::RustFunc(_) => (),
        }
    }
}

impl IsChValue for ChFunction {
    fn get_desc(&self) -> String {
        String::from("function")
    }

    fn into_type(self) -> ChType {
        ChType::Function(self.into())
    }
}

impl ChOperators for ChFunction {
    fn is_true(&self) -> bool {
        true
    }
}

impl IsFunction for ChFunction {
    fn execute(&mut self, args: Vec<ChType>, name: Option<String>) -> Result<ChType, Error> {
        match &mut self.func_type {
            FuncType::ChronFunc(func) => func.execute(args, name),
            FuncType::RustFunc(func) => func.execute(args, name),
        }
    }
}


#[allow(clippy::large_enum_variant)]
#[derive(Clone, Debug)]
pub enum FuncType {
    RustFunc(RustFunc),
    ChronFunc(ChronosFunc),
}

#[derive(Clone)]
pub struct RustFunc {
    pub name: String,
    pub function: fn(args: Vec<ChType>, name: Option<String>) -> Result<ChType, Error>,
}

impl HasScope for RustFunc {
    fn set_scope(&mut self, _scope: Rc<RefCell<Scope>>) {}
}

impl IsFunction for RustFunc {
    fn execute(&mut self, args: Vec<ChType>, name: Option<String>) -> Result<ChType, Error> {
        (self.function)(args, name)
    }
}

impl Display for RustFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "rust function<{}>", self.name)
    }
}

impl Debug for RustFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "function implemented in rust")
    }
}

#[derive(Clone, Debug)]
pub struct ChronosFunc {
    pub name: String,
    pub args_name: Vec<Token>,
    pub body: Node,
    pub start_pos: Position,
    pub end_pos: Position,
    pub scope: Rc<RefCell<Scope>>,
}

impl IsFunction for ChronosFunc {
    fn execute(&mut self, mut args: Vec<ChType>, name: Option<String>) -> Result<ChType, Error> {
        let mut n_scope = Scope::from_parent(
            format!("<function: {}>", name.unwrap_or_else(|| self.name.clone())),
            self.scope.clone(),
            self.start_pos.clone(),
        );

        if args.len() != self.args_name.len() {
            return Err(Error::new(
                ErrType::Runtime,
                &self.start_pos,
                &self.end_pos,
                format!(
                    "expected {} arguments, found {} in function '{}'",
                    self.args_name.len(),
                    args.len(),
                    self.name,
                ),
                Some(self.scope.clone()),
            ));
        }

        for i in 0..args.len() {
            let value = args.get_mut(i).unwrap();
            let n = &self.args_name[i];
            let name = match &n.token_type {
                TokenType::Id(s) => s,
                _ => {
                    return Err(Error::new(
                        ErrType::Runtime,
                        &n.start_pos,
                        &n.end_pos,
                        format!("could not resolve {:?} as an argument", n),
                        Some(n_scope.clone()),
                    ))
                }
            };
            value.set_scope(self.scope.clone());
            n_scope.borrow_mut().set_mut(name, value.clone());
        }

        visit_node(&mut self.body, &mut n_scope)
    }
}

impl Display for ChronosFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "function<{}, {:?}>", self.name, self.args_name)
    }
}

impl HasScope for ChronosFunc {
    fn set_scope(&mut self, scope: Rc<RefCell<Scope>>) {
        self.scope = scope;
    }
}

impl HasPosition for ChronosFunc {
    fn get_start(&self) -> Position {
        self.start_pos.clone()
    }

    fn get_end(&self) -> Position {
        self.end_pos.clone()
    }

    fn set_position(&mut self, start_pos: Position, end_pos: Position) {
        self.start_pos = start_pos;
        self.end_pos = end_pos;
    }
}

