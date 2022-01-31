use crate::chronos::*;
use crate::errors::*;
use crate::interpreter::visit_node;
use std::{cell::RefCell, fmt, fmt::Debug, fmt::Display, rc::Rc};

pub trait HasPosition {
    fn get_start(&self) -> Option<Position>;
    fn get_end(&self) -> Option<Position>;
    fn set_position(&mut self, start_pos: Option<Position>, end_pos: Option<Position>);
}

pub trait HasScope {
    fn set_scope(&mut self, _scope: Rc<RefCell<Scope>>) {}
}

pub trait IsFunction {
    fn execute(&mut self, args: Vec<ChValue>, name: Option<String>) -> Result<ChValue, Error>;
}

pub trait ConvertValue {
    fn into_number_type(self) -> NumberType
    where
        Self: Sized,
    {
        panic!("value can't be converted");
    }

    fn convert_to_number(self) -> Result<NumberType, Error>
    where
        Self: Sized + HasPosition + Display,
    {
        Err(Error::new(
            ErrType::Runtime,
            self.get_start(),
            self.get_end(),
            format!("could not convert {} to Number", self),
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

fn generate_undefined_op(caller: &dyn IsChValue, op_name: &str) -> Result<ChValue, Error> {
    Err(Error::new(
        ErrType::UndefinedOperator,
        caller.get_start(),
        caller.get_end(),
        format!(
            "operator '{}' not defined for type: {}",
            op_name,
            caller.get_desc()
        ),
        None,
    ))
}

pub trait ChOperators {
    fn add(self, _other: ChValue) -> Result<ChValue, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "add")
    }
    fn add_equal(self, other: ChValue) -> Result<ChValue, Error>
    where
        Self: IsChValue + Sized,
    {
        self.add(other)
    }
    fn sub_equal(self, other: ChValue) -> Result<ChValue, Error>
    where
        Self: IsChValue + Sized,
    {
        self.sub(other)
    }
    fn sub(self, _other: ChValue) -> Result<ChValue, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "sub")
    }
    fn mult(self, _other: ChValue) -> Result<ChValue, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "mult")
    }
    fn div(self, _other: ChValue) -> Result<ChValue, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "div")
    }
    fn pow(self, _other: ChValue) -> Result<ChValue, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "pow")
    }
    fn equal(self, _other: ChValue) -> Result<ChValue, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "equal")
    }
    fn not_equal(self, _other: ChValue) -> Result<ChValue, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "not equal")
    }
    fn less(self, _other: ChValue) -> Result<ChValue, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "less")
    }
    fn less_equal(self, _other: ChValue) -> Result<ChValue, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "less equal")
    }
    fn greater(self, _other: ChValue) -> Result<ChValue, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "greater")
    }
    fn greater_equal(self, _other: ChValue) -> Result<ChValue, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "greater equal")
    }
    fn and(self, _other: ChValue) -> Result<ChValue, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "and")
    }
    fn or(self, _other: ChValue) -> Result<ChValue, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "or")
    }
    fn not(self) -> Result<ChValue, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "not")
    }

    fn is_true(&self) -> bool {
        false
    }

    fn negate(self) -> Result<ChValue, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(&self, "negate")
    }

    fn access(&self, _other: ChValue) -> Result<ChValue, Error>
    where
        Self: IsChValue + Sized,
    {
        generate_undefined_op(self, "[]")
    }
}

pub trait IsChValue: Display + HasPosition + HasScope + ChOperators + ConvertValue {
    fn get_desc(&self) -> String;
    fn into_type(self) -> ChValue;
}

//------------- Chronos Types ------------------------//

#[derive(Debug, Clone)]
pub enum ChValue {
    Number(ChNumber),
    String(ChString),
    Array(ChArray),
    Function(ChFunction),
    Bool(ChBool),
    None(ChNone),
}

impl HasScope for ChValue {
    fn set_scope(&mut self, scope: Rc<RefCell<Scope>>) {
        match self {
            ChValue::Function(func) => func.set_scope(scope),
            _ => {}
        }
    }
}

#[macro_export]
macro_rules! unwrap_chvalue {
    ($chtype:ident, $inner:ident, $e:expr) => {
        match $chtype {
            ChValue::Number($inner) => $e,
            ChValue::String($inner) => $e,
            ChValue::Bool($inner) => $e,
            ChValue::Array($inner) => $e,
            ChValue::Function($inner) => $e,
            ChValue::None($inner) => $e,
        }
    };
}

//pub(crate) use unwrap_chvalue;

impl Display for ChValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unwrap_chvalue!(self, e, write!(f, "{}", e))
    }
}

impl HasPosition for ChValue {
    fn get_start(&self) -> Option<Position> {
        unwrap_chvalue!(self, e, e.get_start())
    }

    fn get_end(&self) -> Option<Position> {
        unwrap_chvalue!(self, e, e.get_end())
    }

    fn set_position(&mut self, start_pos: Option<Position>, end_pos: Option<Position>) {
        unwrap_chvalue!(self, e, e.set_position(start_pos, end_pos))
    }
}

impl ChValue {
    pub fn is_true(&self) -> bool {
        unwrap_chvalue!(self, e, e.is_true())
    }
}

impl ConvertValue for ChValue {
    fn into_number_type(self) -> NumberType {
        unwrap_chvalue!(self, e, e.into_number_type())
    }

    fn convert_to_number(self) -> Result<NumberType, Error> {
        unwrap_chvalue!(self, e, e.convert_to_number())
    }

    fn get_number_type(&self) -> NumberType {
        unwrap_chvalue!(self, e, e.get_number_type())
    }
}

impl ChOperators for ChValue {
    fn add(self, other: ChValue) -> Result<ChValue, Error> {
        unwrap_chvalue!(self, e, e.add(other))
    }

    fn sub(self, other: ChValue) -> Result<ChValue, Error> {
        unwrap_chvalue!(self, e, e.sub(other))
    }

    fn mult(self, other: ChValue) -> Result<ChValue, Error> {
        unwrap_chvalue!(self, e, e.mult(other))
    }

    fn div(self, other: ChValue) -> Result<ChValue, Error> {
        unwrap_chvalue!(self, e, e.div(other))
    }

    fn pow(self, other: ChValue) -> Result<ChValue, Error> {
        unwrap_chvalue!(self, e, e.pow(other))
    }

    fn greater(self, other: ChValue) -> Result<ChValue, Error> {
        unwrap_chvalue!(self, e, e.greater(other))
    }

    fn greater_equal(self, other: ChValue) -> Result<ChValue, Error> {
        unwrap_chvalue!(self, e, e.greater_equal(other))
    }

    fn less(self, other: ChValue) -> Result<ChValue, Error> {
        unwrap_chvalue!(self, e, e.less(other))
    }

    fn less_equal(self, other: ChValue) -> Result<ChValue, Error> {
        unwrap_chvalue!(self, e, e.less_equal(other))
    }

    fn add_equal(self, other: ChValue) -> Result<ChValue, Error> {
        unwrap_chvalue!(self, e, e.add_equal(other))
    }

    fn sub_equal(self, other: ChValue) -> Result<ChValue, Error> {
        unwrap_chvalue!(self, e, e.sub_equal(other))
    }

    fn equal(self, other: ChValue) -> Result<ChValue, Error> {
        unwrap_chvalue!(self, e, e.equal(other))
    }

    fn not_equal(self, other: ChValue) -> Result<ChValue, Error> {
        unwrap_chvalue!(self, e, e.not_equal(other))
    }

    fn and(self, other: ChValue) -> Result<ChValue, Error> {
        unwrap_chvalue!(self, e, e.and(other))
    }

    fn or(self, other: ChValue) -> Result<ChValue, Error> {
        unwrap_chvalue!(self, e, e.or(other))
    }

    fn not(self) -> Result<ChValue, Error> {
        unwrap_chvalue!(self, e, e.not())
    }

    fn is_true(&self) -> bool {
        unwrap_chvalue!(self, e, e.is_true())
    }

    fn negate(self) -> Result<ChValue, Error> {
        unwrap_chvalue!(self, e, e.negate())
    }

    fn access(&self, other: ChValue) -> Result<ChValue, Error> {
        unwrap_chvalue!(self, e, e.access(other))
    }
}

impl IsChValue for ChValue {
    fn get_desc(&self) -> String {
        unwrap_chvalue!(self, e, e.get_desc())
    }

    fn into_type(self) -> ChValue {
        self
    }
}

//--------------------------None------------------------------//

#[derive(Clone, Debug)]
pub struct ChNone {
    pub start_pos: Option<Position>,
    pub end_pos: Option<Position>,
}

impl HasScope for ChNone {}

impl HasPosition for ChNone {
    fn get_start(&self) -> Option<Position> {
        self.start_pos
    }

    fn get_end(&self) -> Option<Position> {
        self.end_pos
    }

    fn set_position(&mut self, start_pos: Option<Position>, end_pos: Option<Position>) {
        self.start_pos = start_pos;
        self.end_pos = end_pos;
    }
}

impl ChOperators for ChNone {
    fn equal(self, other: ChValue) -> Result<ChValue, Error> {
        Ok(ChBool {
            value: matches!(other, ChValue::None(_)),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn not_equal(self, other: ChValue) -> Result<ChValue, Error> {
        Ok(ChBool {
            value: !matches!(other, ChValue::None(_)),
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

    fn into_type(self) -> ChValue {
        ChValue::None(self)
    }
}

impl ConvertValue for ChNone {
    fn convert_to_number(self) -> Result<NumberType, Error> {
        Err(Error::new(
            ErrType::Runtime,
            self.get_start(),
            self.get_end(),
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
    pub start_pos: Option<Position>,
    pub end_pos: Option<Position>,
}

impl ChBool {
    pub fn from(v: bool) -> Self {
        ChBool {
            value: v,
            start_pos: None,
            end_pos: None,
        }
    }
}

impl ConvertValue for ChBool {
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

    fn into_type(self) -> ChValue {
        ChValue::Bool(self)
    }
}

impl HasScope for ChBool {}

impl Display for ChBool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", if self.value { "true" } else { "false" })
    }
}

impl HasPosition for ChBool {
    fn get_start(&self) -> Option<Position> {
        self.start_pos
    }

    fn get_end(&self) -> Option<Position> {
        self.end_pos
    }

    fn set_position(&mut self, start_pos: Option<Position>, end_pos: Option<Position>) {
        self.start_pos = start_pos;
        self.end_pos = end_pos;
    }
}

impl ChOperators for ChBool {
    fn equal(self, other: ChValue) -> Result<ChValue, Error> {
        Ok(ChBool {
            value: match other {
                ChValue::Bool(b) => self.value == b.value,
                _ => false,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn not_equal(self, other: ChValue) -> Result<ChValue, Error> {
        Ok(ChBool {
            value: match other {
                ChValue::Bool(b) => self.value != b.value,
                _ => true,
            },
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn not(mut self) -> Result<ChValue, Error> {
        self.value = !self.value;
        Ok(self.into_type())
    }

    fn is_true(&self) -> bool {
        self.value
    }

    fn and(self, other: ChValue) -> Result<ChValue, Error> {
        Ok(ChBool {
            value: self.value && other.is_true(),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }

    fn or(self, other: ChValue) -> Result<ChValue, Error> {
        Ok(ChBool {
            value: self.value || other.is_true(),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }
        .into_type())
    }
}

//--------------------------Number------------------------------//

#[derive(Clone, Debug)]
pub enum NumberType {
    Int(ChInt),
    Float(ChFloat),
}

#[derive(Debug, Clone)]
pub struct ChNumber {
    pub value: NumberType,
    pub start_pos: Option<Position>,
    pub end_pos: Option<Position>,
}

impl HasScope for ChNumber {}

impl IsChValue for ChNumber {
    fn get_desc(&self) -> String {
        String::from("Number")
    }

    fn into_type(self) -> ChValue {
        ChValue::Number(self)
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

impl ConvertValue for ChNumber {
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
    fn get_start(&self) -> Option<Position> {
        self.start_pos
    }

    fn get_end(&self) -> Option<Position> {
        self.end_pos
    }

    fn set_position(&mut self, start_pos: Option<Position>, end_pos: Option<Position>) {
        self.start_pos = start_pos;
        self.end_pos = end_pos;
    }
}

impl ChNumber {
    fn from(value: NumberType, _scope: &mut Rc<RefCell<Scope>>) -> Self {
        ChNumber {
            value,
            start_pos: None,
            end_pos: None,
        }
    }

    #[rustfmt::skip]
    fn operate_on(
        mut self,
        other: NumberType,
        int_op: fn(ChInt, ChInt) -> ChInt,
        float_op: fn(ChFloat, ChFloat) -> ChFloat,
    ) -> Self {
        self.value = match (self.value, other) {
            (NumberType::Int(v1), NumberType::Int(v2)) => NumberType::Int(int_op(v1, v2)),
            (NumberType::Float(v1), NumberType::Int(v2)) => NumberType::Float(float_op(v1, v2 as ChFloat)),
            (NumberType::Int(v1), NumberType::Float(v2)) => NumberType::Float(float_op(v1 as ChFloat, v2)),
            (NumberType::Float(v1), NumberType::Float(v2)) => NumberType::Float(float_op(v1, v2)),
        };

        self
    }
}

impl ChOperators for ChNumber {
    fn add(self, other: ChValue) -> Result<ChValue, Error> {
        Ok(self
            .operate_on(
                other.convert_to_number()?,
                |v1: ChInt, v2: ChInt| v1 + v2,
                |v1: ChFloat, v2: ChFloat| v1 + v2,
            )
            .into_type())
    }

    fn sub(self, other: ChValue) -> Result<ChValue, Error> {
        Ok(self
            .operate_on(
                other.convert_to_number()?,
                |v1: ChInt, v2: ChInt| v1 - v2,
                |v1: ChFloat, v2: ChFloat| v1 - v2,
            )
            .into_type())
    }

    fn mult(self, other: ChValue) -> Result<ChValue, Error> {
        Ok(self
            .operate_on(
                other.convert_to_number()?,
                |v1: ChInt, v2: ChInt| v1 * v2,
                |v1: ChFloat, v2: ChFloat| v1 * v2,
            )
            .into_type())
    }

    fn div(self, other: ChValue) -> Result<ChValue, Error> {
        if match other.clone().convert_to_number()? {
            NumberType::Int(v) => v == 0,
            NumberType::Float(v) => v == 0.0,
        } {
            Err(Error::new(
                ErrType::Runtime,
                self.start_pos,
                self.end_pos,
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

    fn pow(mut self, other: ChValue) -> Result<ChValue, Error> {
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

    #[allow(clippy::float_cmp)]
    fn equal(self, other: ChValue) -> Result<ChValue, Error> {
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

    #[allow(clippy::float_cmp)]
    fn not_equal(self, other: ChValue) -> Result<ChValue, Error> {
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

    fn less(self, other: ChValue) -> Result<ChValue, Error> {
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

    fn less_equal(self, other: ChValue) -> Result<ChValue, Error> {
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

    fn greater(self, other: ChValue) -> Result<ChValue, Error> {
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

    fn greater_equal(self, other: ChValue) -> Result<ChValue, Error> {
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

    fn and(self, other: ChValue) -> Result<ChValue, Error> {
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

    fn or(self, other: ChValue) -> Result<ChValue, Error> {
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

    fn not(mut self) -> Result<ChValue, Error> {
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

    fn negate(mut self) -> Result<ChValue, Error> {
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
    pub start_pos: Option<Position>,
    pub end_pos: Option<Position>,
}

impl Display for ChString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.string)
    }
}

impl HasScope for ChString {}

impl ConvertValue for ChString {}

impl IsChValue for ChString {
    fn into_type(self) -> ChValue {
        ChValue::String(self)
    }

    fn get_desc(&self) -> String {
        String::from("String")
    }
}

impl HasPosition for ChString {
    fn get_start(&self) -> Option<Position> {
        self.start_pos
    }

    fn get_end(&self) -> Option<Position> {
        self.end_pos
    }

    fn set_position(&mut self, start_pos: Option<Position>, end_pos: Option<Position>) {
        self.start_pos = start_pos;
        self.end_pos = end_pos;
    }
}

impl ChOperators for ChString {
    fn is_true(&self) -> bool {
        !self.string.is_empty()
    }

    fn add(mut self, other: ChValue) -> Result<ChValue, Error> {
        let other_string = match other {
            ChValue::Number(n) => format!("{}", n),
            ChValue::String(s) => s.to_string(),
            _ => {
                return Err(Error::new(
                    ErrType::Runtime,
                    self.start_pos,
                    self.end_pos,
                    format!("can't add {:?} to String", other),
                    None,
                ))
            }
        };

        self.string += &other_string;
        Ok(ChValue::String(self))
    }

    fn mult(mut self, other: ChValue) -> Result<ChValue, Error> {
        let n = other.into_number_type();

        match n {
            NumberType::Int(v) => {
                self.string = self.string.repeat(v.try_into().unwrap());
                Ok(self.into_type())
            }
            _ => Err(Error::new(
                ErrType::UndefinedOperator,
                self.start_pos,
                self.end_pos,
                String::from("multiply is only defined for type Int"),
                None,
            )),
        }
    }

    fn access(&self, other: ChValue) -> Result<ChValue, Error> {
        let num = other.convert_to_number()?;

        let num = match num {
            NumberType::Int(v) => v,
            _ => {
                return Err(Error::new(
                    ErrType::Runtime,
                    self.start_pos,
                    self.end_pos,
                    format!("expected Int found: {:?}", num),
                    None,
                ))
            }
        };

        if num >= self.string.len().try_into().unwrap() {
            return Err(Error::new(
                ErrType::Runtime,
                self.start_pos,
                self.end_pos,
                format!(
                    "Array index out of bounds => len: {}, index: {}",
                    self.string.len(),
                    num
                ),
                None,
            ));
        }

        let b: u8 = self.string.as_bytes()[num as usize];
        Ok(ChValue::String(ChString {
            string: (b as char).to_string(),
            start_pos: self.start_pos,
            end_pos: self.end_pos,
        }))
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

impl ConvertValue for ChFunction {}

impl HasPosition for ChFunction {
    fn get_start(&self) -> Option<Position> {
        match &self.func_type {
            FuncType::ChronFunc(func) => func.get_start(),
            FuncType::RustFunc(_) => Some(Position::from_file(0)),
        }
    }

    fn get_end(&self) -> Option<Position> {
        match &self.func_type {
            FuncType::ChronFunc(func) => func.get_end(),
            FuncType::RustFunc(_) => Some(Position::from_file(0)),
        }
    }

    fn set_position(&mut self, start_pos: Option<Position>, end_pos: Option<Position>) {
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

    fn into_type(self) -> ChValue {
        ChValue::Function(self)
    }
}

impl ChOperators for ChFunction {
    fn is_true(&self) -> bool {
        true
    }
}

impl IsFunction for ChFunction {
    fn execute(&mut self, args: Vec<ChValue>, name: Option<String>) -> Result<ChValue, Error> {
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
    ChronFunc(Box<ChronosFunc>),
}

#[derive(Clone)]
pub struct RustFunc {
    pub name: String,
    pub function: fn(args: Vec<ChValue>, name: Option<String>) -> Result<ChValue, Error>,
}

impl HasScope for RustFunc {
    fn set_scope(&mut self, _scope: Rc<RefCell<Scope>>) {}
}

impl IsFunction for RustFunc {
    fn execute(&mut self, args: Vec<ChValue>, name: Option<String>) -> Result<ChValue, Error> {
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
    pub start_pos: Option<Position>,
    pub end_pos: Option<Position>,
    pub scope: Rc<RefCell<Scope>>,
}

impl IsFunction for ChronosFunc {
    fn execute(&mut self, mut args: Vec<ChValue>, name: Option<String>) -> Result<ChValue, Error> {
        let mut n_scope = Scope::from_parent(
            format!("<function: {}>", name.unwrap_or_else(|| self.name.clone())),
            self.scope.clone(),
            self.start_pos,
        );

        if args.len() != self.args_name.len() {
            return Err(Error::new(
                ErrType::Runtime,
                self.start_pos,
                self.end_pos,
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
                        Some(n.start_pos),
                        Some(n.end_pos),
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
    fn get_start(&self) -> Option<Position> {
        self.start_pos
    }

    fn get_end(&self) -> Option<Position> {
        self.end_pos
    }

    fn set_position(&mut self, start_pos: Option<Position>, end_pos: Option<Position>) {
        self.start_pos = start_pos;
        self.end_pos = end_pos;
    }
}

//--------------------------Array------------------------------//

#[derive(Clone, Debug)]
pub struct ChArray {
    pub data: Vec<ChValue>,
    pub start_pos: Option<Position>,
    pub end_pos: Option<Position>,
}

impl Display for ChArray {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.data.is_empty() {
            return write!(f, "");
        }

        write!(f, "[{}", self.data.get(0).unwrap())?;
        for i in 1..self.data.len() {
            write!(f, ", {}", self.data.get(i).unwrap())?;
        }
        write!(f, "]")
    }
}

impl HasPosition for ChArray {
    fn get_start(&self) -> Option<Position> {
        self.start_pos
    }

    fn get_end(&self) -> Option<Position> {
        self.end_pos
    }

    fn set_position(&mut self, start_pos: Option<Position>, end_pos: Option<Position>) {
        self.start_pos = start_pos;
        self.end_pos = end_pos;
    }
}

impl HasScope for ChArray {}

impl ChOperators for ChArray {
    fn access(&self, other: ChValue) -> Result<ChValue, Error> {
        let num = other.convert_to_number()?;

        let num = match num {
            NumberType::Int(v) => v,
            _ => {
                return Err(Error::new(
                    ErrType::Runtime,
                    self.start_pos,
                    self.end_pos,
                    format!("expected Int found: {:?}", num),
                    None,
                ))
            }
        };

        if num >= self.data.len().try_into().unwrap() {
            return Err(Error::new(
                ErrType::Runtime,
                self.start_pos,
                self.end_pos,
                format!(
                    "Array index out of bounds => len: {}, index: {}",
                    self.data.len(),
                    num
                ),
                None,
            ));
        }

        Ok(self.data[num as usize].clone())
    }
}

impl ConvertValue for ChArray {}

impl IsChValue for ChArray {
    fn get_desc(&self) -> String {
        String::from("Array")
    }

    fn into_type(self) -> ChValue {
        ChValue::Array(self)
    }
}
