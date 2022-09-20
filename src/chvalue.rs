use crate::{interpreter::ErrType, primitive::Primitive};
use std::{
    fmt::Display,
    cell::RefCell, cmp, collections::HashMap, ops, rc::Rc};

#[derive(Debug, Clone)]
pub struct Scope {
    pub map: HashMap<String, Rc<RefCell<ChValue>>>,
    pub parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            map: HashMap::new(),
            parent: None,
        }
    }

    pub fn from(parent: Rc<RefCell<Scope>>) -> Self {
        Scope {
            map: HashMap::new(),
            parent: Some(parent),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ChValue {
    pub value: Primitive,
    pub typ: Option<Primitive>,
}

impl Display for ChValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = write!(f, "{}", self.value);
        if let Some(t) = &self.typ {
            res = write!(f, ": {}", t);
        }

        res
    }
}

impl ChValue {
    pub fn from(value: Primitive) -> Self {
        ChValue { value, typ: None }
    }

    pub fn new(value: Primitive, typ: Option<Primitive>) -> Self {
        ChValue { value, typ }
    }
}

impl ops::Add for &ChValue {
    type Output = Result<ChValue, ErrType>;

    fn add(self, rhs: Self) -> Self::Output {
        Ok(ChValue::from((&self.value + &rhs.value)?))
    }
}

impl ops::Add for ChValue {
    type Output = Result<ChValue, ErrType>;

    fn add(self, rhs: Self) -> Self::Output {
        &self + &rhs
    }
}

impl ops::Sub for &ChValue {
    type Output = Result<ChValue, ErrType>;

    fn sub(self, rhs: Self) -> Self::Output {
        Ok(ChValue::from((&self.value - &rhs.value)?))
    }
}

impl ops::Sub for ChValue {
    type Output = Result<ChValue, ErrType>;

    fn sub(self, rhs: Self) -> Self::Output {
        &self - &rhs
    }
}

impl ops::Mul for &ChValue {
    type Output = Result<ChValue, ErrType>;

    fn mul(self, rhs: Self) -> Self::Output {
        Ok(ChValue::from((&self.value * &rhs.value)?))
    }
}

impl ops::Mul for ChValue {
    type Output = Result<ChValue, ErrType>;

    fn mul(self, rhs: Self) -> Self::Output {
        &self * &rhs
    }
}

impl ops::Div for &ChValue {
    type Output = Result<ChValue, ErrType>;

    fn div(self, rhs: Self) -> Self::Output {
        Ok(ChValue::from((&self.value / &rhs.value)?))
    }
}

impl ops::Div for ChValue {
    type Output = Result<ChValue, ErrType>;

    fn div(self, rhs: Self) -> Self::Output {
        &self / &rhs
    }
}

impl PartialEq for ChValue {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl PartialOrd for ChValue {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        self.value.partial_cmp(&other.value)
    }
}
