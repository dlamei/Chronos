use crate::{interpreter::ErrType, primitive::Primitive};
use std::{cell::RefCell, cmp, collections::HashMap, fmt::Display, ops, rc::Rc};

#[derive(Default, Debug, Clone)]
pub struct Scope {
    pub map: HashMap<String, Rc<RefCell<ChValue>>>,
    pub parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    pub fn from(parent: Rc<RefCell<Scope>>) -> Self {
        Scope {
            map: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn find_key(&self, name: &str) -> Option<Rc<RefCell<ChValue>>> {
        if self.map.contains_key(name) {
            return Some(self.map.get(name).unwrap().clone());
        }

        if let Some(parent) = &self.parent {
            return parent.borrow().find_key(name);
        }

        None
    }
}

// fn get_scope_with_key(name: &str, scope: &Rc<RefCell<Scope>>) -> Option<Rc<RefCell<Scope>>> {
//     let mut scp = scope.clone();

//     if scp.borrow().map.contains_key(name) {
//         return Some(scp);
//     }

//     while let Some(parent) = &scp.clone().borrow().parent {
//         if parent.borrow().map.contains_key(name) {
//             return Some(parent.clone());
//         }
//         scp = parent.clone();
//     }
//     None
// }

#[derive(Debug, Clone)]
pub struct ChValue {
    pub value: Primitive,
    pub cast: Option<Primitive>,
}

impl Display for ChValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = write!(f, "{}", self.value);
        if let Some(t) = &self.cast {
            res = write!(f, ": {}", t);
        }
        res
    }
}

impl ChValue {
    pub fn from(value: Primitive) -> Self {
        ChValue { value, cast: None }
    }

    pub fn new(value: Primitive, typ: Option<Primitive>) -> Self {
        ChValue { value, cast: typ }
    }

    // pub fn try_cast(self, cast: &Option<ChValue>) -> Self {
    //     // if cast.is_none()
    // }
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
