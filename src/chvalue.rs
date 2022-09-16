use crate::parser::Node;
use paste::paste;
use std::{
    cell::RefCell,
    cmp,
    collections::{HashMap, LinkedList},
    fmt, ops,
    rc::Rc,
};

use crate::interpreter::ErrType::{self, *};

// pub type Scope = HashMap<String, ChValue>;

#[derive(Debug, Clone)]
pub struct Scope {
    pub map: HashMap<String, ChValue>,
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
pub struct ExpressionData {
    pub nodes: LinkedList<Node>,
    pub ret_last: bool,
    pub scope: Rc<RefCell<Scope>>,
    // pub parent: Rc<RefCell<Scope>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ChType {
    Bool,

    I8,
    I16,
    I32,
    I64,
    ISize,
    I128,

    U8,
    U16,
    U32,
    U64,
    USize,
    U128,

    F32,
    F64,

    Char,
    String,

    Expression,

    Void,
}

#[derive(Debug, Clone)]
pub enum ChValue {
    Bool(bool),

    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    ISize(isize),
    I128(i128),

    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    USize(usize),
    U128(u128),

    F32(f32),
    F64(f64),

    Char(char),
    String(String),

    Expression(ExpressionData),

    Void,
}

macro_rules! unwrap_chvalue {
    ($chvalue:ident, $in:ident, $e:expr) => {{
        use ChValue::*;
        match $chvalue {
            Bool($in) => $e,

            I8($in) => $e,
            I16($in) => $e,
            I32($in) => $e,
            I64($in) => $e,
            ISize($in) => $e,
            I128($in) => $e,

            U8($in) => $e,
            U16($in) => $e,
            U32($in) => $e,
            U64($in) => $e,
            USize($in) => $e,
            U128($in) => $e,

            F32($in) => $e,
            F64($in) => $e,

            Char($in) => $e,
            String($in) => $e,
            _ => panic!("unwrap_chvalue not implemented for {:?}", $chvalue),
        }
    }};
}

macro_rules! chnum_as_typ {
    ($typ: ty) => {
        paste! {
            pub fn [<as_$typ>](&self) -> $typ {
                use ChValue::*;
                match self {
                    Bool(v) => *v as u8 as $typ,

                    I8(v) => *v as $typ,
                    I16(v) => *v as $typ,
                    I32(v) => *v as $typ,
                    I64(v) => *v as $typ,
                    ISize(v) => *v as $typ,
                    I128(v) => *v as $typ,

                    U8(v) => *v as $typ,
                    U16(v) => *v as $typ,
                    U32(v) => *v as $typ,
                    U64(v) => *v as $typ,
                    USize(v) => *v as $typ,
                    U128(v) => *v as $typ,

                    F32(v) => *v as $typ,
                    F64(v) => *v as $typ,

                    Char(v) => *v as u8 as $typ,
                    _ => panic!("to_{} not defined for {:?}", stringify!($typ), self),
                }
            }
        }
    };
}

macro_rules! apply_op {
    ($lhs:expr; $op:tt $rhs:expr; $checked:ident) => {{
        use ChValue::*;
        match ($lhs, $rhs) {
            (Bool(v1), Bool(v2)) => U8(*v1 as u8 $op *v2 as u8),
            (Char(v1), Char(v2)) => {
                if let Some(v) = (*v1 as u8).$checked(*v2 as u8) {
                    Char(v as char)
                } else {
                    U16(*v1 as u16 $op *v2 as u16)
                }
            }
            (U8(v1), U8(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    U8(v)
                } else {
                    U16(*v1 as u16 $op *v2 as u16)
                }
            }
            (U16(v1), U16(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    U16(v)
                } else {
                    U32(*v1 as u32 $op *v2 as u32)
                }
            }
            (U32(v1), U32(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    U32(v)
                } else {
                    U64(*v1 as u64 $op *v2 as u64)
                }
            }
            (U64(v1), U64(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    U64(v)
                } else {
                    U128(*v1 as u128 $op *v2 as u128)
                }
            }
            (U128(v1), U128(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    U128(v)
                } else {
                    todo!()
                }
            }
            (I8(v1), I8(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    I8(v)
                } else {
                    I16(*v1 as i16 $op *v2 as i16)
                }
            }
            (I16(v1), I16(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    I16(v)
                } else {
                    I32(*v1 as i32 $op *v2 as i32)
                }
            }
            (I32(v1), I32(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    I32(v)
                } else {
                    I64(*v1 as i64 $op *v2 as i64)
                }
            }
            (I64(v1), I64(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    I64(v)
                } else {
                    I128(*v1 as i128 $op *v2 as i128)
                }
            }
            (I128(v1), I128(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    I128(v)
                } else {
                    todo!()
                }
            }

            (F32(v1), F32(v2)) => F32(*v1 $op *v2),
            (F64(v1), F64(v2)) => F64(*v1 $op *v2),
            _ => panic!(
                "can't operate on type {:?} with type {:?}",
                $lhs.get_type(),
                $rhs.get_type()
            ),
        }
    }};

    ($lhs:expr; $op:tt $rhs:expr; => $rest:expr) => {{
        use ChValue::*;
        match ($lhs, $rhs) {
            (Bool(v1), Bool(v2)) => *v1 as u8 $op *v2 as u8,
            (U8(v1), U8(v2)) => *v1 $op *v2,
            (U16(v1), U16(v2)) => *v1 $op *v2,
            (U32(v1), U32(v2)) => *v1 $op *v2,
            (U64(v1), U64(v2)) => *v1 $op *v2,
            (U128(v1), U128(v2)) => *v1 $op *v2,
            (I8(v1), I8(v2)) => *v1 $op *v2,
            (I16(v1), I16(v2)) => *v1 $op *v2,
            (I32(v1), I32(v2)) => *v1 $op *v2,
            (I64(v1), I64(v2)) => *v1 $op *v2,
            (I128(v1), I128(v2)) => *v1 $op *v2,

            (F32(v1), F32(v2)) => *v1 $op *v2,
            (F64(v1), F64(v2)) => *v1 $op *v2,
            (Char(v1), Char(v2)) => *v1 $op *v2,
            _ => $rest,
        }
    }};
}

fn type_from_bit_size(size: u32, signed: bool, float: bool) -> Option<ChType> {
    use ChType::*;
    Some(if !float {
        if signed {
            match size {
                8 => I8,
                16 => I16,
                32 => I32,
                64 => I64,
                128 => I128,
                _ => return None,
            }
        } else {
            match size {
                8 => U8,
                16 => U16,
                32 => U32,
                64 => U64,
                128 => U128,
                _ => return None,
            }
        }
    } else {
        match size {
            32 => F32,
            64 => F64,
            _ => return None,
        }
    })
}

fn get_op_numtype(left: &ChValue, right: &ChValue) -> Option<ChType> {
    if !left.is_num() || !right.is_num() {
        return None;
    }

    let mut v1 = &left; //biggest bitsize
    let mut v2 = &right;

    if right.bitsize() > left.bitsize() {
        v1 = &right;
        v2 = &left;
    }

    if v1.is_float() && v2.is_float() {
        return type_from_bit_size(cmp::max(v1.bitsize(), v2.bitsize()), true, true);
    } else if v1.is_float() {
        use ChType::*;
        if v2.bitsize() <= 32 {
            return Some(F32);
        } else {
            return Some(F64);
        }
    } else if v2.is_float() {
        use ChType::*;
        if v1.bitsize() <= 32 {
            return Some(F32);
        } else {
            return Some(F64);
        }
    }

    if v1.bitsize() > v2.bitsize() || v1.is_signed() == v2.is_signed() {
        type_from_bit_size(v1.bitsize(), v1.is_signed(), false)
    } else {
        //TODO: what to do with overflow?
        type_from_bit_size(v1.bitsize(), true, false)
    }
}

impl ChValue {
    #[cfg(target_pointer_width = "64")]
    crate::assign_func!(bitsize -> u32, 0,
        [8; Bool(_), Char(_), U8(_), I8(_)]
        [16; U16(_), I16(_)]
        [32; U32(_), I32(_), F32(_)]
        [64; U64(_), USize(_), I64(_), ISize(_), F64(_)]
        [128; U128(_), I128(_)]
    );

    #[cfg(target_pointer_width = "32")]
    crate::assign_func!(get_bit_size -> u32, 0,
        [8; Bool(_), U8(_), I8(_)]
        [32; U32(_), I32(_), F32(_)]
        [64; U64(_), USize(_), I64(_), ISize(_), F64(_)]
        [128; U128(_), I128(_)]
    );

    crate::assign_func!(is_signed -> bool, true,
        [false; U8(_), U32(_), U64(_), USize(_), U128(_)]
    );

    crate::assign_func!(is_float -> bool, false,
        [true; F32(_), F64(_)]
    );

    crate::assign_func!(is_num -> bool, false,
        [true; Bool(_), Char(_), U8(_), I8(_)]
        [true; U16(_), I16(_)]
        [true; U32(_), I32(_), F32(_)]
        [true; U64(_), USize(_), I64(_), ISize(_), F64(_)]
        [true; U128(_), I128(_)]
    );

    fn from_chvalue(typ: ChType) -> Self {
        use ChValue::*;

        match typ {
            ChType::Bool => Bool(false),
            ChType::I8 => I8(0),
            ChType::I16 => I16(0),
            ChType::I32 => I32(0),
            ChType::I64 => I64(0),
            ChType::ISize => ISize(0),
            ChType::I128 => I128(0),
            ChType::U8 => U8(0),
            ChType::U16 => U16(0),
            ChType::U32 => U32(0),
            ChType::U64 => U64(0),
            ChType::USize => USize(0),
            ChType::U128 => U128(0),
            ChType::F32 => F32(0.0),
            ChType::F64 => F64(0.0),
            ChType::Char => Char('\0'),
            ChType::String => String("".to_owned()),
            ChType::Void => Void,
            ChType::Expression => panic!("can't initialize chvalue"),
        }
    }

    pub fn get_type(&self) -> ChType {
        use ChValue::*;

        match self {
            Bool(_) => ChType::Bool,
            I8(_) => ChType::I8,
            I16(_) => ChType::I16,
            I32(_) => ChType::I32,
            I64(_) => ChType::I64,
            ISize(_) => ChType::ISize,
            I128(_) => ChType::I128,
            U8(_) => ChType::U8,
            U16(_) => ChType::U16,
            U32(_) => ChType::U32,
            U64(_) => ChType::U64,
            USize(_) => ChType::USize,
            U128(_) => ChType::U128,
            F32(_) => ChType::F32,
            F64(_) => ChType::F64,
            Char(_) => ChType::Char,
            String(_) => ChType::String,
            Expression(_) => ChType::Expression,
            Void => ChType::Void,
        }
    }

    pub fn is_zero(&self) -> bool {
        use ChValue::*;

        match self {
            Bool(false) | I8(0i8) | I32(0i32) | I64(0i64) | ISize(0isize) | I128(0i128)
            | U8(0u8) | U32(0u32) | U64(0u64) | USize(0usize) | U128(0u128) | Char('\0') | Void => {
                true
            }
            F32(v) => v == &0.0,
            F64(v) => v == &0.0,
            _ => false,
        }
    }

    pub fn as_bool(&self) -> bool {
        !self.is_zero()
    }

    pub fn as_transformed_num(&self, typ: &ChType) -> ChValue {
        use ChValue::*;
        match typ {
            ChType::Bool => Bool(self.as_bool()),
            ChType::I8 => I8(self.as_i8()),
            ChType::I16 => I16(self.as_i16()),
            ChType::I32 => I32(self.as_i32()),
            ChType::I64 => I64(self.as_i64()),
            ChType::ISize => ISize(self.as_isize()),
            ChType::I128 => I128(self.as_i128()),
            ChType::U8 => U8(self.as_u8()),
            ChType::U16 => U16(self.as_u16()),
            ChType::U32 => U32(self.as_u32()),
            ChType::U64 => U64(self.as_u64()),
            ChType::USize => USize(self.as_usize()),
            ChType::U128 => U128(self.as_u128()),
            ChType::F32 => F32(self.as_f32()),
            ChType::F64 => F64(self.as_f64()),
            ChType::Char => Char(self.as_u8() as char),

            _ => panic!("can't transorm {:?} to type: {:?}", self, typ),
        }
    }

    chnum_as_typ!(i8);
    chnum_as_typ!(i16);
    chnum_as_typ!(i32);
    chnum_as_typ!(i64);
    chnum_as_typ!(isize);
    chnum_as_typ!(i128);

    chnum_as_typ!(u8);
    chnum_as_typ!(u16);
    chnum_as_typ!(u32);
    chnum_as_typ!(u64);
    chnum_as_typ!(usize);
    chnum_as_typ!(u128);

    chnum_as_typ!(f32);
    chnum_as_typ!(f64);
}

impl fmt::Display for ChValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let ChValue::Void = self {
            write!(f, "")
        } else if let ChValue::Expression(_) = self {
            write!(f, "Expression")
        } else {
            unwrap_chvalue!(self, e, write!(f, "{}", e))
        }
    }
}

impl ops::Add<&ChValue> for &ChValue {
    type Output = Result<ChValue, ErrType>;

    fn add(self, rhs: &ChValue) -> Result<ChValue, ErrType> {
        use ChValue::*;

        if let String(ref s1) = self {
            if let String(s2) = rhs {
                let mut res = s1.to_owned();
                res.push_str(s2);
                return Ok(String(res));
            }
        }

        //TODO: id system
        if let Some(typ) = get_op_numtype(self, rhs) {
            let lhs = self.as_transformed_num(&typ);
            let rhs = rhs.as_transformed_num(&typ);

            Ok(apply_op!(&lhs; + &rhs; checked_add))
        } else {
            Err(ErrType::UnsupportedOperand(format!(
                "Operator [Add] not defined for {:?} + {:?}",
                self.get_type(),
                rhs.get_type()
            )))
        }
    }
}

impl ops::Add<ChValue> for ChValue {
    type Output = Result<ChValue, ErrType>;
    fn add(self, rhs: ChValue) -> Result<ChValue, ErrType> {
        &self + &rhs
    }
}

impl ops::Sub<&ChValue> for &ChValue {
    type Output = Result<ChValue, ErrType>;
    fn sub(self, rhs: &ChValue) -> Result<ChValue, ErrType> {
        if let Some(typ) = get_op_numtype(self, rhs) {
            let lhs = self.as_transformed_num(&typ);
            let rhs = rhs.as_transformed_num(&typ);

            Ok(apply_op!(&lhs; - &rhs; checked_sub))
        } else {
            Err(ErrType::UnsupportedOperand(format!(
                "Operator [Sub] not defined for {:?} - {:?}",
                self.get_type(),
                rhs.get_type()
            )))
        }
    }
}

impl ops::Sub<ChValue> for ChValue {
    type Output = Result<ChValue, ErrType>;
    fn sub(self, rhs: ChValue) -> Result<ChValue, ErrType> {
        &self - &rhs
    }
}

impl ops::Mul<&ChValue> for &ChValue {
    type Output = Result<ChValue, ErrType>;
    fn mul(self, rhs: &ChValue) -> Result<ChValue, ErrType> {
        use ChValue::*;

        if rhs.is_num() {
            if let String(v) = self {
                return Ok(String(v.repeat(rhs.as_usize())));
            }
            //TODO: Char mult?
            // else if let Char(v) = self {
            //     return Ok(String(v.to_string().repeat(rhs.to_usize())));
            // }
        }

        if let Some(typ) = get_op_numtype(self, rhs) {
            let lhs = self.as_transformed_num(&typ);
            let rhs = rhs.as_transformed_num(&typ);

            let res = apply_op!(&lhs; * &rhs; checked_mul);
            Ok(res)
        } else {
            Err(ErrType::UnsupportedOperand(format!(
                "Operator [Mul] not defined for {:?} * {:?}",
                self.get_type(),
                rhs.get_type()
            )))
        }
    }
}

impl ops::Mul<ChValue> for ChValue {
    type Output = Result<ChValue, ErrType>;
    fn mul(self, rhs: ChValue) -> Result<ChValue, ErrType> {
        &self * &rhs
    }
}

impl ops::Div<&ChValue> for &ChValue {
    type Output = Result<ChValue, ErrType>;
    fn div(self, rhs: &ChValue) -> Result<ChValue, ErrType> {
        if let ChValue::Void = rhs {
            return Err(ErrType::UnsupportedOperand(format!(
                "Operator [Div] not defined for {:?} / {:?}",
                self.get_type(),
                rhs.get_type()
            )));
        }

        if rhs.is_zero() {
            return Err(ZeroDivision);
        }

        if let Some(typ) = get_op_numtype(self, rhs) {
            let lhs = self.as_transformed_num(&typ);
            let rhs = rhs.as_transformed_num(&typ);

            Ok(apply_op!(&lhs; / &rhs; checked_div))
        } else {
            Err(ErrType::UnsupportedOperand(format!(
                "Operator [Div] not defined for {:?} / {:?}",
                self.get_type(),
                rhs.get_type()
            )))
        }
    }
}

impl ops::Div<ChValue> for ChValue {
    type Output = Result<ChValue, ErrType>;
    fn div(self, rhs: ChValue) -> Result<ChValue, ErrType> {
        &self / &rhs
    }
}

impl PartialEq for ChValue {
    fn eq(&self, other: &Self) -> bool {
        use ChValue::*;

        match self {
            //TODO: maybe 'a' == "a"?
            String(s1) => {
                if let String(s2) = other {
                    return s1 == s2;
                } else {
                    return false;
                }
            }
            Char(c1) => {
                if let Char(c2) = other {
                    return c1 == c2;
                } else {
                    return false;
                }
            }
            Void => {
                if let Void = other {
                    return true;
                } else {
                    return false;
                }
            }
            _ => (),
        }

        if let Some(typ) = get_op_numtype(self, other) {
            let lhs = self.as_transformed_num(&typ);
            let rhs = other.as_transformed_num(&typ);

            apply_op!(&lhs; == &rhs; => false)
        } else {
            false
        }
    }
}

impl PartialOrd for ChValue {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        if self == other {
            return Some(cmp::Ordering::Equal);
        }

        if let (ChValue::String(s1), ChValue::String(s2)) = (self, other) {
            return s1.partial_cmp(s2);
        }

        if let Some(typ) = get_op_numtype(self, other) {
            let lhs = self.as_transformed_num(&typ);
            let rhs = other.as_transformed_num(&typ);

            let b = apply_op!(&lhs; > &rhs; => false);
            if b {
                Some(cmp::Ordering::Greater)
            } else {
                Some(cmp::Ordering::Less)
            }
        } else {
            None
        }
    }
}

#[test]
fn add_chvalue() {
    use ChValue::*;
    assert_eq!((I32(2) + Bool(false)).unwrap(), I32(2));
    assert_eq!(
        (I32(2) + Char(std::char::from_u32(10).unwrap())),
        Ok(I32(12))
    );
    assert_eq!((I32(2) + I32(3)).unwrap(), I32(5));
    assert_eq!((U8(2) + I32(3)).unwrap(), I32(5));
    assert_eq!((U8(2) + U128(3)).unwrap(), U128(5));
    assert_eq!((I32(2) + U64(3)).unwrap(), U64(5));
    assert_eq!((U8(2) + U8(3)).unwrap(), U8(5));
    assert_eq!((I8(2) + U8(3)).unwrap(), I8(5));
    assert_eq!((I128(2) + F32(3.0)).unwrap(), F32(5.0));
    assert_eq!(
        (String("Hello".to_owned()) + String(" World!".to_owned())).unwrap(),
        String("Hello World!".to_owned())
    );
}

#[test]
fn sub_chvalue() {
    use ChValue::*;
    assert_eq!((I32(5) - Bool(true)).unwrap(), I32(4));
    assert_eq!((I32(5) - I32(3)).unwrap(), I32(2));
    assert_eq!((U8(5) - I32(3)).unwrap(), I32(2));
    assert_eq!((U8(5) - U128(3)).unwrap(), U128(2));
    assert_eq!((I32(5) - U64(3)).unwrap(), U64(2));
    assert_eq!((U8(5) - U8(3)).unwrap(), U8(2));
    assert_eq!((I128(5) - F32(3.0)).unwrap(), F32(2.0));
}

#[test]
fn mul_chvalue() {
    use ChValue::*;
    assert_eq!((I32(2) * I32(3)).unwrap(), I32(6));
    assert_eq!((I32(2) * U8(3)).unwrap(), I32(6));
    assert_eq!((U8(2) * U8(3)).unwrap(), U8(6));
    assert_eq!((I128(2) * F64(3.0)).unwrap(), F64(6.0));
    assert_eq!(
        (String("a".to_owned()) * I32(3)).unwrap(),
        String("aaa".to_owned())
    );
}

#[test]
fn div_chvalue() {
    use ChValue::*;
    assert_eq!((I32(5) / I32(3)).unwrap(), I32(1));
    assert_eq!((I32(3) / F32(2.0)).unwrap(), F32(1.5));
    assert_eq!(I32(3) / Bool(false), Err(ZeroDivision));
    assert_eq!(F32(3.0) / Char('\0'), Err(ZeroDivision));
}

#[test]
fn eq_chvalue() {
    use ChValue::*;
    assert_eq!(I32(5), I32(5));
    assert_eq!((I16(2) + I16(3)).unwrap(), I8(5));
    assert_eq!(I8(5), I32(5));
    assert_eq!(I8(5), I128(5));
    assert_eq!(I8(0), F64(0.0));
    assert_ne!(I32(5), I32(2));
    assert_ne!(I8(5), I32(3));
    assert_ne!(I8(5), I128(8));
    assert_ne!(I8(0), F64(0.1));

    assert_eq!(String("Hello".to_owned()), String("Hello".to_owned()));
    assert_ne!(String("Hello".to_owned()), String("hello".to_owned()));
    assert_eq!(Char('c'), Char('c'));
    assert_ne!(Char('d'), Char('c'));
    assert_ne!(String("c".to_owned()), Char('c'));

    assert_eq!(Void, Void);
}

#[test]
fn chvalue_to_bool() {
    use ChValue::*;
    assert!(F32(2.0).as_bool());
    assert!(!U8(0).as_bool());
    assert!(!Void.as_bool());
    assert!(String("".to_owned()).as_bool());
}

#[test]
fn chvalue_overflow() {
    use ChValue::*;
    assert_eq!(I32(i32::MAX) + I32(1), Ok(I64(i32::MAX as i64 + 1)));
    assert_eq!(I32(i32::MAX) + F32(1.0), Ok(F32(i32::MAX as f32 + 1.0)));
    assert_eq!(Bool(false) + Bool(false), Ok(I8(0)));
}

#[test]
fn chvalue_order() {
    use ChValue::*;
    assert!(Char('a') < Char('b'));
    assert!(Bool(true) < Char('b'));
    assert!(F32(1.3) > I128(1));
    assert!(F32(1.0) >= I128(1));
    assert!(String("Hello".to_owned()) >= String("Hello".to_owned()));
    assert!(String("Hello World".to_owned()) > String("Hello".to_owned()));
    assert!(!(Void > Char('d')));
}
