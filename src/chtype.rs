use paste::paste;
use std::{cmp, fmt, ops};

use crate::interpreter::{RuntimeErr::{self, *}, RuntimeRes};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum ChType {
    Bool(bool),

    I8(i8),
    I32(i32),
    I64(i64),
    ISize(isize),
    I128(i128),

    U8(u8),
    U32(u32),
    U64(u64),
    USize(usize),
    U128(u128),

    F32(f32),
    F64(f64),

    Char(char),
    String(String),
}

macro_rules! unwrap_chtype {
    ($chtype:ident, $in:ident, $e:expr) => {
        match $chtype {
            ChType::Bool($in) => $e,

            ChType::I8($in) => $e,
            ChType::I32($in) => $e,
            ChType::I64($in) => $e,
            ChType::ISize($in) => $e,
            ChType::I128($in) => $e,

            ChType::U8($in) => $e,
            ChType::U32($in) => $e,
            ChType::U64($in) => $e,
            ChType::USize($in) => $e,
            ChType::U128($in) => $e,

            ChType::F32($in) => $e,
            ChType::F64($in) => $e,

            ChType::Char($in) => $e,
            ChType::String($in) => $e,
        }
    };
}

macro_rules! chtype_to_typ {
    ($typ: ty) => {
        paste! {
            fn [<to_$typ>](self) -> $typ {
                use ChType::*;
                match self {
                    Bool(v) => v as u8 as $typ,

                    I8(v) => v as $typ,
                    I32(v) => v as $typ,
                    I64(v) => v as $typ,
                    ISize(v) => v as $typ,
                    I128(v) => v as $typ,

                    U8(v) => v as $typ,
                    U32(v) => v as $typ,
                    U64(v) => v as $typ,
                    USize(v) => v as $typ,
                    U128(v) => v as $typ,

                    F32(v) => v as $typ,
                    F64(v) => v as $typ,

                    Char(v) => v as u8 as $typ,
                    _ => panic!("to_{} not defined for {:?}", stringify!($typ), self),
                }
            }
        }
    };
}

macro_rules! apply_op {
    ($lhs:ident $op:tt $rhs:ident) => {{
        let max = cmp::max($lhs.priority(), $rhs.priority());

        match max {
            1 => ChType::U8($lhs.to_u8() $op $rhs.to_u8()),
            2 => ChType::I8($lhs.to_i8() $op $rhs.to_i8()),
            3 => ChType::U32($lhs.to_u32() $op $rhs.to_u32()),
            4 => ChType::I32($lhs.to_i32() $op $rhs.to_i32()),
            5 => ChType::U64($lhs.to_u64() $op $rhs.to_u64()),
            6 => ChType::I64($lhs.to_i64() $op $rhs.to_i64()),
            7 => ChType::U128($lhs.to_u128() $op $rhs.to_u128()),
            8 => ChType::I128($lhs.to_i128() $op $rhs.to_i128()),
            9 => ChType::F32($lhs.to_f32() $op $rhs.to_f32()),
            10 => ChType::F64($lhs.to_f64() $op $rhs.to_f64()),
            _ => panic!(
                "Operator {} not defined for type with priority: {}",
                stringify!($op),
                max
            ),
        }
    }};
}

impl ChType {
    #[cfg(target_pointer_width = "64")]
    crate::assign_func!(priority -> u32, 0,
        [1; U8(_)]
        [2; I8(_)]
        [3; U32(_)]
        [4; I32(_)]
        [5; U64(_), USize(_)]
        [6; I64(_), ISize(_)]
        [7; U128(_)]
        [8; I128(_)]
        [9; F32(_)]
        [10; F64(_)]
    );

    #[cfg(target_pointer_width = "32")]
    crate::assign_func!(priority -> u32, 0,
        [1; U8(_)]
        [2; I8(_)]
        [3; U32(_), Usize(_)]
        [4; I32(_), Isize(_)]
        [5; U64(_)]
        [6; I64(_)]
        [7; U128(_)]
        [8; I128(_)]
        [9; F32(_)]
        [10; F64(_)]
    );

    chtype_to_typ!(i8);
    chtype_to_typ!(i32);
    chtype_to_typ!(i64);
    chtype_to_typ!(isize);
    chtype_to_typ!(i128);

    chtype_to_typ!(u8);
    chtype_to_typ!(u32);
    chtype_to_typ!(u64);
    chtype_to_typ!(usize);
    chtype_to_typ!(u128);

    chtype_to_typ!(f32);
    chtype_to_typ!(f64);
}

impl fmt::Display for ChType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unwrap_chtype!(self, e, write!(f, "{}", e.to_string()))
    }
}

impl ops::Add<ChType> for ChType {
    type Output = RuntimeRes;

    fn add(self, rhs: ChType) -> RuntimeRes {
        if let ChType::String(val) = self {
            return Ok(ChType::String(format!("{}{}", val, rhs)));
        } else if let ChType::Char(_) = self {
            return Err(RuntimeErr::UnsupportedOperand("Operator [Add] not defined for Char".to_string()));
        }

        Ok(apply_op!(self + rhs))
    }
}

impl ops::Sub<ChType> for ChType {
    type Output = RuntimeRes;
    fn sub(self, rhs: ChType) -> RuntimeRes {
        match self {
            ChType::String(_) => return Err(UnsupportedOperand("Operator [Sub] not defined for String".to_string())),
            ChType::Char(_) => return Err(UnsupportedOperand("Operator [Sub] not defined for Char".to_string())),
            _ => (),
        }

        Ok(apply_op!(self - rhs))
    }
}

impl ops::Mul<ChType> for ChType {
    type Output = RuntimeRes;
    fn mul(self, rhs: ChType) -> RuntimeRes {
        use ChType::*;

        if let String(v) = self {
            return Ok(String(v.repeat(rhs.to_usize())));
        } else if let Char(v) = self {
            return Ok(String(v.to_string().repeat(rhs.to_usize())));
        }

        Ok(apply_op!(self * rhs))
    }
}

impl ops::Div<ChType> for ChType {
    type Output = RuntimeRes;
    fn div(self, rhs: ChType) -> RuntimeRes {
        use ChType::*;

        match rhs {
            Bool(false) | I8(0i8) | I32(0i32) | I64(0i64) | ISize(0isize) | I128(0i128)
            | U8(0u8) | U32(0u32) | U64(0u64) | USize(0usize) | U128(0u128) | String(_)
            | Char(_) => return Err(ZeroDivisionError),

            F32(v) => {
                if v == 0.0 {
                    return Err(ZeroDivisionError);
                }
            }

            F64(v) => {
                if v == 0.0 {
                    return Err(ZeroDivisionError);
                }
            }

            _ => (),
        };

        Ok(apply_op!(self / rhs))
    }
}

#[test]
fn add_chtype() {
    use ChType::*;
    assert_eq!((I32(2) + Bool(false)).unwrap(), I32(2));
    assert_eq!((I32(2) + I32(3)).unwrap(), I32(5));
    assert_eq!((U8(2) + I32(3)).unwrap(), I32(5));
    assert_eq!((U8(2) + U128(3)).unwrap(), U128(5));
    assert_eq!((I32(2) + U64(3)).unwrap(), U64(5));
    assert_eq!((U8(2) + U8(3)).unwrap(), U8(5));
    assert_eq!((I128(2) + F32(3.0)).unwrap(), F32(5.0));
    assert_eq!(
        (String("Hello".to_string()) + String(" World!".to_string())).unwrap(),
        String("Hello World!".to_string())
    );
}

#[test]
fn sub_chtype() {
    use ChType::*;
    assert_eq!((I32(5) - Bool(true)).unwrap(), I32(4));
    assert_eq!((I32(5) - I32(3)).unwrap(), I32(2));
    assert_eq!((U8(5) - I32(3)).unwrap(), I32(2));
    assert_eq!((U8(5) - U128(3)).unwrap(), U128(2));
    assert_eq!((I32(5) - U64(3)).unwrap(), U64(2));
    assert_eq!((U8(5) - U8(3)).unwrap(), U8(2));
    assert_eq!((I128(5) - F32(3.0)).unwrap(), F32(2.0));
}

#[test]
fn mul_chtype() {
    use ChType::*;
    assert_eq!((I32(2) * I32(3)).unwrap(), I32(6));
    assert_eq!((I32(2) * U8(3)).unwrap(), I32(6));
    assert_eq!((U8(2) * U8(3)).unwrap(), U8(6));
    assert_eq!((I128(2) * F64(3.0)).unwrap(), F64(6.0));
}

#[test]
fn div_chtype() {
    use ChType::*;
    assert_eq!((I32(5) / I32(3)).unwrap(), I32(1));
    assert_eq!((I32(3) / F32(2.0)).unwrap(), F32(1.5));
    assert_eq!(I32(3) / Bool(false), Err(ZeroDivisionError));
}
