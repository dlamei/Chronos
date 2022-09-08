use paste::paste;
use std::{cmp, fmt, ops};

use crate::interpreter::ErrType::{self, *};

//TODO: custom PartialEq
#[derive(Debug, Clone, PartialOrd)]
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

macro_rules! chnum_to_typ {
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

macro_rules! apply_num_op {
    ($lhs:ident $op:tt $rhs:ident) => {{
        let max = cmp::max($lhs.priority(), $rhs.priority());

        match max {
            1 => ChType::U8($lhs.to_u8() $op $rhs.to_u8()),
            2 => ChType::U8($lhs.to_u8() $op $rhs.to_u8()),
            3 => ChType::I8($lhs.to_i8() $op $rhs.to_i8()),
            4 => ChType::U32($lhs.to_u32() $op $rhs.to_u32()),
            5 => ChType::I32($lhs.to_i32() $op $rhs.to_i32()),
            6 => ChType::U64($lhs.to_u64() $op $rhs.to_u64()),
            7 => ChType::I64($lhs.to_i64() $op $rhs.to_i64()),
            8 => ChType::U128($lhs.to_u128() $op $rhs.to_u128()),
            9 => ChType::I128($lhs.to_i128() $op $rhs.to_i128()),
            10 => ChType::F32($lhs.to_f32() $op $rhs.to_f32()),
            11 => ChType::F64($lhs.to_f64() $op $rhs.to_f64()),
            _ => panic!(
                "Operator not defined for {:?} {} {:?}",
                $lhs,
                stringify!($op),
                $rhs,
            ),
        }
    }};

    ($pat:pat, $lhs:ident $op:tt $rhs:ident) => {{
        let max = cmp::max($lhs.priority(), $rhs.priority());

        paste!(
        match max {
            1 => $pat($lhs.to_u8() $op $rhs.to_u8()),
            2 => $pat($lhs.to_u8() $op $rhs.to_u8()),
            3 => $pat($lhs.to_i8() $op $rhs.to_i8()),
            4 => $pat($lhs.to_u32() $op $rhs.to_u32()),
            5 => $pat($lhs.to_i32() $op $rhs.to_i32()),
            6 => $pat($lhs.to_u64() $op $rhs.to_u64()),
            7 => $pat($lhs.to_i64() $op $rhs.to_i64()),
            8 => $pat($lhs.to_u128() $op $rhs.to_u128()),
            9 => $pat($lhs.to_i128() $op $rhs.to_i128()),
            10 => $pat($lhs.to_f32() $op $rhs.to_f32()),
            11 => $pat($lhs.to_f64() $op $rhs.to_f64()),
            _ => panic!(
                "Operator not defined for {:?} {} {:?}",
                $lhs,
                stringify!($op),
                $rhs,
            ),
        })
    }};
}

impl ChType {
    #[cfg(target_pointer_width = "64")]
    crate::assign_func!(priority -> u32, 0,
        [1; Bool(_)]
        [2; U8(_)]
        [3; I8(_)]
        [4; U32(_)]
        [5; I32(_)]
        [6; U64(_), USize(_)]
        [7; I64(_), ISize(_)]
        [8; U128(_)]
        [9; I128(_)]
        [10; F32(_)]
        [11; F64(_)]
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

    fn is_zero(&self) -> bool {
        use ChType::*;

        match self {
            Bool(false) | I8(0i8) | I32(0i32) | I64(0i64) | ISize(0isize) | I128(0i128)
            | U8(0u8) | U32(0u32) | U64(0u64) | USize(0usize) | U128(0u128) | Char('\0') => true,

            F32(v) => v == &0.0,

            F64(v) => v == &0.0,
            _ => false,
        }
    }

    fn to_bool(&self) -> bool {
        !self.is_zero()
    }

    chnum_to_typ!(i8);
    chnum_to_typ!(i32);
    chnum_to_typ!(i64);
    chnum_to_typ!(isize);
    chnum_to_typ!(i128);

    chnum_to_typ!(u8);
    chnum_to_typ!(u32);
    chnum_to_typ!(u64);
    chnum_to_typ!(usize);
    chnum_to_typ!(u128);

    chnum_to_typ!(f32);
    chnum_to_typ!(f64);
}

impl fmt::Display for ChType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unwrap_chtype!(self, e, write!(f, "{}", e.to_string()))
    }
}

impl ops::Add<ChType> for ChType {
    type Output = Result<ChType, ErrType>;

    fn add(self, rhs: ChType) -> Result<ChType, ErrType> {
        if let ChType::String(val) = self {
            return Ok(ChType::String(format!("{}{}", val, rhs)));
        } else if let ChType::Char(_) = self {
            return Err(ErrType::UnsupportedOperand(
                "Operator [Add] not defined for Char".to_owned(),
            ));
        }

        Ok(apply_num_op!(self + rhs))
    }
}

impl ops::Sub<ChType> for ChType {
    type Output = Result<ChType, ErrType>;
    fn sub(self, rhs: ChType) -> Result<ChType, ErrType> {
        match self {
            ChType::String(_) => {
                return Err(UnsupportedOperand(
                    "Operator [Sub] not defined for String".to_owned(),
                ))
            }
            ChType::Char(_) => {
                return Err(UnsupportedOperand(
                    "Operator [Sub] not defined for Char".to_owned(),
                ))
            }
            _ => (),
        }

        Ok(apply_num_op!(self - rhs))
    }
}

impl ops::Mul<ChType> for ChType {
    type Output = Result<ChType, ErrType>;
    fn mul(self, rhs: ChType) -> Result<ChType, ErrType> {
        use ChType::*;

        if let String(v) = self {
            return Ok(String(v.repeat(rhs.to_usize())));
        } else if let Char(v) = self {
            return Ok(String(v.to_string().repeat(rhs.to_usize())));
        }

        Ok(apply_num_op!(self * rhs))
    }
}

impl ops::Div<ChType> for ChType {
    type Output = Result<ChType, ErrType>;
    fn div(self, rhs: ChType) -> Result<ChType, ErrType> {
        match self {
            ChType::String(_) => {
                return Err(UnsupportedOperand(
                    "Operator [Div] not defined for String".to_owned(),
                ))
            }
            ChType::Char(_) => {
                return Err(UnsupportedOperand(
                    "Operator [Div] not defined for Char".to_owned(),
                ))
            }
            _ => (),
        }

        if rhs.is_zero() {
            return Err(ZeroDivision);
        }

        Ok(apply_num_op!(self / rhs))
    }
}

impl PartialEq for ChType {
    fn eq(&self, other: &Self) -> bool {
        use ChType::*;

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
            _ => (),
        }

        let lhs = self.clone();
        let rhs = other.clone();
        let res = apply_num_op!(ChType::Bool, lhs == rhs);

        if let ChType::Bool(v) = res {
            v
        } else {
            panic!()
        }
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
        (String("Hello".to_owned()) + String(" World!".to_owned())).unwrap(),
        String("Hello World!".to_owned())
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
    assert_eq!(I32(3) / Bool(false), Err(ZeroDivision));
    assert_eq!(F32(3.0) / Char('\0'), Err(ZeroDivision));
}

#[test]
fn eq_chtype() {
    use ChType::*;
    assert_eq!(I32(5), I32(5));
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
}
