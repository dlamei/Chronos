use paste::paste;
// use crate::parser::{Node, NodeType};
use std::{cmp, fmt, ops};

use crate::interpreter::ErrType::{self, *};

#[derive(Debug, Clone)]
pub enum ChType {
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

    // Expression(LinkedList<Node>),
    Void,
}

macro_rules! unwrap_chtype {
    ($chtype:ident, $in:ident, $e:expr) => {
        match $chtype {
            ChType::Bool($in) => $e,

            ChType::I8($in) => $e,
            ChType::I16($in) => $e,
            ChType::I32($in) => $e,
            ChType::I64($in) => $e,
            ChType::ISize($in) => $e,
            ChType::I128($in) => $e,

            ChType::U8($in) => $e,
            ChType::U16($in) => $e,
            ChType::U32($in) => $e,
            ChType::U64($in) => $e,
            ChType::USize($in) => $e,
            ChType::U128($in) => $e,

            ChType::F32($in) => $e,
            ChType::F64($in) => $e,

            ChType::Char($in) => $e,
            ChType::String($in) => $e,
            _ => panic!("unwrap_chtype not implemented for {:?}", $chtype),
        }
    };
}

macro_rules! chnum_as_typ {
    ($typ: ty) => {
        paste! {
            pub fn [<as_$typ>](&self) -> $typ {
                use ChType::*;
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
    ($lhs:expr; $op:tt $rhs:expr; $checked:ident) => {
        match ($lhs, $rhs) {
            (ChType::Bool(v1), ChType::Bool(v2)) => ChType::U8(*v1 as u8 $op *v2 as u8),
            (ChType::Char(v1), ChType::Char(v2)) => {
                if let Some(v) = (*v1 as u8).$checked(*v2 as u8) {
                    ChType::Char(v as char)
                } else {
                    ChType::U16(*v1 as u16 $op *v2 as u16)
                }
            }
            (ChType::U8(v1), ChType::U8(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    ChType::U8(v)
                } else {
                    ChType::U16(*v1 as u16 $op *v2 as u16)
                }
            }
            (ChType::U16(v1), ChType::U16(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    ChType::U16(v)
                } else {
                    ChType::U32(*v1 as u32 $op *v2 as u32)
                }
            }
            (ChType::U32(v1), ChType::U32(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    ChType::U32(v)
                } else {
                    ChType::U64(*v1 as u64 $op *v2 as u64)
                }
            }
            (ChType::U64(v1), ChType::U64(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    ChType::U64(v)
                } else {
                    ChType::U128(*v1 as u128 $op *v2 as u128)
                }
            }
            (ChType::U128(v1), ChType::U128(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    ChType::U128(v)
                } else {
                    todo!()
                }
            }
            (ChType::I8(v1), ChType::I8(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    ChType::I8(v)
                } else {
                    ChType::I16(*v1 as i16 $op *v2 as i16)
                }
            }
            (ChType::I16(v1), ChType::I16(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    ChType::I16(v)
                } else {
                    ChType::I32(*v1 as i32 $op *v2 as i32)
                }
            }
            (ChType::I32(v1), ChType::I32(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    ChType::I32(v)
                } else {
                    ChType::I64(*v1 as i64 $op *v2 as i64)
                }
            }
            (ChType::I64(v1), ChType::I64(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    ChType::I64(v)
                } else {
                    ChType::I128(*v1 as i128 $op *v2 as i128)
                }
            }
            (ChType::I128(v1), ChType::I128(v2)) => {
                if let Some(v) = v1.$checked(*v2) {
                    ChType::I128(v)
                } else {
                    todo!()
                }
            }

            (ChType::F32(v1), ChType::F32(v2)) => ChType::F32(*v1 $op *v2),
            (ChType::F64(v1), ChType::F64(v2)) => ChType::F64(*v1 $op *v2),
            _ => panic!(
                "can't operate on type {} with type {}",
                $lhs.get_type(),
                $rhs.get_type()
            ),
        }
    };

    ($lhs:expr; $op:tt $rhs:expr; => $rest:expr) => {
        match ($lhs, $rhs) {
            (ChType::Bool(v1), ChType::Bool(v2)) => *v1 as u8 $op *v2 as u8,
            (ChType::U8(v1), ChType::U8(v2)) => *v1 $op *v2,
            (ChType::U16(v1), ChType::U16(v2)) => *v1 $op *v2,
            (ChType::U32(v1), ChType::U32(v2)) => *v1 $op *v2,
            (ChType::U64(v1), ChType::U64(v2)) => *v1 $op *v2,
            (ChType::U128(v1), ChType::U128(v2)) => *v1 $op *v2,
            (ChType::I8(v1), ChType::I8(v2)) => *v1 $op *v2,
            (ChType::I16(v1), ChType::I16(v2)) => *v1 $op *v2,
            (ChType::I32(v1), ChType::I32(v2)) => *v1 $op *v2,
            (ChType::I64(v1), ChType::I64(v2)) => *v1 $op *v2,
            (ChType::I128(v1), ChType::I128(v2)) => *v1 $op *v2,

            (ChType::F32(v1), ChType::F32(v2)) => *v1 $op *v2,
            (ChType::F64(v1), ChType::F64(v2)) => *v1 $op *v2,
            (ChType::Char(v1), ChType::Char(v2)) => *v1 $op *v2,
            _ => $rest,
            // _ => panic!(
            //     "can't operate on type {} with type {}",
            //     $lhs.get_type(),
            //     $rhs.get_type()
            // ),
        }
    };
}

fn from_bit_size(size: u32, signed: bool, float: bool) -> Option<ChType> {
    use ChType::*;
    if !float {
        if signed {
            match size {
                8 => Some(I8(0)),
                16 => Some(I16(0)),
                32 => Some(I32(0)),
                64 => Some(I64(0)),
                128 => Some(I128(0)),
                _ => None,
            }
        } else {
            match size {
                8 => Some(U8(0)),
                16 => Some(U16(0)),
                32 => Some(U32(0)),
                64 => Some(U64(0)),
                128 => Some(U128(0)),
                _ => None,
            }
        }
    } else {
        match size {
            32 => Some(F32(0.0)),
            64 => Some(F64(0.0)),
            _ => panic!("no unsigned int chtype with size: {}", size),
        }
    }
}

fn get_op_numtype(lhs: &ChType, rhs: &ChType) -> Option<ChType> {
    use ChType::*;

    if !lhs.is_num() || !rhs.is_num() {
        return None;
    }

    if lhs.is_float() && rhs.is_float() {
        return from_bit_size(cmp::max(lhs.get_bit_size(), rhs.get_bit_size()), true, true);
    } else if lhs.is_float() {
        if rhs.get_bit_size() <= 32 {
            return Some(F32(0.0));
        } else {
            return Some(F64(0.0));
        }
    } else if rhs.is_float() {
        if lhs.get_bit_size() <= 32 {
            return Some(F32(0.0));
        } else {
            return Some(F64(0.0));
        }
    }

    if lhs.get_bit_size() > rhs.get_bit_size() {
        from_bit_size(lhs.get_bit_size(), lhs.is_signed(), false)
    } else if lhs.get_bit_size() < rhs.get_bit_size() {
        from_bit_size(rhs.get_bit_size(), rhs.is_signed(), false)
    } else if lhs.get_bit_size() == rhs.get_bit_size() && lhs.is_signed() == rhs.is_signed() {
        from_bit_size(lhs.get_bit_size(), lhs.is_signed(), false)
    } else {
        from_bit_size(lhs.get_bit_size() * 2, true, false)
    }
}

impl ChType {
    #[cfg(target_pointer_width = "64")]
    crate::assign_func!(get_bit_size -> u32, 0,
        [8; Bool(_), Char(_), U8(_), I8(_)]
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
        [true; U32(_), I32(_), F32(_)]
        [true; U64(_), USize(_), I64(_), ISize(_), F64(_)]
        [true; U128(_), I128(_)]
    );

    pub fn get_type(&self) -> String {
        use ChType::*;

        match self {
            Bool(_) => "bool",
            I8(_) => "i8",
            I16(_) => "i16",
            I32(_) => "i32",
            I64(_) => "i64",
            ISize(_) => "isize",
            I128(_) => "i128",
            U8(_) => "u8",
            U16(_) => "u16",
            U32(_) => "u32",
            U64(_) => "u64",
            USize(_) => "usize",
            U128(_) => "u128",
            F32(_) => "f32",
            F64(_) => "f64",
            Char(_) => "char",
            String(_) => "string",
            Void => "void",
        }
        .to_owned()
    }

    pub fn is_zero(&self) -> bool {
        use ChType::*;

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

    pub fn as_transformed_num(&self, typ: &ChType) -> ChType {
        use ChType::*;
        match typ {
            Bool(_) => Bool(self.as_bool()),
            I8(_) => I8(self.as_i8()),
            I16(_) => I16(self.as_i16()),
            I32(_) => I32(self.as_i32()),
            I64(_) => I64(self.as_i64()),
            ISize(_) => ISize(self.as_isize()),
            I128(_) => I128(self.as_i128()),
            U8(_) => U8(self.as_u8()),
            U16(_) => U16(self.as_u16()),
            U32(_) => U32(self.as_u32()),
            U64(_) => U64(self.as_u64()),
            USize(_) => USize(self.as_usize()),
            U128(_) => U128(self.as_u128()),
            F32(_) => F32(self.as_f32()),
            F64(_) => F64(self.as_f64()),

            _ => panic!("can't transorm {} to type: {}", self, typ.get_type()),
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

impl fmt::Display for ChType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let ChType::Void = self {
            write!(f, "")
        } else {
            unwrap_chtype!(self, e, write!(f, "{}", e.to_string()))
        }
    }
}

impl ops::Add<ChType> for ChType {
    type Output = Result<ChType, ErrType>;

    fn add(self, rhs: ChType) -> Result<ChType, ErrType> {
        use ChType::*;

        if let String(ref s1) = self {
            if let String(s2) = rhs {
                return Ok(String(format!("{}{}", s1, s2)));
            }
        }

        //TODO: id system
        if let Some(typ) = get_op_numtype(&self, &rhs) {
            let lhs = self.as_transformed_num(&typ);
            let rhs = rhs.as_transformed_num(&typ);

            Ok(apply_op!(&lhs; + &rhs; checked_add))
        } else {
            Err(ErrType::UnsupportedOperand(format!(
                "Operator [Add] not defined for {} + {}",
                self.get_type(),
                rhs.get_type()
            )))
        }
    }
}

impl ops::Sub<ChType> for ChType {
    type Output = Result<ChType, ErrType>;
    fn sub(self, rhs: ChType) -> Result<ChType, ErrType> {
        if let Some(typ) = get_op_numtype(&self, &rhs) {
            let lhs = self.as_transformed_num(&typ);
            let rhs = rhs.as_transformed_num(&typ);

            Ok(apply_op!(&lhs; - &rhs; checked_sub))
        } else {
            Err(ErrType::UnsupportedOperand(format!(
                "Operator [Sub] not defined for {} - {}",
                self.get_type(),
                rhs.get_type()
            )))
        }
    }
}

impl ops::Mul<ChType> for ChType {
    type Output = Result<ChType, ErrType>;
    fn mul(self, rhs: ChType) -> Result<ChType, ErrType> {
        use ChType::*;

        if rhs.is_num() {
            if let String(v) = self {
                return Ok(String(v.repeat(rhs.as_usize())));
            }
            // else if let Char(v) = self {
            //     return Ok(String(v.to_string().repeat(rhs.to_usize())));
            // }
        }

        if let Some(typ) = get_op_numtype(&self, &rhs) {
            let lhs = self.as_transformed_num(&typ);
            let rhs = rhs.as_transformed_num(&typ);

            let res = apply_op!(&lhs; * &rhs; checked_mul);
            Ok(res)
        } else {
            Err(ErrType::UnsupportedOperand(format!(
                "Operator [Mul] not defined for {} * {}",
                self.get_type(),
                rhs.get_type()
            )))
        }
    }
}

impl ops::Div<ChType> for ChType {
    type Output = Result<ChType, ErrType>;
    fn div(self, rhs: ChType) -> Result<ChType, ErrType> {
        if let ChType::Void = rhs {
            return Err(ErrType::UnsupportedOperand(format!(
                "Operator [Div] not defined for {} / {}",
                self.get_type(),
                rhs.get_type()
            )));
        }

        if rhs.is_zero() {
            return Err(ZeroDivision);
        }

        if let Some(typ) = get_op_numtype(&self, &rhs) {
            let lhs = self.as_transformed_num(&typ);
            let rhs = rhs.as_transformed_num(&typ);

            Ok(apply_op!(&lhs; / &rhs; checked_div))
        } else {
            Err(ErrType::UnsupportedOperand(format!(
                "Operator [Div] not defined for {} / {}",
                self.get_type(),
                rhs.get_type()
            )))
        }
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
            Void => {
                if let Void = other {
                    return true;
                } else {
                    return false;
                }
            }
            _ => (),
        }

        if let Some(typ) = get_op_numtype(&self, &other) {
            let lhs = self.as_transformed_num(&typ);
            let rhs = other.as_transformed_num(&typ);

            apply_op!(&lhs; == &rhs; => false)
        } else {
            false
        }
    }
}

impl PartialOrd for ChType {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        let res: bool;

        if let Some(typ) = get_op_numtype(&self, &other) {
            let lhs = self.as_transformed_num(&typ);
            let rhs = other.as_transformed_num(&typ);

            res = apply_op!(&lhs; > &rhs; => false);
        } else {
            res = false;
        }

        // let res = apply_op!(&self; > &other;);
        if res {
            Some(cmp::Ordering::Greater)
        } else {
            Some(cmp::Ordering::Less)
        }
    }
}

#[test]
fn add_chtype() {
    use ChType::*;
    assert_eq!((I32(2) + Bool(false)).unwrap(), I32(2));
    assert_eq!((I32(2) + Char(std::char::from_u32(10).unwrap())), Ok(I32(12)));
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
    assert_eq!(
        (String("a".to_owned()) * I32(3)).unwrap(),
        String("aaa".to_owned())
    );
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

    assert_eq!(Void, Void);
}

#[test]
fn chtype_to_bool() {
    use ChType::*;
    assert!(F32(2.0).as_bool());
    assert!(!U8(0).as_bool());
    assert!(!Void.as_bool());
    assert!(String("".to_owned()).as_bool());
}

#[test]
fn chtype_overflow() {
    use ChType::*;
    assert_eq!(I32(i32::MAX) + I32(1), Ok(I64(i32::MAX as i64 + 1)));
    assert_eq!(I32(i32::MAX) + F32(1.0), Ok(F32(i32::MAX as f32 + 1.0)));
    assert_eq!(Bool(false) + Bool(false), Ok(I8(0)));
}
