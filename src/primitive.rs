use crate::{chvalue::ChValue, chvalue::Scope, parser::Node};
use paste::paste;
use std::{cell::RefCell, cmp, collections::LinkedList, fmt, ops, rc::Rc};

use crate::interpreter::ErrType::{self, *};

// pub type Scope = HashMap<String, Primitive>;

#[derive(Debug, Clone)]
pub struct ExpressionData {
    pub nodes: LinkedList<Node>,
    pub ret_last: bool,
    pub scope: Rc<RefCell<Scope>>,
    // pub parent: Rc<RefCell<Scope>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
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

    Ref,
    // DeRef,
    Void,
}

#[derive(Debug, Clone)]
pub enum Primitive {
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

    Ref(Rc<RefCell<ChValue>>),
    // DeRef(Rc<RefCell<Primitive>>),
    Expression(ExpressionData),

    Void,

    UnInit,
}

macro_rules! chnum_as_typ {
    ($typ: ty) => {
        paste! {
            pub fn [<as_$typ>](&self) -> $typ {
                use Primitive::*;
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
        use Primitive::*;
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
        use Primitive::*;
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

fn type_from_bit_size(size: u32, signed: bool, float: bool) -> Option<PrimitiveType> {
    use PrimitiveType::*;
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

fn get_op_numtype(left: &Primitive, right: &Primitive) -> Option<PrimitiveType> {
    if !left.is_num() || !right.is_num() {
        return None;
    }

    let mut v1 = &left; //biggest bitsize
    let mut v2 = &right;

    if right.bitsize() > left.bitsize() {
        (v1, v2) = (v2, v1);
    }

    if v1.is_float() && v2.is_float() {
        return type_from_bit_size(cmp::max(v1.bitsize(), v2.bitsize()), true, true);
    } else if v1.is_float() {
        use PrimitiveType::*;
        if v2.bitsize() <= 32 {
            return Some(F32);
        } else {
            return Some(F64);
        }
    } else if v2.is_float() {
        use PrimitiveType::*;
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

impl Primitive {
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
        [32; U32(_), I32(_), F32(_), USize(_), ISize(_)]
        [64; U64(_), I64(_), F64(_)]
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

    fn from_primitive(typ: PrimitiveType) -> Self {
        use Primitive::*;

        match typ {
            PrimitiveType::Bool => Bool(false),
            PrimitiveType::I8 => I8(0),
            PrimitiveType::I16 => I16(0),
            PrimitiveType::I32 => I32(0),
            PrimitiveType::I64 => I64(0),
            PrimitiveType::ISize => ISize(0),
            PrimitiveType::I128 => I128(0),
            PrimitiveType::U8 => U8(0),
            PrimitiveType::U16 => U16(0),
            PrimitiveType::U32 => U32(0),
            PrimitiveType::U64 => U64(0),
            PrimitiveType::USize => USize(0),
            PrimitiveType::U128 => U128(0),
            PrimitiveType::F32 => F32(0.0),
            PrimitiveType::F64 => F64(0.0),
            PrimitiveType::Char => Char('\0'),
            PrimitiveType::String => String("".to_owned()),
            PrimitiveType::Void => Void,
            PrimitiveType::Ref => Ref(Rc::new(RefCell::new(ChValue::from(Primitive::Void)))),
            // ChType::DeRef => DeRef(Rc::new(RefCell::new(Primitive::Void))),
            PrimitiveType::Expression => panic!("can't initialize primitive"),
        }
    }

    pub fn get_type(&self) -> PrimitiveType {
        use Primitive::*;

        match self {
            Bool(_) => PrimitiveType::Bool,
            I8(_) => PrimitiveType::I8,
            I16(_) => PrimitiveType::I16,
            I32(_) => PrimitiveType::I32,
            I64(_) => PrimitiveType::I64,
            ISize(_) => PrimitiveType::ISize,
            I128(_) => PrimitiveType::I128,
            U8(_) => PrimitiveType::U8,
            U16(_) => PrimitiveType::U16,
            U32(_) => PrimitiveType::U32,
            U64(_) => PrimitiveType::U64,
            USize(_) => PrimitiveType::USize,
            U128(_) => PrimitiveType::U128,
            F32(_) => PrimitiveType::F32,
            F64(_) => PrimitiveType::F64,
            Char(_) => PrimitiveType::Char,
            String(_) => PrimitiveType::String,
            Ref(_) => PrimitiveType::Ref,
            // DeRef(_) => ChType::DeRef,
            Expression(_) => PrimitiveType::Expression,
            Void => PrimitiveType::Void,

            UnInit => panic!("get type should never be called on UnInit"),
        }
    }

    pub fn is_zero(&self) -> bool {
        use Primitive::*;

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

    pub fn as_transformed_num(&self, typ: &PrimitiveType) -> Primitive {
        use Primitive::*;
        match typ {
            PrimitiveType::Bool => Bool(self.as_bool()),
            PrimitiveType::I8 => I8(self.as_i8()),
            PrimitiveType::I16 => I16(self.as_i16()),
            PrimitiveType::I32 => I32(self.as_i32()),
            PrimitiveType::I64 => I64(self.as_i64()),
            PrimitiveType::ISize => ISize(self.as_isize()),
            PrimitiveType::I128 => I128(self.as_i128()),
            PrimitiveType::U8 => U8(self.as_u8()),
            PrimitiveType::U16 => U16(self.as_u16()),
            PrimitiveType::U32 => U32(self.as_u32()),
            PrimitiveType::U64 => U64(self.as_u64()),
            PrimitiveType::USize => USize(self.as_usize()),
            PrimitiveType::U128 => U128(self.as_u128()),
            PrimitiveType::F32 => F32(self.as_f32()),
            PrimitiveType::F64 => F64(self.as_f64()),
            PrimitiveType::Char => Char(self.as_u8() as char),

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

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Primitive::*;

        match self {
            Bool(v) => write!(f, "{}", v),
            I8(v) => write!(f, "{}", v),
            I16(v) => write!(f, "{}", v),
            I32(v) => write!(f, "{}", v),
            I64(v) => write!(f, "{}", v),
            ISize(v) => write!(f, "{}", v),
            I128(v) => write!(f, "{}", v),
            U8(v) => write!(f, "{}", v),
            U16(v) => write!(f, "{}", v),
            U32(v) => write!(f, "{}", v),
            U64(v) => write!(f, "{}", v),
            USize(v) => write!(f, "{}", v),
            U128(v) => write!(f, "{}", v),
            F32(v) => write!(f, "{}", v),
            F64(v) => write!(f, "{}", v),
            Char(v) => write!(f, "{}", v),
            String(v) => write!(f, "{}", v),
            Ref(v) => write!(f, "ref {}", v.borrow()),
            Expression(_) => write!(f, "Expression"),
            Void => write!(f, ""),
            UnInit => write!(f, "UnInit"),
        }
    }
}

impl ops::Add<&Primitive> for &Primitive {
    type Output = Result<Primitive, ErrType>;

    fn add(self, rhs: &Primitive) -> Result<Primitive, ErrType> {
        use Primitive::*;

        if let String(ref s1) = self {
            if let String(s2) = rhs {
                let mut res = s1.to_owned();
                res.push_str(s2);
                return Ok(String(res));
            }
        } else if let Ref(val) = self {
            let v: &Primitive = &val.borrow().value;
            return v + rhs;
        } else if let Ref(val) = rhs {
            let v: &Primitive = &val.borrow().value;
            return self + v;
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

impl ops::Add<Primitive> for Primitive {
    type Output = Result<Primitive, ErrType>;
    fn add(self, rhs: Primitive) -> Result<Primitive, ErrType> {
        &self + &rhs
    }
}

impl ops::Sub<&Primitive> for &Primitive {
    type Output = Result<Primitive, ErrType>;
    fn sub(self, rhs: &Primitive) -> Result<Primitive, ErrType> {
        if let Primitive::Ref(val) = self {
            let v: &Primitive = &val.borrow().value;
            return v - rhs;
        } else if let Primitive::Ref(val) = rhs {
            let v: &Primitive = &val.borrow().value;
            return self - v;
        }

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

impl ops::Sub<Primitive> for Primitive {
    type Output = Result<Primitive, ErrType>;
    fn sub(self, rhs: Primitive) -> Result<Primitive, ErrType> {
        &self - &rhs
    }
}

impl ops::Mul<&Primitive> for &Primitive {
    type Output = Result<Primitive, ErrType>;
    fn mul(self, rhs: &Primitive) -> Result<Primitive, ErrType> {
        use Primitive::*;

        if rhs.is_num() {
            if let String(v) = self {
                return Ok(String(v.repeat(rhs.as_usize())));
            }
            //TODO: Char mult?
            // else if let Char(v) = self {
            //     return Ok(String(v.to_string().repeat(rhs.to_usize())));
            // }
        }

        if let Primitive::Ref(val) = self {
            let v: &Primitive = &val.borrow().value;
            return v * rhs;
        } else if let Ref(val) = rhs {
            let v: &Primitive = &val.borrow().value;
            return self * v;
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

impl ops::Mul<Primitive> for Primitive {
    type Output = Result<Primitive, ErrType>;
    fn mul(self, rhs: Primitive) -> Result<Primitive, ErrType> {
        &self * &rhs
    }
}

impl ops::Div<&Primitive> for &Primitive {
    type Output = Result<Primitive, ErrType>;
    fn div(self, rhs: &Primitive) -> Result<Primitive, ErrType> {
        if let Primitive::Void = rhs {
            return Err(ErrType::UnsupportedOperand(format!(
                "Operator [Div] not defined for {:?} / {:?}",
                self.get_type(),
                rhs.get_type()
            )));
        } else if let Primitive::Ref(val) = self {
            let v: &Primitive = &val.borrow().value;
            return v / rhs;
        } else if let Primitive::Ref(val) = rhs {
            let v: &Primitive = &val.borrow().value;
            return self / v;
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

impl ops::Div<Primitive> for Primitive {
    type Output = Result<Primitive, ErrType>;
    fn div(self, rhs: Primitive) -> Result<Primitive, ErrType> {
        &self / &rhs
    }
}

impl PartialEq for Primitive {
    fn eq(&self, other: &Self) -> bool {
        use Primitive::*;

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

impl PartialOrd for Primitive {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        if self == other {
            return Some(cmp::Ordering::Equal);
        }

        if let (Primitive::String(s1), Primitive::String(s2)) = (self, other) {
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
fn add_primitive() {
    use Primitive::*;
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
fn sub_primitive() {
    use Primitive::*;
    assert_eq!((I32(5) - Bool(true)).unwrap(), I32(4));
    assert_eq!((I32(5) - I32(3)).unwrap(), I32(2));
    assert_eq!((U8(5) - I32(3)).unwrap(), I32(2));
    assert_eq!((U8(5) - U128(3)).unwrap(), U128(2));
    assert_eq!((I32(5) - U64(3)).unwrap(), U64(2));
    assert_eq!((U8(5) - U8(3)).unwrap(), U8(2));
    assert_eq!((I128(5) - F32(3.0)).unwrap(), F32(2.0));
}

#[test]
fn mul_primitive() {
    use Primitive::*;
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
fn div_primitive() {
    use Primitive::*;
    assert_eq!((I32(5) / I32(3)).unwrap(), I32(1));
    assert_eq!((I32(3) / F32(2.0)).unwrap(), F32(1.5));
    assert_eq!(I32(3) / Bool(false), Err(ZeroDivision));
    assert_eq!(F32(3.0) / Char('\0'), Err(ZeroDivision));
}

#[test]
fn eq_primitive() {
    use Primitive::*;
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
fn primitive_to_bool() {
    use Primitive::*;
    assert!(F32(2.0).as_bool());
    assert!(!U8(0).as_bool());
    assert!(!Void.as_bool());
    assert!(String("".to_owned()).as_bool());
}

#[test]
fn primitive_overflow() {
    use Primitive::*;
    assert_eq!(I32(i32::MAX) + I32(1), Ok(I64(i32::MAX as i64 + 1)));
    assert_eq!(I32(i32::MAX) + F32(1.0), Ok(F32(i32::MAX as f32 + 1.0)));
    assert_eq!(Bool(false) + Bool(false), Ok(I8(0)));
}

#[test]
fn primitive_order() {
    use Primitive::*;
    assert!(Char('a') < Char('b'));
    assert!(Bool(true) < Char('b'));
    assert!(F32(1.3) > I128(1));
    assert!(F32(1.0) >= I128(1));
    assert!(String("Hello".to_owned()) >= String("Hello".to_owned()));
    assert!(String("Hello World".to_owned()) > String("Hello".to_owned()));
    assert!(!(Void > Char('d')));
}
