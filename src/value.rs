use super::{AllocId, Gc, GcStr, Result, RuntimeError, RuntimeErrorTy};
use std::ops;

/// The length of an encoded Value
pub const VALUE_LENGTH: usize = 9;

/// A value contained in the GC
#[derive(Debug, Clone, Eq)]
pub enum Value {
    Int(i32),
    Bool(bool),
    String(String),
    Pointer(usize),
    None,
}

impl std::fmt::Display for Value {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(inner) => write!(fmt, "{}", inner),
            Self::Bool(inner) => write!(fmt, "{}", inner),
            Self::String(inner) => write!(fmt, "{}", inner),
            Self::Pointer(inner) => write!(fmt, "0x{:04X}", inner),
            Self::None => write!(fmt, "null"),
        }
    }
}

impl Value {
    /// Get the type of the Value as a static str
    #[inline]
    pub fn ty(&self) -> &'static str {
        match self {
            Self::Int(_) => "int",
            Self::Bool(_) => "bool",
            Self::String(_) => "str",
            Self::Pointer(_) => "ptr",
            Self::None => "none",
        }
    }

    /// Convert the Value into its Bytecode representation
    // TODO: Test this
    #[inline]
    pub fn as_bytes(&self) -> ([u8; VALUE_LENGTH], Option<&[u8]>) {
        use std::mem::size_of;

        let mut bytes = [0; VALUE_LENGTH];
        let mut string_bytes = None;

        match self {
            Self::None => {
                bytes[0] = 0x00;
            }
            Self::Int(i) => {
                bytes[0] = 0x01;
                bytes[1..size_of::<i32>() + 1].copy_from_slice(&i.to_be_bytes());
            }
            Self::Bool(b) => {
                bytes[0] = 0x02;
                bytes[1] = *b as u8;
            }
            Self::String(s) => {
                bytes[0] = 0x03;
                string_bytes = Some(s.as_bytes());
            }
            Self::Pointer(p) => {
                bytes[0] = 0x04;
                bytes[1..size_of::<usize>() + 1].copy_from_slice(&p.to_be_bytes());
            }
        }

        (bytes, string_bytes)
    }

    /// Create a Value from its Bytecode representation
    // TODO: Test this
    #[inline]
    pub fn from_bytes(
        value: [u8; VALUE_LENGTH],
        strings: &mut std::collections::VecDeque<String>,
    ) -> std::result::Result<Self, &'static str> {
        use std::{convert::TryInto, mem::size_of};

        Ok(match value[0] {
            0x00 => Self::None,
            0x01 => Self::Int(i32::from_be_bytes(
                match value[1..size_of::<i32>() + 1].try_into() {
                    Ok(val) => val,
                    Err(_) => return Err("Invalid integer"),
                },
            )),
            0x02 => Self::Bool(value[1] > 0),
            0x03 => Self::String(match strings.pop_front() {
                Some(s) => s,
                None => return Err("Not enough strings supplied"),
            }),
            0x04 => Self::Pointer(usize::from_be_bytes(
                match value[1..size_of::<usize>() + 1].try_into() {
                    Ok(val) => val,
                    Err(_) => return Err("Invalid integer"),
                },
            )),

            _ => return Err("Invalid Value Header"),
        })
    }
}

// TODO: Test all implemented operations

impl ops::Add for Value {
    type Output = Result<Self>;

    #[inline]
    fn add(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(right + left)),
            (Self::None, Self::None) => Ok(Self::None),
            (Self::String(left), Self::String(right)) => {
                let mut output = Vec::with_capacity(left.as_bytes().len() + right.as_bytes().len());
                output.extend_from_slice(left.as_bytes());
                output.extend_from_slice(right.as_bytes());

                Ok(Self::String(unsafe { String::from_utf8_unchecked(output) }))
            }

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot add variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::Sub for Value {
    type Output = Result<Self>;

    #[inline]
    fn sub(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(right - left)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot subtract variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::Mul for Value {
    type Output = Result<Self>;

    #[inline]
    fn mul(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(right * left)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot multiply variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::Div for Value {
    type Output = Result<Self>;

    #[inline]
    fn div(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => {
                if right == 0 {
                    Err(RuntimeError {
                        ty: RuntimeErrorTy::DivideByZero,
                        message: "Cannot divide by zero".to_string(),
                    })
                } else {
                    Ok(Self::Int(left / right))
                }
            }

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot divide variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::BitAnd for Value {
    type Output = Result<Self>;

    #[inline]
    fn bitand(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(right & left)),
            (Self::Bool(left), Self::Bool(right)) => Ok(Self::Bool(right & left)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot bitwise and variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::BitOr for Value {
    type Output = Result<Self>;

    #[inline]
    fn bitor(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(right | left)),
            (Self::Bool(left), Self::Bool(right)) => Ok(Self::Bool(right | left)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot bitwise or variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::BitXor for Value {
    type Output = Result<Self>;

    #[inline]
    fn bitxor(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(right ^ left)),
            (Self::Bool(left), Self::Bool(right)) => Ok(Self::Bool(right ^ left)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot bitwise xor variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::Not for Value {
    type Output = Result<Self>;

    #[inline]
    fn not(self) -> Self::Output {
        match self {
            Self::Int(int) => Ok(Self::Int(!int)),
            Self::Bool(boolean) => Ok(Self::Bool(!boolean)),

            val => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!("Cannot apply not to variable of type {}", val.ty()),
            }),
        }
    }
}

impl std::cmp::PartialOrd for Value {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Some(left.cmp(right)),
            (Self::Bool(left), Self::Bool(right)) => Some(left.cmp(right)),
            (Self::String(left), Self::String(right)) => Some(left.cmp(right)),
            (Self::Pointer(left), Self::Pointer(right)) => Some(left.cmp(right)),

            (_, _) => None,
        }
    }
}

impl std::cmp::PartialEq for Value {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => left == right,
            (Self::Bool(left), Self::Bool(right)) => left == right,
            (Self::String(left), Self::String(right)) => left == right,
            (Self::Pointer(left), Self::Pointer(right)) => left == right,
            (Self::None, Self::None) => true,

            (_, _) => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RuntimeValue {
    Cached(CachedValue),
    Register(RegisterValue),
    None,
}

impl std::fmt::Display for RuntimeValue {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Cached(inner) => write!(fmt, "{}", inner),
            Self::Register(inner) => write!(fmt, "{}", inner),
            Self::None => write!(fmt, "NoneType"),
        }
    }
}

impl RuntimeValue {
    pub fn fetch(&self, gc: &Gc) -> Result<RegisterValue> {
        match self {
            Self::Cached(cached) => cached.get_val(&gc),
            Self::Register(register) => Ok(*register),
            Self::None => Err(RuntimeError {
                ty: RuntimeErrorTy::NullVar,
                message: "The requested variable does not exist".to_string(),
            }),
        }
    }

    pub fn from_val(val: Value) -> Self {
        if val == Value::None {
            Self::None
        } else {
            Self::Register(RegisterValue::from_val(val))
        }
    }

    pub fn eq(&self, other: &Self, gc: &Gc) -> Result<bool> {
        Ok(match (self, other) {
            (Self::Register(left), Self::Register(right)) => left == right,
            (Self::Cached(left), Self::Cached(right)) => left.eq(right, gc)?,
            (Self::Cached(cached), Self::Register(reg))
            | (Self::Register(reg), Self::Cached(cached)) => cached.eq_reg(reg, gc)?,
            (Self::None, Self::None) => true,
            (_, _) => false,
        })
    }

    pub fn not(self, gc: &Gc) -> Result<Self> {
        match self {
            Self::Cached(c) => Ok(Self::Cached(c.not(gc)?)),
            Self::Register(r) => Ok(Self::Register((!r)?)),
            Self::None => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: "Cannot apply not NoneType".to_string(),
            }),
        }
    }

    pub fn cmp(&self, other: &Self, gc: &Gc) -> Result<Option<std::cmp::Ordering>> {
        Ok(match (self, other) {
            (Self::Cached(l), Self::Cached(r)) => l.cmp(r, &gc)?,
            (Self::Register(l), Self::Register(r)) => l.partial_cmp(&r),
            (Self::Cached(l), Self::Register(r)) => l.get_val(&gc)?.partial_cmp(&r),
            (Self::Register(l), Self::Cached(r)) => l.partial_cmp(&r.get_val(&gc)?),
            (Self::None, Self::None) => None,
            (_, _) => {
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::IncompatibleTypes,
                    message: "Those two types cannot be operated on".to_string(),
                })
            }
        })
    }

    pub fn is_register(&self) -> bool {
        if let Self::Register(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_cached(&self) -> bool {
        if let Self::Cached(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_none(&self) -> bool {
        if let Self::None = self {
            true
        } else {
            false
        }
    }

    pub fn register(&self) -> Option<RegisterValue> {
        if let Self::Register(r) = self {
            Some(*r)
        } else {
            None
        }
    }

    pub fn cached(&self) -> Option<CachedValue> {
        if let Self::Cached(c) = self {
            Some(*c)
        } else {
            None
        }
    }
}

macro_rules! generate_op {
    ($( [$t:ty, $token:tt, $name:tt] ),*) => {
        $(
            impl $t {
                pub fn $name(&self, other: &Self, gc: &Gc) -> Result<Self> {
                    Ok(Self::Register(match (self, other) {
                        (Self::Cached(l), Self::Cached(r)) => (l.get_val(&gc)? $token r.get_val(&gc)?)?,
                        (Self::Register(l), Self::Register(r)) => (*l $token *r)?,
                        (Self::Cached(l), Self::Register(r)) => (l.get_val(&gc)? $token *r)?,
                        (Self::Register(l), Self::Cached(r)) => (*l $token r.get_val(&gc)?)?,
                        (Self::None, Self::None) => return Ok(Self::None),
                        (_, _) => return Err(RuntimeError {
                            ty: RuntimeErrorTy::IncompatibleTypes,
                            message: "Those two types cannot be operated on".to_string(),
                        })
                    }))
                }
            }
        )*
    }
}

generate_op! {
    [RuntimeValue, +, add],
    [RuntimeValue, -, sub],
    [RuntimeValue, *, mult],
    [RuntimeValue, /, div],
    [RuntimeValue, |, bit_or],
    [RuntimeValue, ^, bit_xor],
    [RuntimeValue, &, bit_and]
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegisterValue {
    String(&'static str),
    Int(i32),
    Bool(bool),
    Pointer(usize),
}

impl std::fmt::Display for RegisterValue {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(inner) => write!(fmt, "{}", inner),
            Self::Int(inner) => write!(fmt, "{}", inner),
            Self::Bool(inner) => write!(fmt, "{}", inner),
            Self::Pointer(inner) => write!(fmt, "{:#p}", inner),
        }
    }
}

impl RegisterValue {
    #[inline]
    pub fn ty(&self) -> &'static str {
        match self {
            Self::Int(_) => "int",
            Self::Bool(_) => "bool",
            Self::String(_) => "str",
            Self::Pointer(_) => "ptr",
        }
    }

    pub fn from_val(val: Value) -> Self {
        match val {
            Value::Int(i) => Self::Int(i),
            Value::Bool(b) => Self::Bool(b),
            Value::String(s) => Self::String(unsafe { std::mem::transmute(&*s) }),
            Value::Pointer(p) => Self::Pointer(p),
            _ => unreachable!(),
        }
    }

    pub fn to_value(self) -> Value {
        match self {
            Self::Int(i) => Value::Int(i),
            Self::Bool(b) => Value::Bool(b),
            Self::String(s) => Value::String(s.to_string()),
            Self::Pointer(p) => Value::Pointer(p),
        }
    }
}

impl ops::Add for RegisterValue {
    type Output = Result<Self>;

    #[inline]
    fn add(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(left + right)),
            (Self::String(left), Self::String(right)) => {
                let mut output = Vec::with_capacity(left.as_bytes().len() + right.as_bytes().len());
                output.extend_from_slice(left.as_bytes());
                output.extend_from_slice(right.as_bytes());

                Ok(Self::String(unsafe {
                    std::mem::transmute(std::str::from_utf8_unchecked(&output))
                }))
            }

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot add variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::Sub for RegisterValue {
    type Output = Result<Self>;

    #[inline]
    fn sub(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(left - right)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot subtract variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::Mul for RegisterValue {
    type Output = Result<Self>;

    #[inline]
    fn mul(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(left * right)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot multiply variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::Div for RegisterValue {
    type Output = Result<Self>;

    #[inline]
    fn div(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => {
                if right == 0 {
                    Err(RuntimeError {
                        ty: RuntimeErrorTy::DivideByZero,
                        message: "Cannot divide by zero".to_string(),
                    })
                } else {
                    Ok(Self::Int(left / right))
                }
            }

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot divide variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::BitAnd for RegisterValue {
    type Output = Result<Self>;

    #[inline]
    fn bitand(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(left & right)),
            (Self::Bool(left), Self::Bool(right)) => Ok(Self::Bool(left & right)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot bitwise and variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::BitOr for RegisterValue {
    type Output = Result<Self>;

    #[inline]
    fn bitor(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(left | right)),
            (Self::Bool(left), Self::Bool(right)) => Ok(Self::Bool(left | right)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot bitwise or variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::BitXor for RegisterValue {
    type Output = Result<Self>;

    #[inline]
    fn bitxor(self, other: Self) -> Self::Output {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Ok(Self::Int(left ^ right)),
            (Self::Bool(left), Self::Bool(right)) => Ok(Self::Bool(left ^ right)),

            (left, right) => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!(
                    "Cannot bitwise xor variables of types {} and {}",
                    left.ty(),
                    right.ty()
                ),
            }),
        }
    }
}

impl ops::Not for RegisterValue {
    type Output = Result<Self>;

    #[inline]
    fn not(self) -> Self::Output {
        match self {
            Self::Int(int) => Ok(Self::Int(!int)),
            Self::Bool(boolean) => Ok(Self::Bool(!boolean)),

            val => Err(RuntimeError {
                ty: RuntimeErrorTy::IncompatibleTypes,
                message: format!("Cannot apply not to variable of type {}", val.ty()),
            }),
        }
    }
}

impl std::cmp::PartialOrd for RegisterValue {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => Some(left.cmp(right)),
            (Self::Bool(left), Self::Bool(right)) => Some(left.cmp(right)),
            (Self::String(left), Self::String(right)) => Some(left.cmp(right)),
            (Self::Pointer(left), Self::Pointer(right)) => Some(left.cmp(right)),

            (_, _) => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CachedValue {
    String(GcStr),
    Int(AllocId),
    Bool(AllocId),
    Pointer(AllocId),
}

impl std::fmt::Display for CachedValue {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(string) => write!(fmt, "{:?}", string),
            Self::Int(alloc) | Self::Bool(alloc) | Self::Pointer(alloc) => {
                write!(fmt, "{:#p}", alloc)
            }
        }
    }
}

impl CachedValue {
    // TODO: Make this compacting
    pub fn allocate_id(val: Value, id: impl Into<AllocId>, gc: &mut Gc) -> Result<Self> {
        let id = id.into();

        Ok(match val {
            Value::Bool(b) => Self::Bool(Self::alloc(id, gc, b, std::mem::size_of::<bool>())?),
            Value::String(s) => Self::String({ GcStr::new_id(s, id, gc)? }),
            Value::Int(i) => Self::Int(Self::alloc(id, gc, i, std::mem::size_of::<i32>())?),
            Value::Pointer(p) => {
                Self::Pointer(Self::alloc(id, gc, p, std::mem::size_of::<usize>())?)
            }
            _ => unreachable!("Shouldn't be allocating a null space?"),
        })
    }

    fn alloc<T>(
        id: impl Into<AllocId> + Copy,
        gc: &mut Gc,
        val: T,
        size: usize,
    ) -> Result<AllocId> {
        let id = id.into();

        let (alloc_val, alloc_id) = gc.allocate_id(size, id)?;
        assert_eq!(id, alloc_id);

        unsafe {
            gc.write(id, val, Some(&alloc_val))?;
        }

        gc.add_root(alloc_val);

        Ok(id)
    }

    pub fn get_val(&self, gc: &Gc) -> Result<RegisterValue> {
        Ok(match self {
            Self::String(l) => RegisterValue::String(unsafe { std::mem::transmute(l.to_str(gc)?) }),
            Self::Int(l) => RegisterValue::Int(gc.fetch::<i32, AllocId>(*l)?),
            Self::Pointer(l) => RegisterValue::Pointer(gc.fetch::<usize, AllocId>(*l)?),
            Self::Bool(l) => RegisterValue::Bool(gc.fetch::<bool, AllocId>(*l)?),
        })
    }

    pub fn eq(&self, other: &Self, gc: &Gc) -> Result<bool> {
        Ok(match (self, other) {
            (Self::String(l), Self::String(r)) => l.to_str(gc)? == r.to_str(gc)?,
            (Self::Int(l), Self::Int(r)) => {
                gc.fetch::<i32, AllocId>(*l)? == gc.fetch::<i32, AllocId>(*r)?
            }
            (Self::Bool(l), Self::Bool(r)) => {
                gc.fetch::<bool, AllocId>(*l)? == gc.fetch::<bool, AllocId>(*r)?
            }
            (Self::Pointer(l), Self::Pointer(r)) => {
                gc.fetch::<usize, AllocId>(*l)? == gc.fetch::<usize, AllocId>(*r)?
            }
            (_, _) => false,
        })
    }

    pub fn eq_reg(&self, other: &RegisterValue, gc: &Gc) -> Result<bool> {
        Ok(match (self, other) {
            (Self::String(l), RegisterValue::String(r)) => l.to_str(gc)? == *r,
            (Self::Int(l), RegisterValue::Int(r)) => gc.fetch::<i32, AllocId>(*l)? == *r,
            (Self::Bool(l), RegisterValue::Bool(r)) => gc.fetch::<bool, AllocId>(*l)? == *r,
            (Self::Pointer(l), RegisterValue::Pointer(r)) => gc.fetch::<usize, AllocId>(*l)? == *r,
            (_, _) => false,
        })
    }

    pub fn not(self, gc: &Gc) -> Result<Self> {
        match self {
            Self::Int(id) => {
                let int = gc.fetch::<i32, AllocId>(id)?;
                unsafe {
                    gc.write(id, !int, None)?;
                }
            }
            Self::Bool(id) => {
                let boolean = gc.fetch::<bool, AllocId>(id)?;
                unsafe {
                    gc.write(id, !boolean, None)?;
                }
            }
            Self::Pointer(id) => {
                let ptr = gc.fetch::<i32, AllocId>(id)?;
                unsafe {
                    gc.write(id, !ptr, None)?;
                }
            }

            val => {
                return Err(RuntimeError {
                    ty: RuntimeErrorTy::IncompatibleTypes,
                    message: format!("Cannot apply not to variable of type {}", val.ty()),
                })
            }
        }

        Ok(self)
    }

    pub fn ty(&self) -> &'static str {
        match self {
            Self::Int(_) => "int",
            Self::Bool(_) => "bool",
            Self::String(_) => "str",
            Self::Pointer(_) => "ptr",
        }
    }

    fn cmp(&self, other: &Self, gc: &Gc) -> Result<Option<std::cmp::Ordering>> {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => {
                let left = gc.fetch::<i32, AllocId>(*left)?;
                let right = gc.fetch::<i32, AllocId>(*right)?;

                Ok(Some(left.cmp(&right)))
            }
            (Self::Bool(left), Self::Bool(right)) => {
                let left = gc.fetch::<bool, AllocId>(*left)?;
                let right = gc.fetch::<bool, AllocId>(*right)?;

                Ok(Some(left.cmp(&right)))
            }
            (Self::String(left), Self::String(right)) => {
                let left = left.to_str(gc)?;
                let right = right.to_str(gc)?;

                Ok(Some(left.cmp(&right)))
            }
            (Self::Pointer(left), Self::Pointer(right)) => {
                let left = gc.fetch::<usize, AllocId>(*left)?;
                let right = gc.fetch::<usize, AllocId>(*right)?;

                Ok(Some(left.cmp(&right)))
            }

            (_, _) => Ok(None),
        }
    }
}
