#[derive(Debug, Copy, Clone, Eq)]
pub enum Value {
    Int(i32),
    Str(u8),
    None,
}

impl Value {
    pub fn add(self, other: Self, registers: &mut super::Registers) -> Self {
        use Value::*;

        match (self, other) {
            (Int(left), Int(right)) => Int(left + right),
            (Str(left_reg), Str(right_reg)) => {
                let left = &registers.strings[left_reg as usize];
                let right = &registers.strings[right_reg as usize];

                let mut new = String::with_capacity(left.len() + right.len());
                new.push_str(left);
                new.push_str(right);

                registers.strings[left_reg as usize] = new;

                Str(left_reg)
            }
            (None, None) => None,
            (_, _) => unreachable!(),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Value::*;

        let value = match self {
            Int(int) => int.to_string(),
            None => "Empty Register".to_owned(),
            _ => "Invalid Type".to_owned(),
        };

        write!(f, "{}", value)
    }
}

macro_rules! sub {
    ($value:ty) => {
        #[allow(clippy::use_self)]
        impl std::ops::Sub for $value {
            type Output = Value;

            fn sub(self, other: $value) -> Self::Output {
                use Value::*;

                match (self, other) {
                    (Int(left), Int(right)) => Int(left - right),
                    (None, None) => None,
                    (_, _) => unreachable!(),
                }
            }
        }
    };
}

#[allow(clippy::use_self)]
sub!(Value);
sub!(&Value);

impl std::ops::AddAssign for Value {
    fn add_assign(&mut self, other: Self) {
        use Value::*;

        match (self, &other) {
            (Int(left), Int(right)) => *left += right,
            (None, None) => {}
            (_, _) => unreachable!(),
        }
    }
}

impl std::ops::SubAssign for Value {
    fn sub_assign(&mut self, other: Self) {
        use Value::*;

        match (self, &other) {
            (Int(left), Int(right)) => *left -= right,
            (None, None) => {}
            (_, _) => unreachable!(),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;

        match (&self, &other) {
            (Int(left), Int(right)) => left == right,
            (Str(ref left), Str(ref right)) => left == right,
            (None, None) => true,
            (_, _) => false,
        }
    }
}
