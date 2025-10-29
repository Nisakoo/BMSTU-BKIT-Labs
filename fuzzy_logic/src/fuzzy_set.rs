use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub struct Set {
    pub value: f64,
}

pub trait FuzzySet {
    type Output;

    fn new(value: f64) -> Self::Output;
    fn and(self, other: Self) -> Self::Output;
    fn or(self, other: Self) -> Self::Output;
    fn not(self) -> Self::Output;
}

impl FuzzySet for Set {
    type Output = Self;

    fn new(value: f64) -> Self::Output {
        Set { value: value }
    }

    fn and(self, other: Self) -> Self::Output {
        Set { value: self.value.min(other.value) }
    }

    fn or(self, other: Self) -> Self::Output {
        Set { value: self.value.max(other.value) }
    }

    fn not(self) -> Self::Output {
        Set { value: 1.0 - self.value }
    }
}

impl Display for Set {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}