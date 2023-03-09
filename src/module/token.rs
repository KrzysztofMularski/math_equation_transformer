#[derive(Debug, PartialEq)]
pub enum Token {
    NumberLike(String),
    Variable(String),
    Operator(String),
    ParenLeft,
    ParenRight,
    Space
}

impl Clone for Token {
    fn clone(&self) -> Self {
        match self {
            Self::NumberLike(num) => Self::NumberLike(num.clone()),
            Self::Variable(var) => Self::Variable(var.clone()),
            Self::Operator(op) => Self::Operator(op.clone()),
            Self::ParenLeft => Self::ParenLeft,
            Self::ParenRight => Self::ParenRight,
            Self::Space => Self::Space
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CharKind {
    Digit,
    Char,
    Symbol
}
