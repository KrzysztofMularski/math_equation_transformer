use std::fmt;

use crate::*;

#[allow(dead_code)]
pub struct Rule {
    pub name: String,
    pub pattern: Expr,
    pub subs_postfix: Vec<Token>,
    pub substitute: Expr,
    pub reversable: bool
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.pattern, self.substitute)
    }
}

impl Rule {
    
    pub fn new(pattern: &str, substitute: &str, reversable: bool) -> Rule {
        Rule {
            name: pattern.to_string() + substitute,
            pattern: Expr::new(pattern).unwrap(),
            subs_postfix: Expr::parse_to_postfix(substitute).unwrap(),
            substitute: Expr::new(substitute).unwrap(),
            reversable,
        }
    }
    
}