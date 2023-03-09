mod module;

#[allow(unused_imports)]
use module::{
    rule::Rule,
    expr::{
        Expr,
        FunOp,
    },
    token::{
        Token,
        CharKind
    }
};

#[allow(unused_variables)]
fn main() {

    // let equation = Expr::new("2*x + 1 = 5").unwrap();
    // println!("{}", equation);

    // let rule = Rule::new("A + B", "B + A", true);
    // println!("{}", rule);

    // let matches = Rule::apply(equation, &rule, 0).unwrap();
    // println!("{:?}", matches);
    
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;

    use crate::Expr::{self, *};
    use crate::FunOp::*;
    use crate::Rule;

    #[test]
    fn test_parse() {
        assert_eq!(
            Expr::new("2*x + 1 = 5").unwrap(),
            Fun(
                Eq,
                Rc::new(RefCell::new(Fun(
                    Add,
                    Rc::new(RefCell::new(
                        Fun(
                            Mul,
                            Rc::new(RefCell::new(Num(2.))),
                            Rc::new(RefCell::new(Var(String::from("x"))))
                        )
                    )),
                    Rc::new(RefCell::new(Num(1.)))
                ))),
                Rc::new(RefCell::new(Num(5.)))
            )
        );
    }

    #[test]
    fn test_parsing() {
        let expr = Expr::new("2*x + 1 = 5").unwrap();
        let rule = Rule::new("A * B", "B * A", true);
        let new_expr = expr.apply(&rule, 0).unwrap();
        assert_eq!(
            new_expr,
            Fun(
                Eq,
                Rc::new(RefCell::new(Fun(
                    Add,
                    Rc::new(RefCell::new(
                        Fun(
                            Mul,
                            Rc::new(RefCell::new(Var(String::from("x")))),
                            Rc::new(RefCell::new(Num(2.)))
                        )
                    )),
                    Rc::new(RefCell::new(Num(1.)))
                ))),
                Rc::new(RefCell::new(Num(5.)))
            )
        )
    }

    #[test]
    fn test_rule() {
        let expr = Expr::new("2*x + 1 = 5").unwrap();
        let rule = Rule::new("A + B = C", "A = C - B", true);
        let new_expr = expr.apply(&rule, 0).unwrap();
        let template = Expr::new("2*x = 5 - 1").unwrap();

        println!("Expr: {}", expr);
        println!("Rule: {}", rule);
        println!("New : {}", new_expr);
        println!("Temp: {}", template);

        assert_eq!(
            new_expr,
            template
        )
    }

    #[test]
    fn test_reduce_expr() {
        let expr = Expr::new("2*x + 1 = 5 + 2 + 1").unwrap();
        let new_expr = expr.reduce_one().unwrap();
        let new_expr2 = new_expr.reduce_one().unwrap();
        let template = Expr::new("2*x + 1 = 8").unwrap();
        
        println!("Expr: {}", expr);
        println!("New : {}", new_expr);
        println!("New : {}", new_expr2);
        println!("Temp: {}", template);

        assert_eq!(
            new_expr2,
            template
        )
    }

    #[test]
    fn test_all() {
        let rule: [Rule; 3] = [
            Rule::new("A + B = C", "A = C - B", true),
            Rule::new("A * B", "B * A", true),
            Rule::new("A * B = C", "A = C / B", true),
        ];
        let mut expr = Expr::new("2*x + 1 = 5").unwrap();
        println!("Expr: {:<20} | {}", format!("{}", expr), rule[0]);
        expr = expr.apply(&rule[0], 0).unwrap();
        println!("Expr: {:<20} | reducing", format!("{}", expr));
        expr = expr.reduce_one().unwrap();
        println!("Expr: {:<20} | {}", format!("{}", expr), rule[1]);
        expr = expr.apply(&rule[1], 0).unwrap();
        println!("Expr: {:<20} | {}", format!("{}", expr), rule[2]);
        expr = expr.apply(&rule[2], 0).unwrap();
        println!("Expr: {:<20} | reducing", format!("{}", expr));
        expr = expr.reduce_one().unwrap();
        println!("Expr: {}", expr);

        let template = Expr::new("x = 2").unwrap();

        assert_eq!(
            expr,
            template
        )
    }

}