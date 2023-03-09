use std::{fmt, collections::HashMap, cell::RefCell, rc::Rc};

use crate::*;

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
enum NumType {
    Int(i64),
    Float(f64),
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone)]
pub enum FunOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq
}

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum Expr {
    Num(f64),
    Var(String),
    Fun(FunOp, Rc<RefCell<Expr>>, Rc<RefCell<Expr>>),
}

impl Clone for Expr {
    fn clone(&self) -> Self {
        match self {
            Self::Num(num) => Self::Num(num.clone()),
            Self::Var(var) => Self::Var(var.clone()),
            Self::Fun(op, a, b) => Self::Fun(op.clone(), Rc::new(RefCell::new((*a.borrow()).clone())), Rc::new(RefCell::new((*b.borrow()).clone()))),
        }
    }
}

impl fmt::Display for NumType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NumType::Int(int) => write!(f, "{}", int),
            NumType::Float(float) => write!(f, "{}", float),
        }
    }
}

impl fmt::Display for FunOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunOp::Add => write!(f, "+"),
            FunOp::Sub => write!(f, "-"),
            FunOp::Mul => write!(f, "*"),
            FunOp::Div => write!(f, "/"),
            FunOp::Eq => write!(f, "="),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Num(num) => write!(f, "{}", num),
            Expr::Var(x) => write!(f, "{}", x),
            Expr::Fun(op, a, b) => {
                match op {
                    FunOp::Eq => write!(f, "{} {} {}", a.borrow(), op, b.borrow()),
                    _ => write!(f, "({} {} {})", a.borrow(), op, b.borrow())
                }
            }
        }
    }
}

impl Expr {

    pub fn new(expr: &str) -> Result<Expr, String> {
        if expr.is_empty() {
            return Err(format!("Equation is empty."));
        }

        let tokens = Expr::tokenization(expr).unwrap();
        
        let postfix = Expr::infix_to_postfix(tokens).unwrap();

        let expression = Expr::postfix_to_expression(postfix).unwrap();

        Ok(expression)
    }

    pub fn parse_to_postfix(expr: &str) -> Result<Vec<Token>, String> {
        if expr.is_empty() {
            return Err(format!("Equation is empty."));
        }

        let tokens = Expr::tokenization(&expr).unwrap();
        
        let postfix = Expr::infix_to_postfix(tokens).unwrap();

        Ok(postfix)
    }

    pub fn apply(&self, rule: &Rule, mut occurance: i16) -> Result<Expr, String> {

        let new_expr = Rc::new(RefCell::new(self.clone()));

        let rule_root_operator = match &rule.pattern {
            Expr::Fun(op, _, _) => op.clone(),
            _ => return Err(String::from(format!("Rule '{}' should contain at least one operator", &rule.pattern)))
        };
        
        let mut current_expr = Rc::clone(&new_expr);
        let mut stack_rest: Vec<Rc<RefCell<Expr>>> = Vec::new();
        let mut result: (bool, Option<HashMap<String, Expr>>) = (false, None);

        let mut current_road_to_swap_expr = String::new();
        let mut road_to_swap_expr: String = String::new();

        loop {

            match &*Rc::clone(&current_expr).borrow() {
                Expr::Num(_) | Expr::Var(_) => {
                    while current_road_to_swap_expr.ends_with("r") {
                        current_road_to_swap_expr.pop();
                    }
                    current_road_to_swap_expr.pop();
                    current_road_to_swap_expr += "r";
                    
                    current_expr = match stack_rest.pop() {
                        Some(expr) => expr,
                        None => break,
                    }
                },
                Expr::Fun(op, a, b) => {
                    if *op == rule_root_operator {
                        let (
                            is_matched,
                            bindings
                        ) = Expr::compare_to_pattern(
                            Rc::clone(&current_expr),
                            Rc::new(RefCell::new(rule.pattern.clone()))
                        ).unwrap();
                        if is_matched {
                            if occurance == 0 {
                                road_to_swap_expr = current_road_to_swap_expr.clone();
                                result = (
                                    true,
                                    match bindings {
                                        Some(hm) => {
                                            if hm.len() == 0 {
                                                return Err(format!("No bindings found, probably wrong rule pattern: {}", rule.pattern));
                                            }
                                            Some(hm.iter().map( | (str, expr) | (str.clone(), (*expr.borrow()).clone())).collect())
                                        },
                                        _ => unreachable!("Wrong rule pattern: {}", rule.pattern),
                                    }
                                );
                            }
                            occurance -= 1;
                        }
                    }
                    current_expr = Rc::clone(&a);
                    stack_rest.push(Rc::clone(&b));
                    current_road_to_swap_expr += "l";
                },
            }
        }

        let (is_matched, bindings) = result;
    
        if !is_matched {
            return Err(format!("Cannot match rule to expression"));
        }

        Expr::apply_bindings(Rc::clone(&new_expr), &rule, bindings.unwrap(), &mut road_to_swap_expr).unwrap();

        return Ok((*new_expr.borrow()).clone());

    }

    pub fn reduce_one(&self) -> Result<Expr, String> {
        let new_expr = Rc::new(RefCell::new(self.clone()));
        let mut current_expr = Rc::clone(&new_expr);
        let mut stack_rest: Vec<Rc<RefCell<Expr>>> = Vec::new();
        let mut calc_result: Option<Expr> = None;
        
        loop {
            match &*Rc::clone(&current_expr).borrow() {
                Expr::Num(_) | Expr::Var(_) => {
                    current_expr = match stack_rest.pop() {
                        Some(expr) => expr,
                        None => break,
                    }
                },
                Expr::Fun(op, a, b) => {
                    if let Expr::Num(num1) = *a.borrow() {
                        if let Expr::Num(num2) = *b.borrow() {
                            calc_result = Some(Expr::calculate_expr(op, &num1, &num2).unwrap());
                        }
                    }
                    if let None = calc_result {
                        current_expr = Rc::clone(&a);
                        stack_rest.push(Rc::clone(&b));
                        continue;
                    }
                },
            }
            
            match &calc_result {
                Some(cr) => *current_expr.borrow_mut() = cr.clone(),
                None => continue
            }
            calc_result = None;
        }

        return Ok((*new_expr.borrow()).clone());

    }

    fn calculate_expr(op: &FunOp, num1: &f64, num2: &f64) -> Result<Expr, String> {

        Ok(Expr::Num(match op {
            FunOp::Add => num1 + num2,
            FunOp::Sub => num1 - num2,
            FunOp::Mul => num1 * num2,
            FunOp::Div => num1 / num2,
            FunOp::Eq => unreachable!("Shouldn't be used with equals operator"),
        }))

    }

    fn compare_to_pattern(expr: Rc<RefCell<Expr>>, pattern_expr: Rc<RefCell<Expr>>) -> Result<(bool, Option<HashMap<String, Rc<RefCell<Expr>>>>), String> {
        
        let mut current_expr = expr;
        let mut current_pattern_expr = pattern_expr;
        let mut stack_pattern_rest: Vec<Rc<RefCell<Expr>>> = Vec::new();
        let mut stack_rest: Vec<Rc<RefCell<Expr>>> = Vec::new();
        let mut bindings: HashMap<String, Rc<RefCell<Expr>>> = HashMap::new();

        loop {
            match &*Rc::clone(&current_pattern_expr).borrow() {
                Expr::Var(var) => {
                    bindings.insert(var.clone(), Rc::clone(&current_expr));
                    current_pattern_expr = match stack_pattern_rest.pop() {
                        Some(expr) => expr,
                        None => break,
                    };
                    current_expr = match stack_rest.pop() {
                        Some(expr) => expr,
                        None => break,
                    };
                },
                Expr::Num(num) => {
                    bindings.insert(num.clone().to_string(), Rc::clone(&current_expr));
                    current_pattern_expr = match stack_pattern_rest.pop() {
                        Some(expr) => expr,
                        None => break,
                    };
                    current_expr = match stack_rest.pop() {
                        Some(expr) => expr,
                        None => break,
                    };
                },
                Expr::Fun(op, a, b) => {
                    match &*Rc::clone(&current_expr).borrow() {
                        Expr::Fun(opp, aa, bb) => {
                            if op != opp {
                                return Ok((false, None));
                            }
                            current_pattern_expr = Rc::clone(&a);
                            current_expr = Rc::clone(&aa);

                            stack_pattern_rest.push(Rc::clone(&b));
                            stack_rest.push(Rc::clone(&bb));
                        },
                        _ => {
                            break;
                        }
                    }
                },
            }
        }

        Ok((true, Some(bindings)))

    }

    fn apply_bindings(expr: Rc<RefCell<Expr>>, rule: &Rule, bindings: HashMap<String, Expr>, expr_path: &mut String) -> Result<(), String> {

        // traversing through expression
        let mut it = expr_path.chars();
        let mut current_expr = Rc::clone(&expr);
        loop {

            match it.next() {
                Some(ch) => {
                    if let Expr::Fun(_, a, b) = &*Rc::clone(&current_expr).borrow() {
                        if ch == 'l' {
                            current_expr = Rc::clone(&a);
                        } else if ch == 'r' {
                            current_expr = Rc::clone(&b);
                        }
                    }
                },
                None => break,
            };
        }

        // creating substitute with applied bindings 
        let mut it = rule.subs_postfix.iter();
        let mut stack: Vec<Expr> = Vec::new();
        loop {
            let current_token = match it.next() {
                Some(token) => token,
                None => break
            };
            let expression = match current_token {
                Token::NumberLike(num) => {
                    let number = match num.parse::<f64>() {
                        Ok(float) => float,
                        Err(_) => return Err(format!("Cannot parse token '{}' to a float number", num)),
                    };
                    Expr::Num(number)
                },
                Token::Variable(var) => {
                    (*bindings.get(var).unwrap()).clone()
                },
                Token::Operator(op) => {
                    let last = stack.pop().unwrap();
                    Expr::Fun(
                        match op.as_str() {
                            "+" => FunOp::Add,
                            "-" => FunOp::Sub,
                            "*" => FunOp::Mul,
                            "/" => FunOp::Div,
                            "=" => FunOp::Eq,
                            _ => unreachable!("Unknown operator '{}' should be caught sooner", op)
                        },
                        Rc::new(RefCell::new(stack.pop().unwrap())),
                        Rc::new(RefCell::new(last))
                    )
                },
                _ => unreachable!("Unknown token '{:?}' should be caught sooner", current_token)
            };
            stack.push(expression);
        }

        *current_expr.borrow_mut() = stack.pop().unwrap();

        Ok(())

    }

    fn tokenization(expr: &str) -> Result<Vec<Token>, String> {
        // tokenization
        let mut chars = expr.chars().enumerate();
        let mut tokens: Vec<Token> = Vec::new();

        // first char:
        let current_char = chars.next().unwrap().1;
        let mut current_char_kind = match current_char {
            '0'..='9' | '.' => CharKind::Digit,
            'a'..='z' | 'A'..='Z' | '_' => CharKind::Char,
            '+' | '-' | '*' | '/' | '(' | ')' | '=' | ' ' => CharKind::Symbol,
            _ => return Err(format!("Unknown character in the equation: '{}'", current_char)),
        };

        // rest of characters:
        let mut last_used_i: i32 = -1;

        let mut previous_char_kind;

        fn get_token_kind(string: &str, previous_char_kind: CharKind) -> Token {
            match previous_char_kind {
                CharKind::Digit => Token::NumberLike(string.to_string()),
                CharKind::Char => Token::Variable(string.to_string()),
                CharKind::Symbol => {
                    match string {
                        "+" | "-" | "*" | "/" | "=" => Token::Operator(string.to_string()),
                        "(" => Token::ParenLeft,
                        ")" => Token::ParenRight,
                        " " => Token::Space,
                        _ => unreachable!("Unknown character '{}' should be caught sooner, {:?}", string, previous_char_kind)
                    }
                },
            }
        }

        loop {
            let (i, current_char) = match chars.next() {
                Some(pair) => pair,
                None => {
                    previous_char_kind = current_char_kind;
                    break;
                }
            };
            match current_char {
                '0'..='9' | '.' => {
                    match current_char_kind {
                        CharKind::Digit => continue,
                        _ => {
                            previous_char_kind = current_char_kind;   
                            current_char_kind = CharKind::Digit
                        }
                    }
                },
                'a'..='z' | 'A'..='Z' | '_' => {
                    match current_char_kind {
                        CharKind::Char => continue,
                        _ => {
                            previous_char_kind = current_char_kind;   
                            current_char_kind = CharKind::Char
                        }
                    }
                },
                '+' | '-' | '*' | '/' | '(' | ')' | '=' | ' ' => {
                    previous_char_kind = current_char_kind;
                    current_char_kind = CharKind::Symbol;
                },
                _ => return Err(format!("Unknown character in the equation: '{}'", current_char)),
            }

            

            tokens.push(
                get_token_kind(
                    &expr[(last_used_i+1) as usize..i],
                    previous_char_kind
                )
            );            
            last_used_i = i as i32 - 1;
            
        }
        tokens.push(
            get_token_kind(
                &expr[(last_used_i+1) as usize..expr.len()],
                previous_char_kind
            )
        );
        Ok(tokens)
    }

    fn infix_to_postfix(tokens: Vec<Token>) -> Result<Vec<Token>, String> {

        let mut stack: Vec<Token> = Vec::new();
        let mut postfix: Vec<Token> = Vec::new();

        let mut iter = tokens.iter();

        fn get_operator_precedence(token: &Token) -> i32 {
            match token {
                Token::Operator(op) => {
                    match op.as_str() {
                        "*" | "/" => 2,
                        "+" | "-" => 1,
                        "=" => 0,
                        _ => unreachable!("Operator '{:?}' should be defined correctly at this point", op),
                    }
                },
                Token::ParenLeft | Token::ParenRight => 3,
                _ => unreachable!("Invoked on token '{:?}' while should be only invoked on operators and parentheses", token)
            }
        }

        loop {
            let current_token = match iter.next() {
                Some(token) => token,
                None => break,
            };
            match current_token {
                Token::Space => continue,
                Token::NumberLike(_) | Token::Variable(_) => {
                    // 1. If token is an operand, push to postfix vector
                    postfix.push(current_token.clone());
                },
                Token::Operator(_) => {
                    if stack.is_empty() {
                        // 2. If token is an operator and stack is empty, push to stack
                        stack.push(current_token.clone());
                    } else {
                        let current_op_prec = get_operator_precedence(&current_token);
                        let stacks_last_op_prec = get_operator_precedence(stack.last_mut().unwrap());
                        // 3. If token is an operator and stack is not empty...
                        if current_op_prec > stacks_last_op_prec {
                            // 3.1. If current operator precedence is greater than precedence of top most in stack, push it to stack
                            stack.push(current_token.clone());
                        } else if current_op_prec <= stacks_last_op_prec {
                            // 3.2. If current operator precedence is lower than or equal to precedence of top most in stack, pop from stack until it is not
                            while !stack.is_empty() && current_op_prec <= get_operator_precedence(stack.last_mut().unwrap()) {
                                if *stack.last().unwrap() == Token::ParenLeft {
                                    break;
                                }
                                postfix.push(stack.pop().unwrap());
                            }
                            stack.push(current_token.clone());
                        }
                    }
                },
                Token::ParenLeft => {
                    // 4. If current operator is opening bracket, push it to stack
                    stack.push(current_token.clone());
                },
                Token::ParenRight => {
                    // 5. If current operator is closing bracket, pop out operators from stack until closest opening bracket is removed
                    while !stack.is_empty() && *stack.last().unwrap() != Token::ParenLeft {
                        postfix.push(stack.pop().unwrap());
                    }
                    // poping left paren from stack
                    stack.pop();
                }
            };
        }

        // 6. Pop everything from stack to postfix
        while !stack.is_empty() {
            postfix.push(stack.pop().unwrap());
        }
        
        Ok(postfix)

    }

    fn postfix_to_expression(postfix: Vec<Token>) -> Result<Expr, String> {
        
        let mut it = postfix.iter();
        let mut stack: Vec<Expr> = Vec::new();
        loop {
            let current_token = match it.next() {
                Some(token) => token,
                None => break
            };
            let expression = match current_token {
                Token::NumberLike(num) => {
                    let number = match num.parse::<f64>() {
                        Ok(float) => float,
                        Err(_) => return Err(format!("Cannot parse token '{}' to a float number", num)),
                    };
                    Expr::Num(number)
                },
                Token::Variable(var) => Expr::Var(var.to_string()),
                Token::Operator(op) => {
                    let last = stack.pop().unwrap();
                    Expr::Fun(
                        match op.as_str() {
                            "+" => FunOp::Add,
                            "-" => FunOp::Sub,
                            "*" => FunOp::Mul,
                            "/" => FunOp::Div,
                            "=" => FunOp::Eq,
                            _ => unreachable!("Unknown operator '{}' should be caught sooner", op)
                        },
                        Rc::new(RefCell::new(stack.pop().unwrap())),
                        Rc::new(RefCell::new(last))
                    )
                },
                _ => unreachable!("Unknown token '{:?}' should be caught sooner", current_token)
            };
            stack.push(expression);
        }

        Ok(stack.pop().unwrap())

    }

}