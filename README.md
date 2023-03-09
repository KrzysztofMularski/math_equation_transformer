# Math Equation Transformer

Program for transforming math equations. Supports addition, subtraction, multiplication, division and equal sign.

Example:

```rust
let expr = Expr::new("2*x + 1 = 5").unwrap();
let rule = Rule::new("A + B = C", "A = C - B", true);
let new_expr = expr.apply(&rule, 0).unwrap();

// Expr: ((2 * x) + 1) = 5
// Rule: (A + B) = C -> A = (C - B)
// New : (2 * x) = (5 - 1)
```