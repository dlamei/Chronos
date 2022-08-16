# Backus-Naur form:

---

predefined in lexer: \<int>, \<float>, \<string>, \<var>, \<symbol>

```cpp
<atom> ::= <int> | <float> | <string> | <var> | ("(" <expr> ")") | <expr_def>

<term> ::= (<factor> (("*"|"/") <factor>))
<factor> ::= (("+"|"-") <factor>) | <eval>
<arith> ::= <term> (("+"|"-") <term>)*

<comp> ::= ("!" <comp>) | (<arith> (("=="|"<"|">"|"<="|">") <arith>))*

<logic> ::= <comp> (("&&" | "||") <comp>)*

<expr> ::= <logic>

<eval> ::= <atom> "(" (<expr> ("," <expr>)*)? ")"

<expr_desc> ::= "(" (<expr> ("," <expr>)*)? ")" ("->" <expr>)?
<expr_body> ::= "{" (<expr> (";" <expr>)*)? "}"
<expr_def> ::= <expr_desc>? <expr_body>

<expr_to_assign> ::= "const"? <expr> (":" <expr>)?
<assign> ::= <expr_to_assign> "=" <expr>
```