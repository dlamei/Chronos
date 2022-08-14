# Backus-Naur form:

---

[**BNF Playground**](<https://bnfplayground.pauliankline.com/?bnf=%3Cvar%3E%20%3A%3A%3D%20(%5Ba-z%5D%20%7C%20%5BA-Z%5D)%20(%5Ba-z%5D%20%7C%20%5BA-Z%5D%20%7C%20%5B0-9%5D)*%0A%3Cint%3E%20%3A%3A%3D%20%220%22%20%7C%20(%5B1-9%5D%20%5B0-9%5D*)%0A%3Cfloat%3E%20%3A%3A%3D%20%3Cint%3E%3F%20%22.%22%20%3Cint%3E%0A%3Cstring%3E%20%3A%3A%3D%20%22%5C%22%22%20(%5Ba-z%5D%20%7C%20%5BA-Z%5D%20%7C%20%5B0-9%5D%20%7C%20%22%20%22%20)*%20%22%5C%22%22%0A%0A%3Catom%3E%20%3A%3A%3D%20%3Cint%3E%20%7C%20%3Cfloat%3E%20%7C%20%3Cstring%3E%0A%0A%3Cexpr%3E%20%3A%3A%3D%20%3Catom%3E%20%7C%20%22%7B%7D%22%20%7C%20(%22%7B%22%20(%3Cexpr%3E%20%22%3B%22)*%20%3Cexpr%3E%3F%20%22%7D%22)%0A%0A%3Csymbol%3E%20%3A%3A%3D%20%22%2B%22%20%7C%20%22-%22%20%7C%20%22*%22%20%7C%20%22%2F%22%0A%0A%3Cbinop%3E%20%3A%3A%3D%20%3Cexpr%3E%20%3Csymbol%3E%20%3Cexpr%3E%0A%3Cunryop%3E%20%3A%3A%3D%20%3Csymbol%3E%20%3Cexpr%3E&name=>)

predefined in lexer: \<int>, \<float>, \<string>, \<var>, \<symbol>

```
<atom> ::= <int> | <float> | <string> | <var> | <expr>
<expr_body> ::= "{}" | ("{" (<expr> ";")* <expr>? "}")
<expr_desc> ::= "()" | ("(" (<expr_to_assign> ",")* <expr_to_assign> ")")
<expr_eval> ::= "()" | ("(" (<expr> ",")* <expr> ")")

<expr_to_assign> ::= "const"? <expr> | (<expr> ":" <expr>)

<expr_def> ::= <expr_desc>? <expr_body>

<binop> ::= <expr> <symbol> <expr>
<pre_unryop> ::= <symbol> <expr>
<post_unryop> ::= <expr> <symbol>
<eval> ::= <expr> <expr_eval>
<assign> ::= <expr_to_assign> "=" <expr>
<access> ::= <expr> "." <expr>

<op> ::= <binop> | <pre_unryop> | <post_unryop> | <eval> | <assign> | <access>

<expr> ::= <op> | <atom> | <expr_def>
```
