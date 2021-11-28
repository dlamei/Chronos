# factor

    = INT|FLOAT
    = LPAREN expr RPAREN
    = (MINUS)* factor

# term

    = factor ((MUL|DIV) factor)*

# expr

    = term ((PLUS|MINUS) term)*
