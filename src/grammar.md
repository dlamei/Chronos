# factor

    = INT|FLOAT
    = (PLUS|MINUS) factor
    = LPAREN expr RPAREN

# term

    = factor ((MUL|DIV) factor)*

# expr

    = term ((PLUS|MINUS) term)*
