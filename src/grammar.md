ranked by priority

#atom

    = INT|FLOAT
    = LPAREN expr RPAREN

# power

    = atom (POW factor)*

# factor

    = (MINUS)* factor
    = power

# term

    = factor ((MUL|DIV) factor)*

# expr

    = term ((PLUS|MINUS) term)*
