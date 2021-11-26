factor = INT|FLOAT

term = factor ((MUL|DIV) factor)\*

expr = term ((PLUS|MINUS) term)\*
