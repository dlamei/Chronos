factor = INT|FLOAT

prod = factor ((MUL|DIV) factor)\*

expr = prod ((PLUS|MINUS) prod)\*
