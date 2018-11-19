#lang brag


bin: OPEN arg-expr (SEP arg-expr)*  CLOSE
arg-expr : (expr COLON expr)
expr : (LPAREN op expr expr RPAREN) | num
op : OP
num : NUM
