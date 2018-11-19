#lang brag

program : (bin | expr)*
bin: OPEN arg-expr (SEP arg-expr)*  CLOSE
arg-expr : expr COLON expr
expr : LPAREN op expr expr RPAREN | num | identifier
op : OP
num : NUM
identifier : ID
