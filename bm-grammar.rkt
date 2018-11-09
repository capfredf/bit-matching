#lang brag


bin: OPEN arg-expr CLOSE
arg-expr : expr COLON expr arg-expr | expr COLON expr
expr : NUMBER | "(" expr expr expr ")" | op
op : "+"
