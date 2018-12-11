# bit-matching

## How to install the language extension
Install the package via raco or the package manager in DrRacket.
```
raco pkg install https://github.com/capfredf/bit-matching.git
```
and then 
```
#lang bit-matching
```

## How to construct a bit-string
``` Racket
<<e1,...,en>>
```
Each `ei` is a semgment of a bit string. It is a pair of `(expr size)`. `expr` must be evaluated to a number and currently `size` must be a literal numbe and it indicates the number of bits in which the value of expr will be encoded.

```
(define a <<1:4, 1:4>>)
;; a => (bytes 17) or (bytes #b00010001)
```

## How to extract segments of bit-string
``` Racket
(bit-match expr
 (<<seg-pat-1,...,seg-pat-n>> body))
```
The function `bit-match` is similar to match, except that it only works for bit-strings.
Each `seg-pat-i` is a pattern against a semgment of a bit string. It is a pair of `(val-pattern size-pat)`. `val-pattern` can be either identifier or literal number. When it is a bound identifier, the bit-match will use the value to match against the value of `expr`. When it is unbound and the matching succeeds, it will be assigned to the value decoded in the number of bits specified by `size-pat`. 
`size-pat` must be a literal number.

## Future work
see issues
