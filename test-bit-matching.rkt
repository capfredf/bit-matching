#lang bit-matching
(module+ test
  (require rackunit)
  (define x 10)
  (define y 10)
  (check-equal? (bytes 170) <(x 4) (y 4)>)
  (check-equal? (bit-match (bytes 170)
                           (<(x 4) (y 4)> (+ x y)))
                20)
  (check-equal? (bit-match <(11 4)(10 4)>
                           (<(x 4) (y 4)> (+ x y))
                           (<(w 4) (z 4)> "dogs"))
                "dogs"))
