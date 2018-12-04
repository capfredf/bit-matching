#lang racket

(require rackunit
         racket/pretty
         (for-syntax syntax/parse
                     racket/pretty
                     racket/list))

(provide bit-match ->bytes bin)

(define (bindings-match bvs ids)
  (let/cc k
    (let ([Mls (for/list ([bv bvs]
                          [bv-idx (length bvs)])
                 (let ([v (hash-ref ids bv-idx #f)])
                   (if v (if (= (cdr v) bv) v (k #f))
                       (cons 'IDENT bv))))])
      (map cdr (filter (lambda (p)
                         (eqv? (car p) 'IDENT))
                       Mls)))))


(define-for-syntax (bound-vars/acc stx)
  (syntax-parse stx
    [(n e:id) (if (identifier-binding #'e)
               #`(list (cons n (cons 'IDENT e)))
               #`empty)]
    [(n e:number) #`(list  (cons n (cons 'NUM e)))]
    [(n e1:number e2 ...)  #`(cons (cons n (cons 'NUM e1))
                                   #,(bound-vars/acc #`(#,(add1 (syntax->datum #'n)) e2 ...)))]
    [(n e1:id e2 ...) (if (identifier-binding #'e1)
                       #`(cons (cons n (cons 'IDENT e1))
                               #,(bound-vars/acc #`(#,(add1 (syntax->datum #'n)) e2 ...)))
                       #`#,(bound-vars/acc #`(#,(add1 (syntax->datum #'n)) e2 ...)))]))




(begin-for-syntax
  (define-syntax-class pat
    #:attributes ((vars 1) (test 0))
    #:literals (quote cons)
    [pattern (bin (id n) ...)
             #:with test (begin
                           (define triples (bound-vars/acc #'(0 id ...)))

                           #`(λ (mch)
                               (let ([Mextract (extract mch (list n ...))])
                                 (and Mextract (bindings-match Mextract
                                                               (make-hash #,triples))))))
             #:with (vars ...) #`#,(filter (compose not number? syntax->datum)
                                           (syntax->list #'(id ...)))]
    ))


(define-syntax (bit-match stx)
  (syntax-parse stx
    [(_ e) #'(raise-syntax-error 'bit-match "no matching pattern for:" #'e)]
    [(_ e [p:pat rhs] more ...)
     (with-syntax ([rhs^ (if (empty? '(p.vars ...))
                             #'(λ (_) rhs)
                             #'(λ (result)
                                 (let-values
                                     ([(p.vars ...)
                                       (apply values result)])
                                   rhs)))])
       #'(let ([mch e])
           (cond
             [(p.test mch) => rhs^]
             [else (bit-match e more ...)])))]))


(define (extract bv lens)
  ;; TODO check if the sum of lens are equal to 8 * size of bv
  (let loop ([not-consumed 8]
             [curr-val #f]
             [idx 0]
             [curr-res 0]
             [acc '()]
             [lens lens])
    (cond
      ((and (empty? lens) (= not-consumed 8)) (reverse acc))
      ((or (empty? lens) (>= idx (bytes-length bv))) #f)
      (else (let* ([curr-len      (car lens)]
                   [curr-val      (or curr-val (bytes-ref bv idx))]
                   [consumed      (if (< not-consumed curr-len) not-consumed curr-len)]
                   [move          (- not-consumed consumed)]
                   [mask          (arithmetic-shift (sub1 (expt 2 consumed)) move)]
                   [res           (bitwise-ior (arithmetic-shift curr-res curr-len)
                                               (arithmetic-shift (bitwise-and curr-val mask) (- move)))]
                   [not-consumed^ (- not-consumed consumed)])
              (let-values ([(res^ acc^ lens^)
                            (if (equal? consumed curr-len)
                                (values 0
                                        (cons res acc)
                                        (cdr lens))
                                (values res
                                        acc
                                        (cons (- curr-len consumed) (cdr lens))))])

                (if (zero? not-consumed^)
                    (loop 8 #f (add1 idx) res^ acc^
                          lens^)
                    (loop not-consumed^ (bitwise-and (bitwise-not mask) curr-val)
                          idx res^ acc^
                          lens^))))))))


#;
(define y 1)
#;
(bit-match (bytes 16)
           ((bin (y 3) (z 5)) #;y
                                (+ y z)))


(define ->byte (λ (xs)
                   (apply bytes
                          (for/fold ([s 0]
                                     [acc '()]
                                     [not-consumed 8]
                                     #:result (reverse acc))
                                    ([i xs])
                            (define move (if (< (cdr i) not-consumed)
                                             (- not-consumed (cdr i))
                                             not-consumed))
                            (let ([s^ (bitwise-and 255 (bitwise-ior (arithmetic-shift (car i) move) s))])
                              (values (if (zero? move) 0 s^)
                                      (if (zero? move) (cons s^ acc) acc)
                                      (if (zero? move) 8 move)))))))


(define (->bytes xs)
  (apply bytes
         (let loop ([s 0]
                    [acc '()]
                    [not-consumed 8]
                    [xs xs])
           (if (empty? xs) (reverse acc)
               (let ([i (car xs)])


                 (define consumed (if (< (cdr i) not-consumed)
                                      (cdr i)
                                      not-consumed))

                 (define move
                   (- not-consumed consumed))

                 (define mask (sub1 (expt 2 (- (cdr i) consumed))))
                 (define rem-length (- (cdr i) consumed))
                 (define mask1 (arithmetic-shift (sub1 (expt 2 consumed)) rem-length))
                 (define val (arithmetic-shift (bitwise-and (car i) mask1) (- rem-length)))
                 (let ([s^ (bitwise-and 255 (bitwise-ior (arithmetic-shift val
                                                                           move) s))])
                   (loop (if (zero? move) 0 s^)
                         (if (zero? move) (cons s^ acc) acc)
                         (if (zero? move) 8 move)
                         (if (= (cdr i) consumed)
                             (cdr xs)
                             (cons (cons (bitwise-and (car i) mask) rem-length)
                                   (cdr xs))))))))))



(define (zip xs ys)
  (map cons xs ys))



#;
(define-syntax (bin stx)
  (syntax-parse stx
    [(_ (l r) ...) #'(->bytes (list (cons l r) ...))]))

(define-syntax-rule (bin (l r) ...)
  (->bytes (list (cons l r) ...)))

;; (define x 10)
;; (define y 10)
;; (bin (x 4) (y 4))

#;
(bin (x . 4) (y . 4))

(module+ test

  (check-equal? (bit-match (->bytes '((16 . 8)))
                           ((bin (0 3) (16 5)) "dogs"))
                "dogs")
  (check-equal? (bit-match (bytes 16)
                           ((bin (1 3) (1 5)) "dogs")
                           ((bin (0 3) (16 5)) "cats"))
                "cats")
  (check-equal? (->bytes (zip '(0 4 20) '(3 5 8))) (bytes 4 20))
  (check-equal? (->bytes (zip '(0 32 12) '(4 8 4))) (bytes 2 12))
  ;(check-equal? (->bytes (zip '(0 32 12) '(4 8 4))) (bytes 2 12))
  (check-equal? (->bytes '((0 . 3) (16 . 5))) (bytes 16))
  (check-equal? (->bytes (zip '(0 1 16) '(4 4 8))) (bytes 1 16))
  (check-equal? (->bytes (zip '(0 34 0) '(4 9 3))) (bytes 1 16))

  (check-equal? (extract (bytes 16) '(3 5)) '(0 16))
  (check-equal? (extract (bytes 16) '(4 4)) '(1 0))
  (check-equal? (extract (bytes 17) '(4 4)) '(1 1))
  (check-equal? (extract (bytes 1 16) '(4 4 8)) '(0 1 16))

  ;; (bytes 4 16) ==> 0000 0001 0001 0000
  ;; 4 9 3 ==> 0000 0001000010 000
  ;; 4 4 5 3 ==> 0000 0001 000010 000
  (check-equal? (extract (bytes 1 16) '(4 9 3)) '(0 34 0))
  (check-equal? (extract (bytes 1 16) '(4 4 5 3)) '(0 1 2 0))
  (check-equal? (bit-match (bytes 16)
                           [(bin (y 4)) y]
                           [(bin (y 3) (z 5)) (+ y z)])
                16)
  (check-equal? (bit-match (bytes 1 16)
                           [(bin (y 4)) y]
                           [(bin (y 4) (x 4) (z 8)) (+ y x z)])
                17))
;; 9
;; len 1
;; len 8
#;
(extract (bytes 1 16) '(4 4 8)) ;; 0000 0001 00010000 => 0 1 16
;; 00010000 00000001
;; 0001 000 000000001
;; 0001 => not-consumed 4 ; acc '(1) ; idx 0
;; 000 => not-consumed 1 ; acc '(0 1) ; idx 0
;; 0  => not-consumed 0 ; acc '(0 0 1) ; idx 1
;; 0000000 => not-consumed 0 ; acc '(1 0 1) ; idx 2

#;
(let-values ([(x y z) (extract (bytes 16 1) 4 3 9)])
  (begin
    (check-equal? x 1)
    (check-equal? y 0)))

;(define-values (x y z) (10 10 10))
;
;(bit-match (bin x 4 y 2 z 20)
;           [(bin e1 n1 e2 n2 e3 n3) (+ e1 e2 e3)])
#;
(bit-match (bytes 16)
           ((bin (x 4) (y 4)) (+ x y))) ; ==> 1


;; (bit-match (bytes 40 10)
;;            ((bin (x 8) (y 8)) (+ x y))) ;; ==> 50





#;
(bit-match '(bin (40 10) (200 60))
  [(bin (y n)) y]
  [(bin (y n) (z m)) (+ y z)])

;; (check-equal? (bit-match '(bin (40 10))
;;                          [(bin (y n)) y])
;;               40)



;
;(bit-match '(bin 40 10)
;  [(bin x n) n])


;(define z 10)
;
;(bit-match `(bin ,z 10)
;  [(bin x n) (add1 n)])
;
;(bit-match `(bin ,z 10)
;  [(bin x n) x])
;
;(bit-match `(bin ,z 10)
;  [(bin x n) (begi x n)])




#|
#;(define-syntax (vars->hash stx)
  (syntax-parse stx
    [(_ n h e) #'(if (and (symbol? e) (not (identifier? e)))
                   h
                   (hash-set h n #'e))]

    [(_ n h e1 e2 ...)
     #'(if (and (symbol? e1) (not (identifier? e1)))
           (vars->hash (add1 n) h e2 ...)
           (vars->hash (add1 n) (hash-set h n #'e1) e2 ...))]))

(define-for-syntax (hasherize ids)
    (for/hash ([ident ids]
               [i (length ids)])
      #;(pretty-print (list ident i))
      (if (let ([b (identifier-binding ident)])
            (begin #;(printf "id-binding ~v\n" b)
                   b))
          (values i ident)
          (values i #f))))

#;#;(define x 5)
(identifier-binding #'x)|#
