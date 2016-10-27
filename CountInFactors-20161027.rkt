#lang racket/base
; http://rosettacode.org/wiki/Count_in_factors
(define (dispNum n)
  (define (factors n c)
    (cond ((<= n c) `(,n))
          ((= (remainder n c) 0) (cons c (factors (/ n c) 2)))
          (else (factors n (+ 1 c)))))
  (define (dispNum_ t)
    (if (null? t) (void)
        (begin (display (string-append " * " (number->string (car t))))
               (dispNum_ (cdr t)))))
  (let ([f (factors n 2)])
    (begin (display (car f))
           (dispNum_ (cdr f)))))
