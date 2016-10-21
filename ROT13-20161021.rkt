#lang racket
(define (rot-13 str)
  (let* ([determine (λ (t)
                      (cond ((and (char>=? t #\a) (char<=? t #\z)) #T)
                            ((and (char>=? t #\A) (char<=? t #\Z)) #F)
                            (#T '())))]
         [c13By (λ (t y) (+ t (remainder (+ y (- t) 13) 26)))]
         [trans (λ (t) (case (determine t)
                         ((#T) (integer->char (c13By 97 (char->integer t))))
                         ((#F) (integer->char (c13By 65 (char->integer t))))
                         (('()) t)))])
  (list->string (map trans (string->list str)))))
