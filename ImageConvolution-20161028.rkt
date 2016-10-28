#lang racket
(require racket/draw)

(define (pixelPlus2 p1 p2)
  (list (let ([a1 (car p1)] [a2 (car p2)])
          (if (> a1 a2) a1 a2))
        (+ (cadr p1) (cadr p2))
        (+ (caddr p1) (caddr p2))
        (+ (cadddr p1) (cadddr p2))))
(define (pixelPlus t)
  (define (pixelPlus_ t hold)
    (cond ((null? t) hold)
          (else (pixelPlus_ (cdr t) (pixelPlus2 (car t) hold)))))
  (pixelPlus_ t (list 0 0 0 0)))
(define (pixelScalarMult s p)
  (list (first p) (* s (second p)) (* s (third p)) (* s (fourth p))))

(define (3x3window bitmap x y)
  (for/list ([i (list 0 1 2 0 1 2 0 1 2)] [j (list 0 0 0 1 1 1 2 2 2)])
    (let ([pixelBuffer (bytes 0 0 0 0)])
      (begin (send bitmap get-argb-pixels (+ x i -1) (+ y j -1) 1 1 pixelBuffer)
             ; todo: find a better way to change this.
             (bytes->list pixelBuffer)))))

(define (P bitmap 3x3matrix x y)
  ; where 3x3matrix is a list consisting of 9 numbers.
  (define (safing t)
    (map (λ (t) (exact-round (cond ((< t 0) 0) ((> t 255) 255) (else t)))) t))
  (let ([pixels (3x3window bitmap x y)])
    (list->bytes (safing
                  (pixelPlus (for/list ([p pixels] [k 3x3matrix]) (pixelScalarMult k p)))))))

; todo: solving the edge case.
(define (convBy matrix)
  ; where matrix is a list consisting of 9 numbers
  (λ (bitmap)
    (let* ([w (send bitmap get-width)] [h (send bitmap get-height)]
           [res (make-object bitmap% w h)])
      (begin
        (for ([x (range 1 (- w 1))]) (for ([y (range 1 (- h 1))])
          (let ([rRes (P bitmap matrix x y)])
            (send res set-argb-pixels x y 1 1 rRes))))
        res))))

; use it like this:
;     (define m '(-1 -1 -1 -1 9 -1 -1 -1 -1))
; then:
;     ((convBy m) (make-object bitmap% "A.bmp"))
