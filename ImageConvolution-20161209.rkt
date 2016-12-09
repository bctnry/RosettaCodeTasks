#lang racket
(require racket/draw)

(define (convolute matrix bitmap)
  (define (clip x) (cond ((< x 0) 0) ((> x 255) 255) (#T x))) ; !
  (define (mult x y v) (* (vector-ref (vector-ref matrix y) x) v))
  (define bitmapW (send bitmap get-width))
  (define bitmapH (send bitmap get-height))
  (define matrixW (vector-length (vector-ref matrix 0)))
  (define matrixH (vector-length matrix))
  (define offsetW (quotient matrixW 2))
  (define offsetH (quotient matrixH 2))
  (define res (make-object bitmap% bitmapW bitmapH))
  (for* ([i (range offsetW (- bitmapW offsetW))]
         [j (range offsetH (- bitmapH offsetH))])
    (let ([pixelBuffer (bytes 0 0 0 0)]
          [currentR 0] [currentG 0] [currentB 0]
          [pixelAlpha 0])
      (begin (for* ([y (range 0 matrixH)] [x (range 0 matrixW)])
               (begin (send bitmap get-argb-pixels
                            (- i x (- offsetW)) (- j y (- offsetH)) 1 1 pixelBuffer)
                      (set! currentR (+ currentR (mult x y (bytes-ref pixelBuffer 1))))
                      (set! currentG (+ currentG (mult x y (bytes-ref pixelBuffer 2))))
                      (set! currentB (+ currentB (mult x y (bytes-ref pixelBuffer 3))))
                      (when (and (= x offsetW) (= y offsetH))
                        (set! pixelAlpha (bytes-ref pixelBuffer 0)))))
             (send res set-argb-pixels i j 1 1
                   (bytes pixelAlpha (clip currentR) (clip currentG) (clip currentB))))))
  res)
                          
                      