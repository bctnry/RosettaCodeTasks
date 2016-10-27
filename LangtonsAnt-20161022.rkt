#lang racket
(require racket/gui)

(define map (make-vector 10000 0))
(define ant '(49 49 RIGHT))
(define (left t)
  (case t ((RIGHT) 'UP) ((UP) 'LEFT) ((LEFT) 'DOWN) ((DOWN) 'RIGHT)))
(define (right t)
  (case t ((RIGHT) 'DOWN) ((UP) 'RIGHT) ((LEFT) 'UP) ((DOWN) 'LEFT)))
(define (willOutOfBound?)
  (let ([x (first ant)] [y (second ant)] [d (third ant)])
    (case d
      ((RIGHT) (> (+ x 1) 99))
      ((LEFT) (< (- x 1) 0))
      ((UP) (< (- y 1) 0))
      ((DOWN) (> (+ y 1) 99)))))
(define (stepOut)
  (let ([x (first ant)] [y (second ant)] [d (third ant)])
    (case d
      ((RIGHT) (set! ant (list (+ 1 x) y d)))
      ((LEFT) (set! ant (list (- x 1) y d)))
      ((UP) (set! ant (list x (- y 1) d)))
      ((DOWN) (set! ant (list x (+ 1 y) d))))))
(define (turnLeft)
  (set! ant (list (first ant) (second ant) (left (third ant)))))
(define (turnRight)
  (set! ant (list (first ant) (second ant) (right (third ant)))))
(define (step)
  (let ([pos (+ (second ant) (* 100 (first ant)))])
    (if (= 0 (vector-ref map pos))
        (begin (vector-set! map pos 1)
               (turnRight) (when (not (willOutOfBound?)) (stepOut)))
        (begin (vector-set! map pos 0)
               (turnLeft) (when (not (willOutOfBound?)) (stepOut))))))

(define (mkBitmapWith w h f)
  (let* ([b (make-object bitmap% w h)]
         [dc (send b make-dc)])
    (begin (f b dc) b)))

(define whitePixel (make-object bitmap% 6 6))
(define blackPixel
  (mkBitmapWith 6 6
    (λ (b dc)
      (begin (send dc set-brush "black" 'solid)
             (send dc draw-rectangle 0 0 6 6)))))

(define (drawPixelAt dc x y)
  (let ([f (if (= (vector-ref map (+ y (* x 100))) 0) whitePixel blackPixel)])
    (begin (send dc draw-bitmap f (* 6 x) (* 6 y)))))
(define (drawAnt bitmap dc)
  (let ([rightPL '((0 . 0) (5 . 2) (5 . 3) (0 . 5))]
        [leftPL '((0 . 2) (0 . 3) (5 . 5) (5 . 0))]
        [upPL '((2 . 0) (3 . 0) (0 . 5) (5 . 5))]
        [downPL '((0 . 0) (5 . 0) (3 . 5) (2 . 5))]
        [x (first ant)] [y (second ant)])
    (begin (send dc set-brush "red" 'solid)
           (send dc set-pen "red" 1 'solid)
           (send dc draw-polygon
                 (case (third ant)
                   ((LEFT) leftPL)
                   ((RIGHT) rightPL)
                   ((UP) upPL)
                   ((DOWN) downPL))
                 (* x 6)
                 (* y 6)))))

(define frmMain
  (new frame%
       (label "ant")))
(define canvasMain
  (new (class canvas% (super-new)
         (define interval 1)
         (define/public (performOnInterval f)
           (let ([t (f interval)])
             (set! interval (if (= t 0) interval t))))
         (define/public (retrInterval) interval)
         (define lock #T) (define canvasThread '())
         (define isRunning? #F)
         (define/public (locked?) lock)
         (define (loop)
           (begin (unless lock (send this on-paint))
                  (sleep/yield (/ interval))
                  (loop)))
         (define/public (run)
           (unless isRunning?
             (begin (set! lock #F) (set! isRunning? #T)
                    (set! canvasThread (thread loop)))))
         (define/public (kill)
           (begin (set! lock #T) (set! isRunning? #F)
                  (kill-thread canvasThread))))
       (parent frmMain)
       (min-width 600) (min-height 600)
       (stretchable-width #F) (stretchable-height #F)
       (paint-callback
        (λ (canvas dc)
          (let ([x (first ant)] [y (second ant)])
            (begin (send dc draw-bitmap whitePixel x y)
                   (drawPixelAt dc x y)
                   (step)
                   (drawAnt (void) dc)))))))
(define hPanel
  (new horizontal-panel% (parent frmMain)))
(define btnRun
  (new button% (parent hPanel)
       (label "Run") (callback (λ (b e) (send canvasMain run)))))
(define btnKill
  (new button% (parent hPanel)
       (label "Kill") (callback (λ (b e) (send canvasMain kill)))))
(define btnSpeed+
  (new button% (parent hPanel)
       (label "+")
       (callback (λ (b e) (send canvasMain performOnInterval add1)))))
(define btnSpeed-
  (new button% (parent hPanel)
       (label "-")
       (callback (λ (b e) (send canvasMain performOnInterval sub1)))))
(define btnReset
  (new button% (parent hPanel)
       (label "Reset")
       (callback (λ (b e)
                   (begin (send canvasMain kill)
                          (set! ant '(49 49 RIGHT))
                          (set! map (make-vector 10000 0))
                          (send (send canvasMain get-dc) clear))))))
(send frmMain show #T)
