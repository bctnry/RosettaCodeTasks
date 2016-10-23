#lang racket
(require racket/gui)
; http://rosettacode.org/wiki/Chaos_game
; http://www.geoastro.de/ChaosSpiel/ChaosEnglish.html
(define a '(299 . 183))
(define b '(199 . 357))
(define c '(399 . 357))
(define (randomPoint)
  (let ([x (random 49 549)] [y (random 49 549)])
    (if (and (< y 357)
             (> y (+ (* -1.74 x) 703.26))
             (> y (+ (* 1.74 x) -337.26)))
        (cons x y)
        (randomPoint))))
(define p (randomPoint))
(define (randomStartingPoint)
  (let ([x (random 1 4)])
    (case x ((1) a) ((2) b) ((3) c))))
(define (midP x y)
  (cons (/ (+ (car x) (car y)) 2) (/ (+ (cdr x) (cdr y)) 2)))
(define mainPaintCallback
  (λ (canvas dc)
    (let ([st (randomStartingPoint)])
      (begin (send dc set-pen "black" 1 'solid)
             (send dc draw-point (car p) (cdr p))
             (set! p (midP p st))))))
(define (startUpCallback canvas dc)
  (begin (send dc set-pen "red" 2 'solid)
         (send dc draw-polygon (list a b c))))
(define frmMain
  (new frame% (label "chaos game")
       (stretchable-width #F) (stretchable-height #F)))
(define canvasMain
  (new (class canvas% (super-new)
         (init paint-callback)
         (define isStartingUp? #T)
         (define lock #T)
         (define startingUpFunc (car paint-callback))
         (define mainFunc (cdr paint-callback))
         (define/override (on-paint)
           (unless lock
             (if isStartingUp?
               (startingUpFunc this (send this get-dc))
               (mainFunc this (send this get-dc)))))
         (define/public (nextStep)
           (begin (set! lock #F)
                  (send this on-paint)
                  (when isStartingUp? (set! isStartingUp? #F))
                  (set! lock #T)))
         (define/public (reset)
           (begin (send (send this get-dc) clear)
                  (set! lock #T)
                  (set! p (randomPoint))
                  (set! isStartingUp? #T)))
         )
       (parent frmMain)
       (min-width 600) (min-height 600)
       (stretchable-width #F) (stretchable-height #F)
       (paint-callback
        (cons startUpCallback mainPaintCallback))
       ))
(define (repeat b e)
  (letrec ([f (λ (t) (if (= t 0) (void)
                         (begin (send canvasMain nextStep) (f (- t 1)))))])
    (f (string->number (send tfTimes get-value)))))
(define hPanel
  (new horizontal-panel%
       (parent frmMain)))
(define btnNext
  (new button%
       (label "Next")
       (parent hPanel)
       (callback (λ (b e) (send canvasMain nextStep)))))
(define btnReset
  (new button%
       (label "Reset")
       (parent hPanel)
       (callback (λ (b e) (send canvasMain reset)))))
(define btnRepeat
  (new button%
       (label "Repeat")
       (parent hPanel)
       (callback repeat)))
(define tfTimes
  (new text-field%
       (parent hPanel) (label "")))
(define lblTimes
  (new message%
       (parent hPanel)
       (label "times")))
(send frmMain show #T)
