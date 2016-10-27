#lang racket
(require racket/gui)

(define penColor (make-object color% 0 177 0))
(define (setPen r g b) (send penColor set r g b))
(define penSize 1)
(define (setPenSize t) (set! penSize t))
(define origin (cons 285 449))
(define (setOrigin t) (set! origin t))
(define scaler (cons 30 30))
(define (setScaler t) (set! scaler t))

(define-syntax-rule (setterOf name x)
  (define/public (name f) (set! x f)))
(define st (list 0 0))
(define (mkTrans a b c d e f)
  (λ (x y) (list (+ e (* a x) (* b y)) (+ f (* c x) (* d y)))))
(define (mkNext f1 f2 f3 f4 pL)
  (λ (x y)
    (let ([p (random)])
      (cond ((<= p (first pL)) (f1 x y))
            ((<= p (+ (first pL) (second pL))) (f2 x y))
            ((<= p (+ (first pL) (second pL) (third pL))) (f3 x y))
            (else (f4 x y))))))

(define f1 (list 0 0 0 0.16 0 0))
(define f2 (list 0.85 0.04 -0.04 0.85 0 1.60))
(define f3 (list 0.20 -0.26 0.23 0.22 0 1.60))
(define f4 (list -0.15 0.28 0.26 0.24 0 0.44))
(define prob (list 0.01 0.85 0.07 0.07))

(define mainCallback
    (λ (canvas dc)
      (begin (set! st (apply (send canvas getFunc) st))
             (send dc draw-point
                   (exact-round (* (car scaler) (first st)))
                   (- (exact-round (* (cdr scaler) (second st)))))))
  )

(define-syntax-rule (valOf/num t) (string->number (send t get-value)))
(define-syntax-rule (dcOf t) (send t get-dc))

(define frmMain
  (new frame% (label "barnsley fern")))
(define canvasMain
  (new (class canvas% (super-new)
         (init f1 f2 f3 f4 probability-distr paint-callback)
         (define f1Args f1) (define f2Args f2) (define f3Args f3) (define f4Args f4)
         (define/public (getF1Args) f1Args) (setterOf setF1Args f1Args)
         (define/public (getF2Args) f2Args) (setterOf setF2Args f2Args)
         (define/public (getF3Args) f3Args) (setterOf setF3Args f3Args)
         (define/public (getF4Args) f4Args) (setterOf setF4Args f4Args)
         (define pDistr probability-distr)
         (define/public (getProbability) pDistr) (setterOf setProbability pDistr)
         (define/public (getFunc)
           (mkNext (apply mkTrans f1Args) (apply mkTrans f2Args)
                   (apply mkTrans f3Args) (apply mkTrans f4Args) pDistr))
         
         (define t '())
         (define lock #T) (define started? #F)
         (define callback paint-callback)
         (define/public (start)
           (letrec ([mainLoop (λ () (send this on-paint) (mainLoop))])
             (begin (set! lock #F)
                    (unless started? (set! t (thread mainLoop)))
                    (set! started? #T))))
         (define/public (stop)
           (begin (kill-thread t) (set! started? #F)))
         (define/public (reset)
           (begin (set! lock #T)
                  (kill-thread t)
                  (send (send this get-dc) clear)
                  (send this start)))
         (define/override (on-paint)
           (unless lock (callback this (send this get-dc))))
         )
       (parent frmMain)
       (min-width 600) (min-height 600)
       (stretchable-width #F) (stretchable-height #F)
       (f1 f1) (f2 f2) (f3 f3) (f4 f4)
       (probability-distr prob)
       (paint-callback mainCallback)))
(define (refreshOrigin) (send (dcOf canvasMain) set-origin (car origin) (cdr origin)))
(define (refreshPen) (send (dcOf canvasMain) set-pen penColor penSize 'solid))

(define hPanel (new horizontal-panel% (parent frmMain)))
(define btnStart
  (new button% (parent hPanel) (label "Start")
       (callback (λ (b e) (send canvasMain start)))))
(define btnStop
  (new button% (parent hPanel) (label "Stop")
       (callback (λ (b e) (send canvasMain stop)))))
(define btnReset11
  (new button% (label "Reset") (parent hPanel)
       (callback (λ (b e) (send canvasMain reset)))))

(define frmSetting
  (new frame% (label "Draw settings")))
(define hPanel2
  (new horizontal-panel% (parent frmSetting)))
(define groupPen
  (new group-box-panel% (parent hPanel2) (label "Pen setting")))
(define tfR
  (new text-field% (label "R") (parent groupPen)
       (init-value (number->string (send penColor red)))))
(define tfG
  (new text-field% (label "G") (parent groupPen)
       (init-value (number->string (send penColor green)))))
(define tfB
  (new text-field% (label "B") (parent groupPen)
       (init-value (number->string (send penColor blue)))))
(define tfPenSize
  (new text-field% (label "pen size") (parent groupPen)
       (init-value (number->string penSize))))

(define groupDisp
  (new group-box-panel%
       (parent hPanel2) (label "Displaying setting")))
(define tfCenterX
  (new text-field% (label "X of center") (parent groupDisp)
       (init-value (number->string (car origin)))))
(define tfCenterY
  (new text-field% (label "Y of center") (parent groupDisp)
       (init-value (number->string (cdr origin)))))
(define tfScaleX
  (new text-field% (label "X of scale") (parent groupDisp)
       (init-value (number->string (car scaler)))))
(define tfScaleY
  (new text-field% (label "Y of scale") (parent groupDisp)
       (init-value (number->string (cdr scaler)))))

(define btnConfigApply
  (new button%
       (label "Apply") (parent frmSetting)
       (callback (λ (b e)
                   (let ([newOriginX (valOf/num tfCenterX)]
                         [newOriginY (valOf/num tfCenterY)]
                         [newScaleX (valOf/num tfScaleX)]
                         [newScaleY (valOf/num tfScaleY)]
                         [newR (valOf/num tfR)] [newG (valOf/num tfG)] [newB (valOf/num tfB)]
                         [newPenSize (valOf/num tfPenSize)]
                         )
                   (begin (setPen newR newG newB) (setPenSize newPenSize) (refreshPen)
                          (set! origin (cons newOriginX newOriginY)) (refreshOrigin)
                          (set! scaler (cons newScaleX newScaleY))
                          
                          ))))))
(define btnSetting
  (new button% (label "Draw settings") (parent hPanel)
       (callback (λ (b e) (send frmSetting show #T)))))

(send (send canvasMain get-dc) set-origin (car origin) (cdr origin))
(send (send canvasMain get-dc) set-pen penColor penSize 'solid)
(send frmMain show #T)
