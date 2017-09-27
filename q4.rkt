#lang racket

(provide gosper-step)

(require plot)
(plot-new-window? #t)
(plot-width 900)
(plot-height 900)
(define (draw curve)
  (plot (parametric
         (lambda (t) (vector
                      (x-of (curve t))
                      (y-of (curve t))))
         0 1 #:width 1 #:samples 20000
         #:x-min -0.5 #:x-max 1.5
         #:y-min -0.5 #:y-max 1.5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-point x y)
  (lambda (bit)
    (if (zero? bit) x y)))

(define (x-of point)
  (point 0))

(define (y-of point)
  (point 1))

(define (unit-circle)
(lambda (t)
(make-point (sin (* 2 pi t))
(cos (* 2 pi t)))))

(define (unit-semi-circle)
(lambda (t)
(make-point (sin (* pi (- t (/ 1 2))))
(cos (* pi (- t (/ 1 2)))))))

(define (unit-line-at y)
(lambda (t) (make-point t y)))

(define (unit-line) (unit-line-at 0))

(define (reflect-through-y-axis curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point (- (x-of ct)) (y-of ct)))))

(define (translate x y curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point (+ x (x-of ct)) (+ y (y-of ct))))))

(define (scale x y curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point (* x (x-of ct)) (* y (y-of ct))))))

(define (rotate-around-origin radians curve)
  (lambda (t)
    (let ((ct (curve t))
          (val-cos (cos radians))
          (val-sin (sin radians)))
      (make-point (- (* val-cos (x-of ct)) (* val-sin (y-of ct)))
                  (+ (* val-cos (y-of ct)) (* val-sin (x-of ct)))))))

(define (put-in-standard-position curve)
  (let* [(curve1 (translate (- ((curve 0) 0)) (- ((curve 0) 1)) curve))
         (angle (- (atan (/ ((curve1 1) 1) ((curve1 1) 0)))))
         (curve2 (rotate-around-origin (if (> ((curve1 1) 0) 0) angle
                                           (+ pi angle)) curve1))
         (scale-factor (/ 1 ((curve2 1) 0)))]
    (scale scale-factor scale-factor curve2)))

(define (connect-rigidly curve1 curve2)
  (lambda (t)
    (if (< t 0.5) (curve1 (* 2 t))
        (curve2 (- (* 2 t) 1)))))

(define (connect-ends curve1 curve2)
  (let* [(x (- ((curve1 1) 0) ((curve2 0) 0)))
         (y (- ((curve1 1) 1) ((curve2 0) 1)))]
    (lambda (t)
      (if (< t 0.5) (curve1 (* 2 t))
          ((translate x y curve2) (- (* 2 t) 1))))))

(define (gosper-step curve)
  (let* [(scale-factor (/ 1 (sqrt 2)))
         (angle (/ pi 4))
         (curve-scale (scale scale-factor scale-factor curve))
         (curve1 (rotate-around-origin  angle curve-scale))
         (curve2 (rotate-around-origin (- angle) curve-scale))]
    (connect-ends curve1 curve2)))