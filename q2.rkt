#lang racket

(provide reflect-through-y-axis)
(provide translate)
(provide scale)
(provide rotate-around-origin)
(provide put-in-standard-position)

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
         #:x-min -3 #:x-max 3
         #:y-min -3 #:y-max 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-point x y)
  (lambda (bit)
    (if (zero? bit) x y)))

(define (x-of point)
  (point 0))

(define (y-of point)
  (point 1))

(define (vertical-line p l)
  (lambda (t) (make-point (p 0) (+ (p 1) (* l t)))))

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