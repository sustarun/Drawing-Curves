#lang racket

(provide vertical-line)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (vertical-line p l)
  (lambda (t) (make-point (p 0) (+ (p 1) (* l t)))))

(define (make-point x y)
  (lambda (bit)
    (if (zero? bit) x y)))

(define (x-of point)
  (point 0))

(define (y-of point)
  (point 1))