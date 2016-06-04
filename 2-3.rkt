#lang sicp

(define (mk-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (mk-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (mid-segment s)
  (let ((start-point (start-segment s))
        (end-point (end-segment s)))
    (mk-point (/ (+ (x-point start-point)
                    (x-point end-point))
                 2)
              (/ (+ (y-point start-point)
                    (y-point end-point))
                 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;(define (mk-rectange pt-start pt-end)
;  (cons pt-start pt-end))

;(define (rectange-width r)
;  (let ((start-x (x-point (car r)))
;        (end-x (x-point (cdr r))))
;    (- end-x start-x)))

;(define (rectange-height r)
;  (let ((start-y (y-point (car r)))
;        (end-y (y-point (cdr r))))
;    (- end-y start-y)))

(define (mk-rectange pt-start w h)
  (cons pt-start (cons w h)))

(define (rectange-width r)
  (car (cdr r)))

(define (rectange-height r)
  (cdr (cdr r)))

(define (rectangle-circumference r)
  (* 2 (+ (rectange-width r) (rectange-height r))))

(define (rectangle-area r)
  (* (rectange-width r) (rectange-height r)))