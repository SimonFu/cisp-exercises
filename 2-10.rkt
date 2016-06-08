#lang sicp

(define (mk-interval a b)
  (cons a b))

(define (upper-bound x)
  (cdr x))

(define (lower-bound x)
  (car x))

(define (add-interval x y)
  (mk-interval (+ (lower-bound x) (lower-bound y))
               (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (mk-interval (- (lower-bound x) (upper-bound y))
               (+ (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (mk-interval (min p1 p1 p3 p4)
                 (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (< (car y) 0)
       (not (< (cdr y) 0)))
      (display "error")
      (mul-interval x
                (mk-interval (/ 1.0 (upper-bound y))
                             (/ 1.0 (lower-bound y))))))

(define x (mk-interval 0.9 1.1))
(define y (mk-interval -9 11))
(add-interval x y)
(sub-interval x y)
(mul-interval x y)
(div-interval x y)