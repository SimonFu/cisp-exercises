#lang sicp

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (symbol? v1) (symbol? v2) (eq? v1 v2)))

(define (=number? a1 a2)
  (and (number? a1) (number? a2) (= a1 a2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (if (null? (cdr (cdr (cdr s))))
                       (caddr s)
                       (append (list '+) (cdr (cdr s)))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (if (null? (cdr (cdr (cdr p))))
                             (caddr p)
                             (append (list '*) (cdr (cdr p)))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))

(define (exponent x) (caddr x))

(define (make-exponentiation b e)
  (define (exp x n)
    (define (exp_iter val counter)
      (if (= counter 0)
          val
          (exp_iter (* x val) (- counter 1))))
    (exp_iter 1 n))
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (exp b e))
        (else (list '** b e))))

(define (deriv exp var)
  ;(newline)
  ;(display exp)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp) (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else "Unknown expression type -- DERIV" exp)))
(newline)
(deriv '(+ (** x 2) (* x y)) 'x)
(newline)
(display (sum? '(+ 'a 'b 'c)))
(newline)
(display (deriv '(+ x (* x x) x) 'x))
(newline)
(display (augend '(+ x (* x x) x y)))
(newline)
(display (product? '(* 'a 'b 'c)))
(newline)
(display (multiplicand '(* 'a 'b 'c)))

(newline)
(display (deriv '(* x (* x y) x) 'x))

