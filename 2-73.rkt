#lang sicp

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (symbol? v1) (symbol? v2) (eq? v1 v2)))

(define (=number? a1 a2)
  (and (number? a1) (number? a2) (= a1 a2)))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (deriv-add-package)
  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))
  
  (define (addend s) (cadr s))
  
  (define (augend s) (caddr s))
  
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  
  (define (derive_add x)
    (if (sum? x)
        (make-sum (deriv (addend exp) var)
                  (deriv (augend exp) var))
        (else (display "Eror: " (list x)))))
  
  (put 'deriv '+ derive_add)
  (put 'deriv 'make-sum make-sum))

(define (deriv-product-package)
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
  
  (define (multiplier p) (cadr p))
  
  (define (multiplicand p) (caddr p))
  
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  
  (define (derive_product x)
    (if (product? x)
        (make-sum
         (make-product (deriv (multiplier exp) var)
                       (multiplicand exp))
         (make-product (multiplier exp)
                       (deriv (multiplicand exp) var))))
    (else (display "Eror: " (list x))))
  
  (put 'deriv '* derive_product)
  (put 'deriv 'make-product make-product))

(define (deriv-exponentiation-package)
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
  
  
  (define (derive_exponentiation x)
    (if (product? x)
        (exponentiation? exp)
        (let ((make-product (get 'deriv 'make-product)))
          (make-product (make-product (exponent exp)
                                      (make-exponentiation (base exp) (- (exponent exp) 1)))
                        (deriv (base exp) var))))
    (display "Eror: " (list x)))
  
  (put 'deriv '** derive_exponentiation)
  (put 'deriv 'make-exponentiation make-exponentiation))