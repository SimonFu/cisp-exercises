#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? seqs)
      nil
      (cons (accumulate op
                        init
                        ((lambda (x)
                           (define (handler x)
                             (if (or (not (pair? x))
                                     (null? (car x)))
                                 nil
                                 (cons (car (car x))
                                       (handler (cdr x)))))
                           (handler x)) seqs))
            (accumulate-n op
                          init
                          ((lambda (x)
                           (define (handler x)
                             (if (or (not (pair? x))
                                     (null? (car x)))
                                 nil
                                 (cons (cdr (car x))
                                       (handler (cdr x)))))
                           (handler x)) seqs)))))

(accumulate-n +
              0
              (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))