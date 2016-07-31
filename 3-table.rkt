#lang sicp

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? (caar records) key) (car records))
        (else (assoc key (cdr records)))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table))))))

(define (make-table)
  (cons '*table* '()))

(define t (make-table))
(insert! 1 1 t)
(lookup 1 t)
(insert! 2 22 t)
(lookup 2 t)
