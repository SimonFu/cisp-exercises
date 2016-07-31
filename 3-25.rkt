#lang sicp

(define (make-table)
  (let ((local-table (cons '*table* '())))
    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? (caar records) key) (car records))
            (else (assoc key (cdr records)))))
    
    (define (lookup keys)
      (define (lookup-iter keys records)
        (let ((record (assoc (car keys) records)))
          (if record
              (if (null? (cdr keys))
                  (cdr record)
                  (lookup-iter (cdr keys) (cdr record)))
              false)))
      (lookup-iter keys (cdr local-table)))
    
    (define (make-record keys value)
      (define (make-record-iter keys)
        (cond ((null? keys) value)
              (else (cons (cons (car keys) (make-record-iter (cdr keys))) '()))))
      (make-record-iter keys))
    
    (define (insert! keys value)
      (define (insert-iter! keys value table)
        (let ((record (assoc (car keys) (cdr table))))
          (cond (record
                 (if (null? (cdr keys))
                     (set-cdr! record value)
                     (insert-iter! (cdr keys) value record)))
                (else (set-cdr! table
                                (cons (cons (car keys) (make-record (cdr keys) value))
                                      (cdr table)))))))
      (insert-iter! keys value local-table))
    
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (display "error"))))
    
    dispatch))

(define t (make-table))
((t 'insert!) '(2 2 1) 222)
((t 'insert!) '(1 1 1) 111)
((t 'insert!) '(1 3 4) 134)
((t 'insert!) '(1 2 3) 123)
((t 'insert!) '(3 2 1) 321)
((t 'insert!) '(3 2 2) 322)
((t 'lookup) '(3 2 1))
((t 'lookup) '(3 2 2))
((t 'lookup) '(1 2 3))