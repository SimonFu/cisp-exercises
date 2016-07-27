#lang sicp

(define (front-ptr queue)
  (car queue))

(define (rear-ptr queue)
  (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue)
  (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (begin (display "queue is empty. Can not call front on it.")
             (newline)
             (print-queue queue))
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           (print-queue queue))
          (else (set-cdr! (rear-ptr queue) new-pair)
                (set-rear-ptr! queue new-pair)
                (print-queue queue)))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (display "queue is empty. Can not delete item from it.")
         (newline)
         (print-queue queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              (print-queue queue))))

(define (print-queue queue)
  (display (front-ptr queue)))

(define q (make-queue))
;(delete-queue q)
;(front-queue q)
(insert-queue! q '1)
(newline)
(delete-queue! q)
(newline)
(insert-queue! q '2)
(newline)
(insert-queue! q '3)
(newline)
(front-queue q)
;(newline)
(delete-queue! q)
(newline)