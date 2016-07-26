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
             queue)
      (car (front-ptr queue))))

(define (insert-queue queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-cdr! (rear-ptr queue) new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))

(define (delete-queue queue)
  (cond ((empty-queue? queue)
         (display "queue is empty. Can not delete item from it.")
         (newline)
         queue)
        ((eq? (front-ptr queue) (rear-ptr queue))
         (set-front-ptr! queue (cdr (front-ptr queue)))
         (set-rear-ptr! queue (cdr (rear-ptr queue)))
         queue)
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

(define q (make-queue))
(empty-queue? q)
(delete-queue q)
(front-queue q)
(insert-queue q '1)
(delete-queue q)
(insert-queue q '2)
(insert-queue q '3)
(front-queue q)
(delete-queue q)