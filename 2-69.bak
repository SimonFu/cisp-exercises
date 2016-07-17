#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (decode bits tree)
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (display (list "bad bit -- CHOOSE-BRANCH" bit)))))
  (define (decode-1 bits current-tree)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-tree)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  
  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (cons (make-leaf (car (car pairs))
                       (cadr (car pairs)))
            (make-leaf-set (cdr pairs)))))

(display (make-leaf-set '((A 4) (B 2) (C 1) (D 1))))
(newline)

(define (generate-huffman-tree pairs)
  (define (successive-merge tree)
    (define (merge-iter tree)
      (cond ((null? (cddr tree)) (list (make-code-tree (car tree)
                                                 (cadr tree))))
            ((< (+ (weight (car tree)) (weight (cadr tree)))
                (+ (weight (cadr tree)) (weight (caddr tree))))
             (append (make-code-tree (car tree)
                                     (cadr tree))
                     (caddr tree)))
            (else (cons (car tree) (successive-merge (cdr tree))))))
    (let ((next-merge (merge-iter tree)))
      (if (null? (cdr next-merge))
          next-merge
          (merge-iter next-merge))))
  (successive-merge (make-leaf-set pairs)))

(display (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))
(newline)