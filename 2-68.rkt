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

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(display sample-tree)
(newline)

(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(display (decode sample-message sample-tree))
(newline)

(define (encode-symbol symbol tree)
  (define (set-contain? symbol set)
    (cond ((null? set) false)
          ((eq? symbol (car set)) true)
          (else set-contain? symbol (cdr set))))
  (define (tree-contain? symbol tree)
    (if (leaf? tree)
        (and (leaf? tree) (eq? (symbol-leaf tree) symbol))
        (set-contain? symbol (symbols tree))))
  (cond ((and (leaf? tree) (eq? (symbol-leaf tree) symbol))
         '())
        ((tree-contain? symbol (left-branch tree))
         (cons '0 (encode-symbol symbol (left-branch tree))))
        ((tree-contain? symbol (right-branch tree))
         (cons '1 (encode-symbol symbol (right-branch tree))))
        (else (display "Error!"))))
  
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(display (encode '(A D A B B C A) sample-tree))
(newline)