#lang sicp

; Exercises for SICP chapter 2.3.4

; Helpers pulled from text or earlier assignments
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

; Exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

; Exercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol char tree)
  (cond ((not (memq char (symbols tree)))
         (error "symbol not in tree," char))
        ((leaf? tree) '())
        ((memq char (symbols (left-branch tree))) (cons 0 (encode-symbol char (left-branch tree))))
        (else (cons 1 (encode-symbol char (right-branch tree))))))

(encode '(A D A B B C A) sample-tree)

; Exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set)) (car leaf-set)
      (successive-merge (adjoin-set
                         (make-code-tree (car leaf-set) (cadr leaf-set))
                         (cddr leaf-set)))))

(generate-huffman-tree (list (list 'A 4) (list 'B 5) (list 'C 1) (list 'D 1)))

; Exercise 2.70
(define 50s-tree
  (generate-huffman-tree (list
                          (list 'A 2)
                          (list 'GET 2)
                          (list 'SHA 3)
                          (list 'WAH 1)
                          (list 'BOOM 1)
                          (list 'JOB 2)
                          (list 'NA 16)
                          (list 'YIP 9))))

(define 50s-message
  '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(encode 50s-message 50s-tree)
; 84 bits are required for the encoding. Using fixed length, we'd require 3 bits, and there are 86 chars,
; so 258 total bits.

; Exercise 2.72
; O(nlogn) to encode a symbol. N time to search in lists for the symbol, and the tree is logn deep.
  