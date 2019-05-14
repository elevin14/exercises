#lang sicp

; Exercises for SICP chapter 2.3.3

; Helpers pulled from text or earlier assignments
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (element-of-set?3 x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set3 set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set?4 x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set4 x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-set x (right-branch set))))))

; Exercise 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))

(union-set (list 1 2 3) (list 1 3 5))

; Exercise 2.60
; element-of-set? and adjoin-set would be the same
(define (intersection-set2 set1 set2)
  (define (intersection-helper set1 set2 inter)
    (cond ((null? set1) inter)
          ((and (element-of-set? (car set1) set2) (not (element-of-set? (car set1) inter)))
           (intersection-helper (cdr set1) set2 (adjoin-set (car set1) inter)))
          (else (intersection-helper (cdr set1) set2 inter))))
  (intersection-helper set1 set2 '()))

(define (union-set2 set1 set2)
  (define (union-helper set1 set2 inter)
    (cond ((and (null? set1) (null? set2)) inter)
          ((and (null? set1) (element-of-set? (car set2) inter)) (union-helper set1 (cdr set2) inter))
          ((null? set1) (union-helper set1 (cdr set2) (adjoin-set (car set2) inter)))
          ((element-of-set? (car set1) inter) (union-helper (cdr set1) set2 inter))
          (else (union-helper (cdr set1) set2 (adjoin-set (car set1) inter)))))
  (union-helper set1 set2 '()))

(intersection-set2 (list 1 1 2 3 3 4 5) (list 5 3 0 7 3))
(union-set2 (list 1 1 2 3 3 4 5) (list 5 3 0 7 3))

; Exercise 2.61
; Ordered sets will use '3' version of methods
(define (adjoin-set3 x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((> x (car set)) (cons (car set) (adjoin-set3 x (cdr set))))
        (else (cons x set))))

(adjoin-set3 0 (list 1 2 5))

; Exercise 2.62
(define (union-set3 set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set3 (cdr set1) (cdr set2))))
                      ((< x1 x2) (cons x1 (union-set3 (cdr set1) set2)))
                      (else (cons x2 (union-set3 set1 (cdr set2)))))))))

(union-set3 (list 1 3 5 6) (list 0 3 5 6))

; Exercise 2.63
; Binary trees will use '4' version of methods
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(define tree1 (make-tree 7 (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
                         (make-tree 9 '() (make-tree 11 '() '()))))
(define tree2 (make-tree 3 (make-tree 1 '() '()) (make-tree 7 (make-tree 5 '() '())
                                                            (make-tree 9 '() (make-tree 11 '() '())))))
(define tree3 (make-tree 5 (make-tree 3 (make-tree 1 '() '()) '()) (make-tree 9 (make-tree 7 '() '())
                                                                              (make-tree 11 '() '()))))

(tree->list-1 tree1)
(tree->list-2 tree1)
(tree->list-1 tree2)
(tree->list-2 tree2)
(tree->list-1 tree3)
(tree->list-2 tree3)
; tree->list-2 is more effecient because append is an o(n) operation, while cons is o(1).

; Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))
(list->tree (list 1 3 5 6 7))

; Exercise 2.65
; A trivial solution is to cast the tree to a list using tree->list-2 (O(n)), and use solutions from
; ordered sets to implement union/interesction (also O(n))
(define (union-set-tree tree1 tree2)
  (list->tree (union-set3 (tree->list-2 tree1) (tree->list-2 tree2))))

(define (intersection-set-tree tree1 tree2)
  (list->tree (intersection-set3 ((tree->list-2 tree1) (tree->list-2 tree2)))))

; Exercise 2.66
(define (lookup-tree given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key set-of-records)) (entry set-of-records))
        ((< given-key (key set-of-records)) (lookup-tree given-key (left-branch set-of-records)))
        ((> given-key (key set-of-records)) (lookup-tree given-key (right-branch set-of-records)))))
  
