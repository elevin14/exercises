#lang sicp

; Exercises for SICP chapter 2.2.2

; Helpers pulled from earlier assignments or text
(define (square x) (* x x))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse x)
  (if (= (length x) 1)
      x
      (append (reverse (cdr x)) (cons (car x) nil))))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

; Exercise 2.24
(list 1 (list 2 (list 3 4)))

; Exercise 2.25
(car (cdaddr (list 1 3 (list 5 7) 9)))
(caar (list (list 7)))
(cadadr (cadadr (cadadr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))

; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)

; Exercise 2.27
(define (deep-reverse x)
  (cond((null? x) nil)
       ((not (pair? x)) x)
       (else (append (deep-reverse (cdr x)) (cons (deep-reverse (car x)) nil)))))

(define l3 (list (list 1 2) (list 3 4)))
(reverse l3)
(deep-reverse l3)

; Exercise 2.28
(define (fringe x)
  (cond ((null? x) nil)
        ((not (pair? x)) (list x))
        (else (append (fringe (car x)) (fringe (cdr x))))))

(define l4 (list (list 1 2 (list 3)) (list 4 5)))
(fringe l4)

; Exerceise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m) (list-ref m 0))
(define (right-branch m) (list-ref m 1))
(define (branch-length b) (list-ref b 0))
(define (branch-structure b)(list-ref b 1))

(define (total-weight m)
  (define (total-weight-branch b)
    (if (number? (branch-structure b))
        (branch-structure b)
        (total-weight (branch-structure b))))
  (+ (total-weight-branch (left-branch m)) (total-weight-branch (right-branch m))))

(total-weight (make-mobile (make-branch 5 5) (make-branch 1 (make-mobile (make-branch 5 8) (make-branch 4 9)))))

(define (balanced? m)
  (define (total-weight-branch b)
    (if (number? (branch-structure b))
        (branch-structure b)
        (total-weight (branch-structure b))))
  (define (balanced?-branch b)
    (if (number? (branch-structure b))
        #t
        (balanced? (branch-structure b))))
  (and (and (balanced?-branch (left-branch m)) (balanced?-branch (right-branch m)))
       (= (* (total-weight-branch (left-branch m)) (branch-length (left-branch m)))
          (* (total-weight-branch (right-branch m)) (branch-length (right-branch m))))))

(balanced? (make-mobile (make-branch 5 5) (make-branch 1 (make-mobile (make-branch 5 8) (make-branch 4 9)))))
(balanced? (make-mobile (make-branch 2 5) (make-branch 1 (make-mobile (make-branch 1 5) (make-branch 1 5)))))

; If defitions of make-mobile and make-branch are changed, only the left-branch, right-branch, branch-length,
; and branch-structure defitions need to be changed. The higher functions will stay the same.

; Exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-2 sub-tree)
             (* sub-tree sub-tree)))
       tree))

(square-tree-2 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

; Exercise 2.31
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(define (square-tree-3 tree) (tree-map square tree))
(square-tree-3 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))


