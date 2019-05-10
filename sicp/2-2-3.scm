#lang sicp

; Exercises for SICP chapter 2.2.3

; Helpers pulled from earlier assignments or text
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (square n) (* n n))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; Exercise 2.33
(define (map2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(append (list 1 2 3) (list 4 5 6))
(map2 (lambda (x) (+ x 1)) (list 1 2 3 4))
(length (list 1 2 3))

; Exerceise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 1 1 3))
(horner-eval 2 (list 1 3 0 5 0 1))

; Exercise 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(count-leaves (list 1 (list 2 (list 3 4)) 5))

; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (accumulate (lambda (x y) (cons (car x) y)) nil seqs))
            (accumulate-n op init (accumulate (lambda (x y) (cons (cdr x) y)) nil seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose m)
  (accumulate-n cons nil m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

 
(dot-product (list 1 2 0) (list 2 7 1))
(matrix-*-vector (list (list 1 2 3) (list 3 4 5)) (list 5 6 1))
(transpose (list (list 1 1 1 1) (list 2 2 2 2)))
(matrix-*-matrix (list (list 1 1 1 1) (list 2 2 2 2)) (list (list 2 3) (list 2 3) (list 2 3) (list 2 3)))

; Exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(fold-right / 1 (list 1 2 3)) ; will be 3/2
(fold-left / 1 (list 1 2 3)) ; will be 1/6
(fold-right list nil (list 1 2 3)) ; will be (1 2 3) was (1 (2 (3 ())))
(fold-left list nil (list 1 2 3)) ; will be (3 2 1) was (((() 1) 2) 3)

; if order of arguments does not matter, fold left and right will provide same results.
(fold-right + 0 (list 1 2 3))
(fold-left + 0 (list 1 2 3))

; Exercise 2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(reverse (list 1 2 3))
(reverse2 (list 1 2 3))

; Exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(unique-pairs 5)

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 5)

; Exercise 2.41

(define (unique-triples n)
  (flatmap (lambda (pair)
             (map (lambda (k) (append pair (list k)))
                  (enumerate-interval 1 (- (cadr pair) 1))))
             (unique-pairs n)))

(unique-triples 5)

; Exercise 2.42
