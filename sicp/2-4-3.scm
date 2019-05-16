#lang sicp

; Exercises for SICP chapter 2.4.3

; Helpers pulled from text or earlier assignments
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt b (/ n 2))))
        (else (* b (expt b (- n 1))))))

(define (square x) (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

; Table procedures
(define (lookup key-1 key-2 table)
  (let ((subtable
         (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; Exercise 2.73
; A) Instead of enumerating all procedures, we now use a lookup table to operate on the deriv.
;    We cannot use number and variable in the lookup table because they would have no inherant operator.
; B) Needed to pull table methods from elswhere.
; C) Add exponent

(define (install-deriv-operator-package)
  (define (deriv-sum exp var) (make-sum (deriv (addend exp) var)
                                        (deriv (augend exp) var)))
  (define (deriv-product exp var) (make-sum
                                   (make-product (multiplier exp)
                                                 (deriv (multiplicand exp) var))
                                   (make-product (deriv (multiplier exp) var)
                                                 (multiplicand exp))))
  (define (deriv-expt exp var) (make-product
                                (make-product (exponent exp)
                                              (make-exponentiation (base exp)
                                                                   (make-sum (exponent exp) -1)))
                                (deriv (base exp) var)))
  (define (make-exponentiation b n)
    (cond ((=number? n 0) 1)
          ((=number? n 1) b)
          ((and (number? b) (number? n)) (expt b n))
          (else (list '** b n))))
  (define (=number? exp num) (and (number? exp) (= exp num)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (base e) (car e))
  (define (exponent e) (cadr e))

  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-expt)
  'done)

(install-deriv-operator-package)

(deriv '(** x 2) 'x)

; D) We would just need to change the order of the put statement to list procedure and then deriv.

; Exercise 2.74
; A/B)To get different formatted records, something like the methods below should work. Each group needs
; to tag their specific methods in the table. I will impliment an example.
(define (office-1-package)
  (define (make-employee name record salary)
    (list name record salary))
  (define (get-name e) (car e))
  (define (get-salary e) (caddr e))
  (define (get-record e) (cadr e))
  (define (tag x) (attach-tag 'office-1 x))
  (put 'get-salary '(office-1) get-salary)
  (put 'get-record '(office-1) get-record)
  (put 'get-name '(office-1) get-name)
  (put 'make-employee 'office-1
       (lambda (r s) (tag (make-employee r s))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

(define (get-record e) (apply-generic 'get-record e))
(define (get-salary e) (apply-generic 'get-salary e))
; C
(define (find-employee-record e f) (or (get-record (car f) e) (find-employee-record e (cdr f))))
; D) changes only need to be made to the new company's files.

; Exercise 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

; Exercise 2.76
; Explicit dispatch is good for small systems with limited expansions. Data-directed is good for
; systems with new base data types being added frequently. Message passing may be good if there are
; few data types, but new operations being constantly added. Generally, data-directed is probably the
; best.