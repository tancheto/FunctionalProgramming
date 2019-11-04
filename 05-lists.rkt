#lang racket

;part 2

;1
(define (maximum l)
  (cond ((null? l) '())
        ((= (length l) 1) (car l))
        (else (if (> (car l) (maximum (cdr l)))
                  (car l)
                  (maximum (cdr l))))))

(define (minimum l)
  (cond ((null? l) '())
        ((= (length l) 1) (car l))
        (else (if (< (car l) (minimum (cdr l)))
                  (car l)
                  (minimum (cdr l))))))

(require rackunit rackunit/text-ui)

(define maximum-tests
  (test-suite
    "Tests for maximum"

    (check = (maximum '(2)) 2)
    (check = (maximum '(5 3 5 5)) 5)
    (check = (maximum '(8 4 92 82 8 13)) 92)
    (check = (maximum '(8 4 82 12 31 133)) 133)))

(run-tests maximum-tests)

;2
(define (remove l x)
  (cond ((null? l) l)
        ((= (car l) x) (cdr l))
        (else (cons (car l) (remove (cdr l) x)))))

(require rackunit rackunit/text-ui)

(define remove-tests
  (test-suite
    "Tests for remove"

    (check-equal? (remove '(42) 42) '())
    (check-equal? (remove '(5 3 5 5) 5) '(3 5 5))
    (check-equal? (remove '(8 4 92 82 8 13) 82) '(8 4 92 8 13))
    (check-equal? (remove '(8 4 82 12 31 133) 133) '(8 4 82 12 31))))

(run-tests remove-tests)

;3
(define (selection-sort l)
  (if (null? l)
      l
      (cons (minimum l) (selection-sort (remove l (minimum l))))))

(require rackunit rackunit/text-ui)

(define selection-sort-tests
  (test-suite
    "Tests for selection-sort"

    (check-equal? (selection-sort '(42)) '(42))
    (check-equal? (selection-sort '(6 6 6)) '(6 6 6))
    (check-equal? (selection-sort '(1 2 3 4 5 6)) '(1 2 3 4 5 6))
    (check-equal? (selection-sort '(6 5 4 3 2 1)) '(1 2 3 4 5 6))
    (check-equal? (selection-sort '(3 1 4 6 2 5)) '(1 2 3 4 5 6))
    (check-equal? (selection-sort '(5 2 5 1 5 2 3)) '(1 2 2 3 5 5 5))))

(run-tests selection-sort-tests)

;4
(define (filter p l)
  (if (null? l)
      l
      (if (p (car l))
          (cons (car l) (filter p (cdr l)))
          (filter p (cdr l)))))

(define (reject p l)
  (filter (lambda (x)(not (p x))) l))

(define (partition p l)
      (list (filter p l) (reject p l)))

(require rackunit rackunit/text-ui)

(define partition-tests
  (test-suite
    "Tests for partition"

    (check-equal? (partition even? '(1 2 3 4 5 6 7)) '((2 4 6) (1 3 5 7)))
    (check-equal? (partition odd? '(1 3 3 7 42)) '((1 3 3 7) (42)))
    (check-equal? (partition odd? '(3)) '((3) ()))
    (check-equal? (partition even? '()) '(() ()))
    (check-equal? (partition (lambda (x) (< x 4)) '(1 2 3 4 5 6 7))
                  '((1 2 3) (4 5 6 7)))))

(run-tests partition-tests)

;5
(define (flatten l)
  (cond ((null? l) l)
        ((number? l) (list l))
        (else (append (flatten (car l)) (flatten (cdr l))))))

(require rackunit rackunit/text-ui)

(define flatten-tests
  (test-suite
    "Tests for flatten"

    (check-equal? (flatten '((1 2) (3 4) (5 6))) '(1 2 3 4 5 6))
    (check-equal? (flatten '((1 2) 3 (4 5) (6 7))) '(1 2 3 4 5 6 7))
    (check-equal? (flatten '(5 3 5 5) ) '(5 3 5 5))
    (check-equal? (flatten '(5 () 3 () 5 () 5) ) '(5 3 5 5))))

(run-tests flatten-tests)

;6
(define (map-deep f l)
  (cond ((null? l) l)
        ((number? l) (f l))
        (else (cons (map-deep f (car l)) (map-deep f (cdr l))))))

(define (square x) (* x x))
(define (cube x) (* x x x))

(define map-deep-tests
  (test-suite
    "Tests for map-deep"

    (check-equal? (map-deep cube '())
                  '())
    (check-equal? (map-deep square '((1 2 (3 4)) 5))
                  '((1 4 (9 16)) 25))
    (check-equal? (map-deep square '((((2)) 1 ((4) 3)) (9)))
                  '((((4)) 1 ((16) 9)) (81)))
    (check-equal? (map-deep cube '(3 2 ((3) 4)))
                  '(27 8 ((27) 64)))))

(run-tests map-deep-tests)

;7
(define (zip a b)
  (cond ((or
         (and
          (= (length a) 1)
          (not (null? b)))
         (and
          (= (length b) 1)
          (not (null? a)))) (list (list (car a) (car b))))
        ((and
         (> (length a) 1)
         (> (length b) 1)) (cons
                            (list (car a) (car b))
                            (zip (cdr a) (cdr b))))
        (else '())))      

(require rackunit rackunit/text-ui)

(define zip-tests
  (test-suite
    "Tests for zip"

    (check-equal? (zip '() '(6 6 6)) '())
    (check-equal? (zip '(1 1 1) '(2 2)) '((1 2) (1 2)))
    (check-equal? (zip '(1 3 5) '(2 4 6)) '((1 2) (3 4) (5 6)))
    (check-equal? (zip '(1 3 5 7 9) '(2 4 6)) '((1 2) (3 4) (5 6)))))

(run-tests zip-tests)

;8
(define (member? x l)
  (if (null? l)
      #f
      (or (eq? x (car l))
          (member? x (cdr l)))))

(define (remove-duplicates l)
  (define (find-new-list l nl)
    (cond ((null? l) nl)
          ((member? (car l) nl) (find-new-list (cdr l) nl))
          (else (find-new-list (cdr l) (append nl (list (car l)))))))

  (find-new-list l '()))

(define remove-duplicates-tests
  (test-suite
    "Tests for remove-duplicates"

    (check-equal? (remove-duplicates '(42)) '(42))
    (check-equal? (remove-duplicates '(6 6 6)) '(6))
    (check-equal? (remove-duplicates '(1 2 3 4 5 6)) '(1 2 3 4 5 6))
    (check-equal? (remove-duplicates '(4 3 3 2 5 2)) '(4 3 2 5))
    (check-equal? (remove-duplicates '(10 10 8 2 2 2 9 9)) '(10 8 2 9))))

(run-tests remove-duplicates-tests)

;9
(define (chunk l n)
  (define (chunk-iter l n current-n new-chunk new-list)
    (cond ((null? l) (append new-list (list new-chunk)))
          ((= current-n n) (chunk-iter (cdr l) n 1 (list (car l)) (append new-list (list new-chunk))))
          (else (chunk-iter (cdr l) n (+ 1 current-n) (append new-chunk (list (car l))) new-list))))

  (if (null? l)
      l
      (chunk-iter l n 0 '() '())))

(define chunk-tests
  (test-suite
    "Tests for chunk"

    (check-equal? (chunk '() 42) '())
    (check-equal? (chunk '(1 2) 3) '((1 2)))
    (check-equal? (chunk '(1 2 3 4 5 6) 2) '((1 2) (3 4) (5 6)))
    (check-equal? (chunk '(1 2 3 4 5 6) 3) '((1 2 3) (4 5 6)))
    (check-equal? (chunk '(1 2 3 4 5 6 7 8) 3) '((1 2 3) (4 5 6) (7 8)))))

(run-tests chunk-tests)
