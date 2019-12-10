#lang racket
;1
(define (length l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))

;2
(define (sum l)
  (if (null? l)
      0
      (+ (car l) (sum (cdr l)))))

;3
(define (member? x l)
  (if (null? l)
      #f
      (or (= x (car l))
          (member? x (cdr l)))))

;4
(define (last l)
  (cond ((null? l) '())
        ((= (length l) 1) (car l))
        (else (last (cdr l)))))

;5
(define (nth l n)
  (cond ((or (null? l) (> n (length l))) '())
        ((= 0 n) (car l))
        (else (nth (cdr l) (- n 1)))))

;6
(define (scale l x)
  (if (null? l)
      l
      (cons (* x (car l)) (scale (cdr l) x))))

;7
(define (add-last l x)
  (if (null? l)
      (cons x l)
      (cons (car l) (add-last (cdr l) x))))

;8
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

;9
(define (reverse l)
  (if (null? l)
      '()
      (append (reverse (cdr l)) (list (car l)))))

;10
(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l))
            (map f (cdr l)))))

(define (identity x) x)
(define (1+ x) (+ x 1))
(define (square x) (* x x))

 (map square '())
 (map identity '(42)) 
(map 1+ '(41)) 
 (map 1+ '(1 2 3 4)) 
 (map square '(1 2 3 4 5)) 
 (map identity '(8 4 92 82 8 13)) 
 (map 1+ '(8 4 92 82 8 13))

(require rackunit rackunit/text-ui)

(define map-tests
  (test-suite
    "Tests for map"

    (check-equal? (map square '()) '())
    (check-equal? (map identity '(42)) '(42))
    (check-equal? (map 1+ '(41)) '(42))
    (check-equal? (map 1+ '(1 2 3 4)) '(2 3 4 5))
    (check-equal? (map square '(1 2 3 4 5)) '(1 4 9 16 25))
    (check-equal? (map identity '(8 4 92 82 8 13)) '(8 4 92 82 8 13))
    (check-equal? (map 1+ '(8 4 92 82 8 13)) '(9 5 93 83 9 14))))

(run-tests map-tests)

;11
(define (filter p l)
  (if (null? l)
      '()
      (if (p (car l))
          (cons (car l)
                (filter p (cdr l)))
          (filter p (cdr l)))))

;12
(define (reject p l)
  (filter (lambda (x) (not (p x))) l))

;accumulate
(define (accumulate op nv a b term next)
(if (> a b)
    nv
    (op (term a) (accumulate op nv (next a) b term next))))

(define (foldr combiner null-value l)
  (accumulate combiner
              null-value
              0
              (- (length l) 1)
              (lambda (i) (list-ref l i))
              (lambda (a) (+ 1 a))
              ))

;accumulate-iter

(define (accumulate-iter op nv a b term next)
  (if (> a b)
      nv
      (accumulate-iter op (op nv (term a)) (next a) b term next)))
  
(define (foldl combiner null-value l)
  (accumulate-iter combiner
                   null-value
                   0
                   (- (length l) 1)
                   (lambda (i) (list-ref l i))
                   (lambda (a) (+ 1 a))))

;13
(define (foldr op nv l)
  (if (null? l)
      nv
      (op (car l)
          (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l)
      nv
      (foldl op
             (op nv (car l))
             (cdr l))))
