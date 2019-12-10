#lang racket
;1a
(define (product-digits n)
  (define (prod-iter n prod)
    (if (< n 10)
        (* prod n)
        (prod-iter (quotient n 10) (* prod (remainder n 10)))))

  (prod-iter n 1))

;1b
(define (diff n)
  (- n (product-digits n)))

(define (pairs-fixed-beg a b)
  (if (= a b)
      '()
      (append (list (cons a b) (cons b a)) (pairs-fixed-beg a (- b 1)))))

(define (pairs-list a b)
  (if (= a b)
      '()
      (append (pairs-fixed-beg a b) (pairs-list (+ a 1) b))))

(define (largest-diff a b)
  (define pairs (pairs-list a b))
  
  (define (iter max l)
    (define cur-pair (if (null? l)
                         l
                         (car l)))
    (define cur-diff (if (null? l)
                         0
                         (- (diff (car cur-pair)) (diff (cdr cur-pair)))))
    
    (cond ((null? l) max)
          ((< max cur-diff) (iter cur-diff (cdr l)))
          (else (iter max (cdr l)))))

  (iter (- (diff (car (car pairs))) (diff (cdr (car pairs)))) pairs))

;2
(define (max-metric ml ll)
  (define (sum-applied-metric sum m l)
    (if (null? l)
        sum
        (sum-applied-metric (+ sum (m (car l))) m (cdr l))))

  
  (define (iter max-value max-metr metric-list)
    (cond ((null? metric-list) max-metr)
          ((< max-value (sum-applied-metric 0 (car metric-list) ll)) (iter (sum-applied-metric 0 (car metric-list) ll)
                                                                           (car metric-list)
                                                                           (cdr metric-list)))
          (else (iter max-value max-metr (cdr metric-list)))))

  (iter (sum-applied-metric 0 (car ml) ll) (car ml) ml))

(define (prod l) (apply * l))
(define (suma l) (apply + l))

;(max-metric (list suma prod) '((0 1 2) (3 4 5) (1337 0)))
;(max-metric (list car suma) '((1000 -1000) (29 1) (42)))

;3
(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (list-rep x rep)
  (if (= rep 0)
      '()
      (cons x (list-rep x (- rep 1)))))

(define (deep-repeat l)
  (define (iter depth l)
    (cond ((null? l) '())
          ((atom? l) (list-rep l depth))
          (else (cons (iter (+ 1 depth) (car l)) (iter depth (cdr l))))))

  (iter 0 l))

;(deep-repeat '(1 (2 3) 4 (5 (6))))




