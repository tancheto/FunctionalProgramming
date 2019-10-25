#lang racket

(define(factorial-iter n)
   (define (iter product counter)
     (if
       (<= counter n)(iter (* product counter) (+ 1 counter))
       product
          ))
  (iter 1 1))

(define (sum-iter start end)
  (define (iter sum counter)
    (if
      (<= counter end) (iter (+ sum counter) (+ counter 1))
      sum))
  (iter 0 start)
  )

(define (expt-iter x n)
  (define (iter product counter)
    (if
     (<= counter n) (iter (* product x) (+ counter 1))
     product))
  (iter 1 1))

(define (count-digits n)
  (if (> n 0) (+ 1 (count-digits (quotient n 10)))
      0))

(define (count-digits-iter n)
  (define (iter counter n)
    (if
      (> n 0) (iter (+ 1 counter) (quotient n 10))
      counter)
  )
  (iter 0 n))

(define (sum-digits n)
  (if
   (> n 0) (+ (sum-digits (quotient n 10)) (remainder n 10))
   0))

(define (sum-digits-iter n)
  (define (iter counter sum n)
    (if
     (> n 0) (iter (+ counter 1) (+ sum (remainder n 10)) (quotient n 10))
     sum))
  (iter 0 0 n))

(define (reverse-digits n)
  (if
   (> n 0) (+ (* (remainder n 10) (expt 10 (- (count-digits n) 1))) (reverse-digits (quotient n 10)))
   0))

(define (reverse-digits-iter n)
  (define (iter result n)
    (if
     (> n 0) (iter (+ (* result 10) (remainder n 10)) (quotient n 10))
     result))
  (iter 0 n))

(define (count-divisors n)
  (define (count n i)
    (cond ((and (> n i) (= 0 (remainder n i))) (+ 1 (count n (+ 1 i))))
          ((or (= n i) (= n 0)) 1)
          ((not (= 0 (remainder n i))) (count n (+ 1 i)))))
  (count n 1))

(define (count-divisors-iter n)
  (define (iter count n i)
    (cond
      ((and (> n i) (= 0 (remainder n i))) (iter (+ 1 count) n (+ 1 i)))
      ((= n i) count)
      ((= n 0) 1)
      ((not (= 0 (remainder n i))) (iter count n (+ 1 i)))))
  (iter 1 n 1))

(define (sum-divisors n)
  (define (sum n i)
    (cond ((and (> n i) (= 0 (remainder n i))) (+ i (sum n (+ 1 i))))
          ((or (= n i) (= n 0)) n)
          ((not (= 0 (remainder n i))) (sum n (+ 1 i)))))
  (sum n 1))

(define (sum-divisors-iter n)
  (define (iter sum n i)
    (cond
      ((and (> n i) (= 0 (remainder n i))) (iter (+ i sum) n (+ 1 i)))
      ((= n i) (+ i sum))
      ((= n 0) 0)
      ((not (= 0 (remainder n i))) (iter sum n (+ 1 i)))))
  (iter 0 n 1))

(define (prime? n)
  (= (count-divisors n) 2))

(define (fast-expt x n)
  (if (= n 0)
      1
      (if (even? n)
          (sqr (fast-expt x (/ n 2)))
          (* x (expt x (- n 1))))))

(define (sqr x) (* x x))

;(define (fast-expt-iter x n)
;  (define (iter product counter)
;    (if (= n counter) 
;        product
;        (if (integer? n)
;            (if (even? n)
;                (iter (* product x) (+ 1 counter))
;                (sqr (fast-expt-iter x (/ n 2))))
;            (sqr (fast-expt-iter x (/ n 2))))))
;  (iter 1 0))
  