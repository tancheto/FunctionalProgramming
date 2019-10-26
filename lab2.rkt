#lang racket

(define(factorial-iter n)
   (define (iter product counter)
     (if (> counter n)
         product
         (iter (* product counter) (+ 1 counter))))
  
  (iter 1 1))

(define (sum-iter start end)
  (define (iter sum counter)
    (if
      (<= counter end) (iter (+ sum counter) (+ counter 1))
      sum))
  
  (iter 0 start))

(define (expt-iter x n)
  (define (iter product i)
    (if (> i n)
        product
        (iter (* x product) (+ i 1))))

  (if (< n 0)
      (/ 1 (expt-iter x (- n)))
      (iter 1 1)))

(define (count-digits n)
  (if (> n 9) (+ 1 (count-digits (quotient n 10)))
      1))

(define (count-digits-iter n)
  (define (iter counter n)
    (if
      (> n 9) (iter (+ 1 counter) (quotient n 10))
      counter)
  )
  (iter 1 n))

(define (sum-digits n)
  (if
   (> n 0) (+ (sum-digits (quotient n 10))
              (remainder n 10))
   0))

(define (sum-digits-iter n)
  (define (iter sum n)
    (if
     (> n 0) (iter
              (+ sum (remainder n 10))
              (quotient n 10))
     sum))
  
  (iter 0 0 n))

(define (reverse-digits n)
  (if
   (> n 0)
   (+ (* (remainder n 10)
         (expt 10 (- (count-digits n) 1)))
      (reverse-digits (quotient n 10)))
   0))

(define (reverse-digits-iter n)
  (define (iter result n)
    (if
     (> n 0)
     (iter
      (+ (* result 10) (remainder n 10))
      (quotient n 10))
     result))
  
  (iter 0 n))

(define (divides? k n) (= 0 (remainder n k)))

(define (count-divisors n)
  (define (count-divisors-up-to i)
    (cond ((= i 0) 0)
          ((divides? i n) (+ 1 (count-divisors-up-to (- i 1))))
          (else (count-divisors-up-to (- i 1)))))
  (count-divisors-up-to n))                     

(define (count-divisors-iter n)
  (define (iter count i)
    (cond ((> i n) count)
          ((divides? i n) (iter (+ 1 count) (+ 1 i)))
          (else (iter count (+ 1 i)))))
  (iter 0 1))

(define (sum-divisors n)
  (define (sum-up-to i)
    (cond ((= i 0) 0)
          ((divides? i n) (+ i (sum-up-to (- i 1))))
          (else (sum-up-to (- i 1)))))

  (sum-up-to n))

(define (sum-divisors-iter n)
  (define (iter sum i)
    (cond ((> i n) sum)
          ((divides? i n) (iter (+ sum i) (+ i 1)))
          (else (iter sum (+ i 1)))))

  (iter 0 1))

(define (prime? n)
  (= (count-divisors n) 2))

(define (prime?-iter n)
  (define (iter k)
    (or (= k n)
        (and (not (divides? k n))
             (iter (+ k 1)))))

  (and (not (= n 1)) (iter 2)))

(define (fast-expt x n)
  (if (= n 0)
      1
      (if (even? n)
          (sqr (fast-expt x (/ n 2)))
          (* x (expt x (- n 1))))))

(define (sqr x) (* x x))

(define (fast-expt-iter x n)
  (define (iter product x n)
    (cond ((= n 0) product)
          ((even? n) (iter product (sqr x) (/ n 2)))
          (else (iter (* x product) x (- n 1)))))

  (if (< n 0)
      ( / 1 (fast-expt-iter x (- n)))
      (iter 1 x n)))