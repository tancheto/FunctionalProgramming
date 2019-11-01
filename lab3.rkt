#lang racket

;1.1
(define (fibonacci n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fibonacci (- n 1))
             (fibonacci (- n 2))))))

(define (fibonacci-iter n)
  (define (iter k a b)
        (if (= k n)
            a
            (iter (+ 1 k) (+ a b) a)))

    (iter 0 0 1))

;1.2
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
             (* 2 (f (- n 2)))
             (* 3 (f (- n 3))))))

(define (f-iter n)
  (define (iter k a b c)
  (if (= k n)
      c
      (iter (+ 1 k)
            (+ a
               (* 2 b)
               (* 3 c))
            a
            b)))
 
  (iter 0 2 1 0))

;1.3
(define (binomial-coefficient row index)
  (if (or (= row index)
          (= index 1))
      1
      (+
       (binomial-coefficient (- row 1) (- index 1))
       (binomial-coefficient (- row 1) index))))

; ----- high-order functions -----

;(define (prod a b term next)
;(if (> a b) 1 ( * (term a) (prod (next a) b term next))))

;(define (sum a b term next)
;(if (> a b) 0 ( + (term a) (sum (next a) b term next))))

;2.1
(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-iter op nv a b term next)
  (if (> a b)
      nv
      (accumulate-iter op (op nv (term a)) (next a) b term next)))

(define (sum a b term next)
  (accumulate + 0 a b term next))

(define (product a b term next)
  (accumulate * 1 a b term next))

(define (identity x) x)
(define (2+ x) (+ x 2))

(sum 1 5 identity 2+) ; 9
(product 1 5 identity 2+) ; 15
(accumulate + 0 1 5 identity 2+) ; 9
(accumulate * 1 1 5 identity 2+) ; 15

;2.2
;(define (count predicate a b)
;  (sum a
;       b
;       (lambda (x) (if (predicate x)
;                           1
;                           0))
;       (lambda (x) (+ 1 x))))

(define (count predicate a b)
  (accumulate +
              0
              a
              b
              (lambda (x)
                (if (predicate x)
                    1
                    0))
              (lambda (x) (+ x 1))))


(count even? 1 5)
(count even? 0 10)
(count odd? 1 5) 
(count odd? 0 10)

;2.3
(define (count-digits n)
  (if (> n 9) (+ 1 (count-digits (quotient n 10)))
      1))

(define (reverse-digits n)
  (if
   (> n 0)
   (+ (* (remainder n 10)
         (expt 10 (- (count-digits n) 1)))
      (reverse-digits (quotient n 10)))
   0))

(define (count-palindromes a b)
  (count (lambda (x)
           (if (= x (reverse-digits x))
               #t
               #f))
         a
         b))

(count-palindromes 1 5)
(count-palindromes 0 10)
(count-palindromes 11 100)
(count-palindromes 101 1000)

;2.4
;(define (exists? predicate a b)
;  (define (is-true-for? i)
;    (cond ((> i b) #f)
;          (else
;           (if (predicate i)
;               #t
;               (is-true-for? (+ 1 i))))))
;
; (is-true-for? a))

;(define (exists? predicate a b)
;  (accumulate (lambda (x y) (or x y))
;              #f
;              a
;              b
;              predicate
;              (lambda (x) (+ 1 x))))

(define (exists? predicate a b)
  (and (<= a b)
       (or (predicate a)
           (exists? predicate (+ a 1) b))))

(exists? (lambda (x) (= x 3)) 1 5)
(exists? (lambda (x) (< x 0)) -3 9)
(exists? (lambda (x) (= 0 (* x 0))) -3 15)

(exists? (lambda (x) (= x 13)) 1 5)
(exists? (lambda (x) (< x 3)) 10 42)
(exists? (lambda (x) (< x 0)) 3 8)
(exists? (lambda (x) (= 0 (* x 0))) 2 1)

;2.5
;(define (for-all? predicate a b)
;  (define (is-true-for? i)
;  (cond ((> i b) #t)
;        (else
;         (if (predicate i)
;             (is-true-for? (+ i 1))
;             #f))))
;
;  (is-true-for? a))

;(define (for-all? predicate a b)
;  (or (> a b)
;       (and (predicate a)
;            (for-all? predicate (+ 1 a) b))))

;(define (for-all? predicate a b)
;  (accumulate (lambda (current acc)
;                (and current acc))
;              #t
;              a
;              b
;              predicate
;              (lambda (x) (+ 1 x))))

(define (for-all? predicate a b)
  (not (exists? (lambda (x)
                  (not (predicate x)))
                a
                b)))

(for-all? (lambda (x) (> x 0)) 2 98)
(for-all? (lambda (x) (< x 0)) -10 -1)
(for-all? (lambda (x) (= 0 (* x 0))) -3 15)
(for-all? (lambda (x) (= 0 (* x 1))) 2 1) 

(for-all? (lambda (x) (= x 3)) 1 5)
(for-all? (lambda (x) (= x 13)) 1 5)
(for-all? (lambda (x) (< x 3)) -5 42)

(define (<10? x)
  (< x 10))

;2.6
;return anonymous function as a result
(define (double f)
  (lambda (x) (f(f x))))

(define (1+ x) (+ 1 x))

(define (10* x) (* x 10))
(define 100* (double 10*))

(100* 4) ; 400

(define (compose f g)
  (lambda (x)
    (f(g x))))

(define (square x) (* x x))

((compose square 1+) 6) ; 49
((compose 1+ square) 6) ; 37

;2.7
(define (repeated f n)
  (lambda (x)
    (if (= n 0)
        x
        (f((repeated f (- n 1)) x)))))

;(define (repeated f n)
;  (if (= n 0)
;      (lambda (x) x)
;      (lambda (x)
;        (f ((repeated f (- n 1)) x)))))

;(define (repeated f n)
;  (if (= n 0)
;      (lambda (x) x)
;      (compose f (repeated f (- n 1)))))

((repeated square 2) 5) ; 625