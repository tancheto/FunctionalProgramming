#lang racket

(define (add x y) (+ x y))

(define (even? n) (=(remainder n 2)0))

(define (odd? n) (not (even? n)))

(define (signum x) (cond ((< x 0) -1)
                         ((= x 0) 0)
                         (else 1)))

(define (fac x)
  (if (= x 0)
      1
      (* x(fac (- x 1)))))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (suma x y)
  (if (> x y)
      0
      (+ x (suma (+ x 1) y))))

(define (sum start end)
  (if (> start end)
     0
     (+ start
        (sum (+ start 1) end))))

(define (expt x n)
  (if (= n 0)
      1
      (* x (expt x (- n 1)))))

(define (sqr x) (* x x))

(define (fast-expt x n)
  (if (= n 0)
      1
      (if (even? n)
          (sqr (fast-expt x (/ n 2)))
          (* x (expt x (- n 1))))))
