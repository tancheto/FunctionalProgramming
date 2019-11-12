#lang racket

;1
(define(number-to-list n)
  (define (reversed-num n)
    (if (< n 10)
      (list n)
      (cons (remainder n 10)
            (reversed-num (quotient n 10)))))
  (reverse (reversed-num n)))

(define (middle-digit n)
  (define num-list (number-to-list n))
  
  (if (even? (length num-list))
      -1
      (list-ref num-list (quotient (length num-list) 2))))

;2
(define (every? p l)
  (or (null? l)
      (and  (p (car l))
            (every? p (cdr l)))))

(define (endomorphism? l op f)
  (define (image-in-l? x)
    (member (f x) l))

  (define (f-preserves-op? x y)
    (= (op (f x) (f y))
       (f (op x y))))
  
  (and (every? image-in-l? l)
       (every? (lambda (x)
                 (every? (lambda (y)
                           (f-preserves-op? x y))
                         l))
               l)))

;3
(define (compose f g)
  (lambda (x) (f (g x))))

(define (any? p l)
  (not (every? (compose not p) l)))

(define (enum-interval a b)
  (if (> a b)
      '()
      (cons a (enum-interval (+ 1 a) b))))

(define (meet-twice? f g a b)
  (define int (enum-interval a b))
  
  (any? (lambda (x)
          (any? (lambda (y)
                  (and (not (= x y))
                       (= (f x) (g x))
                       (= (f y) (g y))))
                int))
        int))

(define (take-while p l)
  (if (or (null? l)
          (not (p (car l))))
      '()
      (cons (car l)
            (take-while p (cdr l)))))

(define (drop-while p l)
  (if (or (null? l)
          (not (p (car l))))
      l
      (drop-while p (cdr l))))

(define (next-look-and-say y)
  (define (take-first-equals y)
    (take-while (lambda (x)
                  (= x (car y))) y))

  (define (drop-first-equals y)
    (drop-while (lambda (x)
                  (= x (car y))) y))
  
  (if (null? y)
      y
      (cons (length (take-first-equals y))
            (cons (car y)
                  (next-look-and-say (drop-first-equals y))))))