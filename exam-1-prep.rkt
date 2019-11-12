#lang racket

;1
(define (section a b)
  (filter (lambda (x)(member x b)) a))

(define (union a b)
  (append (filter (lambda (x) (not (member x (section a b)))) (append a b)) (section a b)))

(define (diff a b)
  (filter (lambda (x) (not (member x (section a b)))) a))

;2
(define (filter-func f l)
  (filter (lambda (x) (member (f x) l)) l))

;3
(define (num-to-list n)
  (define (reversed-list n)
    (if (< n 10)
        (list n)
        (cons (remainder n 10)
              (reversed-list (quotient n 10)))))

  (reverse (reversed-list n)))

(define (middle-digit n)
  (define num-list (num-to-list n))

  (if (even? (length num-list))
      -1
      (list-ref num-list (quotient (length num-list) 2))))

;6
(define (every? p l)
  (or (null? l)
      (and (p (car l))
           (every? p (cdr l)))))

(define (exists? p l)
  (not (every? (lambda (x) (not (p x))) l)))

(define (is-em? l op f)
  (define (is-image-in-l? x)
    (member (f x) l))
  
  (and (every? is-image-in-l? l)
       (every? (lambda (x)
                 (every? (lambda (y)
                           (= (op (f x) (f y)) (f (op x y))))
                         l))
               l)))

;7
(define (is-sur? l1 l2 f)
  (define (is-image-element-of? x)
    (member (f x) l2))

  (define (exists-proto? y)
    (every? (lambda (x)
              (exists? (lambda (x) (= (f x) y))
                       l1))
            l2))
  
  (and (every? is-image-element-of? l1)
       (every? exists-proto? l2)))

;11
(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))))

(define (reverse-num n)
  (define (iter rev n)
    (if (< n 9)
        (+ (* rev 10) n)
        (iter (+ (* rev 10) (remainder n 10)) (quotient n 10))))

  (iter 0 n))

(define (diff-reverse n)
  (- n (reverse-num n)))

;12
(define (incr-elem pos l)
  (if (or (= pos 0)
          (null? l))
      (cons (+ 1 (car l)) (cdr l))
      (cons (car l) (incr-elem (- pos 1) (cdr l)))))

(define (sort-digits n)
  (define list-dig (list 0 0 0 0 0 0 0 0 0 0))

  (define (iter n l)
    (if (< n 10)
        (incr-elem (- 9 n) l)
        (iter (quotient n 10) (incr-elem (- 9 (remainder n 10)) l))))

  (define (list-to-num l n pos)
    (cond ((null? l) n)
          ((= (car l) 0) (list-to-num (cdr l) n (- pos 1)))
          (else (list-to-num (cons (- (car l) 1) (cdr l))
                             (+ (* 10 n) pos)
                             pos))))

  (list-to-num (iter n list-dig) 0 9))

;13
(define (for-each-even? p a b)
  (or (> a b)
      (odd? a)
      (and (p a)
           (for-each-even? p (+ 1 a) b))))

(define (compose f g x times)
  (if (= times 0)
      x
      (compose f g (f (g x)) (- times 1))))

(define (permutable? a b f g)
  (define (pred? x)
    (= (compose f g x x) (compose g f x x)))

  (for-each-even? pred? a b))

;14
(define (longest-interval-subsets il)
  (define (longest-interval l i amplitude)
    (cond ((null? l) i)
          ((> (- (cdr (car l)) (car (car l))) amplitude)
           (longest-interval (cdr l)
                                (car l)
                                (- (cdr (car l)) (car (car l)))))
          (else (longest-interval (cdr l) i amplitude))))

  (define (pred? p)
    (define long (longest-interval il '() 0))
    
    (and (pair? p)
         (>= (car p) (car long))
         (<= (cdr p) (cdr long))))

  (filter pred? il))

;15
(define (narcissistic? n)
  (define (length num)
    (if (< num 10)
        1
        (+ 1 (length (quotient num 10)))))

  (define (sum-of-powers n)
    (define len (length n))

    (define (sum-iter sum n)
      (if (< n 10)
          (+ sum (expt n len))
          (sum-iter (+ sum (expt (remainder n 10) len))
                    (quotient n 10))))

  (sum-iter 0 n))

  (= n (sum-of-powers n)))

;16
(define (friendly? a b)
  (define (d x)
    (define (sum-iter x i sum)
      (cond ((= x i) sum)
            ((= 0 (remainder x i)) (sum-iter x (+ 1 i) (+ sum i)))
            (else (sum-iter x (+ 1 i) sum))))

    (sum-iter x 1 0))

  (and (= (d a) b)
       (= (d b) a)))

;17
(define (make-interval a b)
  (if (> a b)
      '()
      (cons a (make-interval (+ 1 a) b))))

(define (make-subintervals a b)
  (if (= a b)
      '()
      (cons (make-interval a b) (make-subintervals a (- b 1)))))

(define (get-subintervals a b)
  (if (= a b)
      '()
      (append (make-subintervals a b) (get-subintervals (+ 1 a) b))))

(define (f-comp f l)
    (if (= 2 (length l))
        (f (car l) (car (cdr l)))
        (f (car l) (f-comp f (cdr l)))))

(define (find-max f a b)
  (define subint (get-subintervals a b))

  (define (get-max max l)
    (cond ((null? l) max)
          ((> (f-comp f (car l)) max)
           (get-max (f-comp f (car l)) (cdr l)))
          (else (get-max max (cdr l)))))

  (get-max (f-comp f (make-interval a b)) subint))




