#lang racket

;1.1
(define (compose-two f g)
  (lambda (x)(f (g x))))

(define (identity x) x)

(define (compose . fns)
  (if (null? fns)
  identity
  (compose-two (car fns)
               (apply compose (cdr fns)))))

(define (double x) (* 2 x))
(define (square x) (* x x))
(define (inc x) (+ x 1))

(define f (compose double square inc)) ; 2 * (x + 1)^2
(f 3) ; -> 32
(f 4) ; -> 50

;1.2
(define (flip fn)
  (lambda args
    (apply fn (reverse args))))

(define list^ (flip list))
(list^ 1 2 3) ; -> (3 2 1)

;1.3
(define (zip x y)
  (if (or (null? x)
          (null? y))
      '()
      (cons (list (car x) (car y)) (zip (cdr x) (cdr y)))))

(zip '(1 3 5) '(2 4 6)) ; -> ((1 2) (3 4) (5 6))
(zip '(1 3 5) '(2 4 6 8)) ; -> ((1 2) (3 4) (5 6))

(define (zip-with fn x y)
    (if (or (null? x)
            (null? y))
        '()
        (cons (fn (car x) (car y)) (zip-with fn (cdr x) (cdr y)))))

(zip-with + '(1 3 5) '(2 4 6)) ; -> (3 7 11)
(zip-with + '(1 3 5) '(2 4 6 8)) ; -> (3 7 11)

(define (any? p l)
  (and (not (null? l))
       (or (p (car l))
           (any? p (cdr l)))))

(define (zip-with* fn . ls)
  (if (or (null? ls)
          (any? null? ls))
      '()
      (cons (apply fn (map car ls)) (apply zip-with* fn (map cdr ls)))))

(zip-with* + '(1 2 3) '(4 5 6) '(7 8 9)) ; -> (12 15 18)
(zip-with* cons '(1 3 5) '(2 4 6 8 10)) ; -> ((1 . 2) (3 . 4) (5 . 6))


;1.4
(define (juxt . fns)
  (lambda args
    (define (inner fns)
      (if (null? fns)
          '()
          (cons (apply (car fns) args) (inner (cdr fns)))))
    
    (inner fns)))

(define (dec x) (- x 1))

(define func (juxt inc dec square double)) ; (func x) = (list (inc x) (dec x) (square x) (double x))
(func 5) ; -> (6 4 25 10)

(define g (juxt + *))
(g 3 4 5) ; -> (12 60)

;2.1
(define (dimensions m)
  (cons (length m) (length (car m))))

(dimensions '((1 2 3) (4 5 6))) ; -> (2 . 3)

;2.2
(define (reverse-columns matrix)
  (map reverse matrix))

(reverse-columns '((1 2 3) (4 5 6) (7 8 9))) ; -> ((3 2 1) (6 5 4) (9 8 7))

;2.3
(define (nth-column matrix n)
  (map (lambda (row) (list-ref row n)) matrix))

(nth-column '((1 2 3) (4 5 6) (7 8 9)) 1) ; -> (2 5 8)

;2.4
(define (enum-interval a b)
  (if (> a b)
      '()
      (cons a (enum-interval (+ 1 a) b))))

(define (main-diagonal matrix)
  (map (lambda (row i)
         (list-ref row i))
       matrix
       (enum-interval 0 (- (length matrix) 1))))

(main-diagonal '((1 2 3) (4 5 6) (7 8 9))) ; -> (1 5 9)

;2.5
;(define (transpose matrix)
;  (apply map list matrix))

(define (transpose matrix)
  (map (lambda (column-index)
         (nth-column matrix column-index))
       (enum-interval 0 (- (length (car matrix)) 1))))

(transpose '((1 2 3) (4 5 6))) ; -> ((1 4) (2 5) (3 6))
(transpose '((1)))

;2.6
(define (for-all? p l)
  (or (null? l)
      (and (p (car l))
           (for-all? p (cdr l)))))

(define (for-all-columns? p m)
  (define (for-all-rows? p m)
  (or (null? m)
      (and (p (car m))
           (for-all-rows? p (cdr m)))))

  (for-all-rows? p (transpose m)))


(define (odd-exists? l)
  (any? odd? l))

(for-all-columns? odd-exists? '((1)))
(for-all-columns? odd-exists? '((1) (2)))
(for-all-columns? odd-exists? '((1 2 3)))
(for-all-columns? odd-exists? '((1 2 3) (4 5 6)))
(for-all-columns? odd-exists? '((1 2 3) (4 5 6) (7 8 9)))
(for-all-columns? odd-exists? '((1 2 3) (4 42 6) (7 8 9)))

  

;2.7
(define (divides? k n) (= 0 (remainder n k)))

(define (count-divisors n)
  (define (count-divisors-up-to i)
    (cond ((= i 0) 0)
          ((divides? i n) (+ 1 (count-divisors-up-to (- i 1))))
          (else (count-divisors-up-to (- i 1)))))
  (count-divisors-up-to n))

(define (is-prime? n)
  (= (count-divisors n) 2))

(define (exists? p l)
  (and (not (null? l))
       (or (p (car l))
           (exists? p (cdr l)))))

(define (exists-prime? l)
  (exists? is-prime? l))

(define (prime-in-each-column? matrix)
  (for-all-columns? exists-prime? matrix))

(prime-in-each-column? '((1 2 3) (4 5 6) (42 8 9))) ; -> #f
(prime-in-each-column? '((17 2 16) (4 5 3))) ; -> #t

;2.8
(define (mul-mat-vec a v)
    (map (lambda (a-row) (apply + (map * a-row v)))
         a))

;(define (multiply a b)
;  (transpose (map (lambda (b-col) (mul-mat-vec a b-col))
;                  (transpose b))))


(define (multiply a b)
  (map (lambda (a-row)
         (apply map (lambda b-col
                      (apply + (map * a-row b-col)))
                b))
       a))

(multiply '((2)) '((21)))
(multiply '((1) (2)) '((21)))
(multiply '((0 21)) '((1) (2)))
(multiply '((1 2 3) (3 2 1) (1 2 3)) '((4 5 6) (6 5 4) (4 6 5)))

;2.9
;???




