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
(define (every? p l)
  (or (null? l)
      (and (p (car l))
           (every? p (cdr l)))))

(define (any? p l)
  (and (not (null? l))
       (or (p (car l))
           (any? p (cdr l)))))

(define (count-columns matrix)
  (define (subset? column row)
    (every? (lambda (x)
              (member x row))
            column))

  (define (subset-of-row? column)
    (any? (lambda (row)
            (subset? column row))
          matrix))

  (length (filter subset-of-row?
                  (transpose matrix))))

;3
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (cadr t))
           (tree? (caddr t)))))

(define empty-tree '())
(define (make-tree root left right)
  (list root left right))
(define (leaf root)
  (make-tree root empty-tree empty-tree))

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)
(define (leaf? tree)
  (and (not (empty-tree? tree))
       (empty-tree? (left-tree tree))
       (empty-tree? (right-tree tree))))

(define tree
  (make-tree 1
             (make-tree 2
                        (leaf 4)
                        (leaf 5))
             (leaf 3)))

;3.1
(define (pre-order tree)
  (if (empty-tree? tree)
      empty-tree
      (cons (root-tree tree)
            (append (pre-order (left-tree tree))
                    (pre-order (right-tree tree))))))

(define (in-order tree)
  (if (empty-tree? tree)
      empty-tree
      (append (in-order (left-tree tree))
              (list (root-tree tree))
              (in-order (right-tree tree)))))

(define (post-order tree)
  (if (empty-tree? tree)
      empty-tree
      (append (post-order (left-tree tree))
              (post-order (right-tree tree))
              (list (root-tree tree)))))

(pre-order tree); => '(1 2 4 5 3))
(in-order tree); => '(4 2 5 1 3))
(post-order tree); => '(4 5 2 3 1))

;3.2
(define (level n tree)
  (cond ((empty-tree? tree) '())
        ((= n 0) (list (root-tree tree)))
        (else (append (level (- n 1) (left-tree tree))
                      (level (- n 1) (right-tree tree))))))

(level 0 (leaf 42)); => '(42))
(level 0 tree); => '(1))
(level 1 tree); => '(2 3))
(level 2 tree); => '(4 5))

;3.3
(define (count-leaves tree)
  (cond ((empty-tree? tree) 0)
        ((leaf? tree) 1)
        (else (+ (count-leaves (right-tree tree))
                 (count-leaves (left-tree tree))))))

(count-leaves empty-tree); => 0)
(count-leaves (leaf 1)); => 1)
(count-leaves tree); => 3)

;3.4
(define (map-tree fn tree)
  (if (empty-tree? tree)
      empty-tree
      (make-tree (fn (root-tree tree))
                 (map-tree fn (left-tree tree))
                 (map-tree fn (right-tree tree)))))

(define squared-tree
  (make-tree 1
             (make-tree 4
                        (leaf 16)
                        (leaf 25))
             (leaf 9)))

(define cubed-tree
  (make-tree 1
             (make-tree 8
                        (leaf 64)
                        (leaf 125))
             (leaf 27)))

(define (cube x) (* x x x))

(map-tree square empty-tree); => empty-tree)
(map-tree square (leaf 4)); => (leaf 16))
(map-tree square tree); => squared-tree)
(map-tree cube tree); => cubed-tree)

;3.5
(define (binary-heap? tree)
  (or (empty-tree? tree)
      (and (or (empty-tree? (left-tree tree))
               (< (root-tree tree)
                  (root-tree (left-tree tree))))
           (or (empty-tree? (right-tree tree))
               (< (root-tree tree)
                  (root-tree (right-tree tree))))
           (binary-heap? (left-tree tree))
           (binary-heap? (right-tree tree)))))

(define binary-heap
  (make-tree 1
             (make-tree 2
                        (leaf 4)
                        (leaf 5))
             (leaf 42)))

(define !binary-heap
  (make-tree 1
             (make-tree 2
                        (leaf 0)
                        (leaf 5))
             (leaf 3)))

(binary-heap? empty-tree); => #t)
(binary-heap? (leaf 42)); => #t)
(binary-heap? binary-heap); => #t)
(binary-heap? !binary-heap); => #f)

;3.6
(define (height tree)
  (if (empty-tree? tree)
      0
      (+ 1
         (max (height (left-tree tree))
              (height (right-tree tree))))))

(define (balanced? tree)
  (or (empty-tree? tree)
      (and (< (abs (- (height (left-tree tree))
                 (height (right-tree tree))))
              2)
           (balanced? (left-tree tree))
           (balanced? (right-tree tree)))))

(define balanced-tree
  (make-tree 1
             (make-tree 2
                        empty-tree
                        (leaf 5))
             (leaf 42)))

(define !balanced-tree
  (make-tree 1
             (make-tree 2
                        (make-tree 42
                                   (leaf 1337)
                                   empty-tree)
                        (leaf 5))
             (leaf 3)))

(balanced? empty-tree); => #t)
(balanced? (leaf 42)); => #t)
(balanced? balanced-tree); => #t)
(balanced? !balanced-tree); => #f)
           
