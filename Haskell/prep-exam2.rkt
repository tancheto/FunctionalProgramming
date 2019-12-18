#lang racket
;2016-02

(define (map-lenghts l)
  (map (lambda (x)
         (string-length x))
         l))

(define (unique l)
  (if (null? l)
      '()
      (cons (car l) (unique (filter (lambda (x)
                                      (not (equal? x (car l))))
                                    (cdr l))))))

(define (histogram l)
  (if (null? l)
      '()
      (map (lambda (x)
             (cons (length (filter (lambda (y)
                                     (equal? y x))
                                   l))
                   x))
           (unique l))))

(define (group l)
  (if (null? l)
      '()
      (cons (cons (car (car l)) (map cdr (filter (lambda (x)
                                             (equal? (car x) (car (car l))))
                                           l)))
            (group (filter (lambda (x)
                            (not (equal? (car x) (car (car l)))))
                          l)))))

(define (car< a b)
  (if (< (car a) (car b))
      a
      b))

(define (min-car l)
  (foldr car<
         (car l)
         l))

(define (sort-by-car l)
  (if (null? l)
      '()
      (cons (min-car l) (sort-by-car (filter (lambda (x)
                                               (not (equal? x
                                                            (min-car l))))
                                             l)))))

(define (flatmap f l)
  (apply append (map f l)))

(define (exclude l)
  (flatmap (lambda (x)
         (cdr (map list x)))
         l))

(define (lfsort l)
  (flatmap (lambda (n)
             (filter (lambda (str)
                       (= (car n) (string-length str)))
                     l))
           (exclude (sort-by-car (group (histogram (map-lenghts l)))))))

(lfsort '("abc" "de" "fgh" "fp" "ijkl" "mn" "o"))

; [trees]

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
;---------------------------------------

;8 ---> test 1 prep

(define (nodes-alike? a b)
  (and (not (empty-tree? a))
       (not (empty-tree? b))
       (equal? (root-tree a) (root-tree b))
       (equal? (root-tree (left-tree a)) (root-tree (left-tree b)))
       (equal? (root-tree (right-tree a)) (root-tree (right-tree b)))))

(define (nodes-exist? a tree)
  (and (not (empty-tree? a))
       (not (empty-tree? tree))
       (or (nodes-alike? a tree)
           (nodes-exist? a (left-tree tree))
           (nodes-exist? a (right-tree tree)))))

(define (families-alike? tree)
  (and (not (empty-tree? tree))
       (not (leaf? tree))
       (or (nodes-exist? (left-tree tree) tree)
           (nodes-exist? (right-tree tree) tree)
           (families-alike? (left-tree tree))
           (families-alike? (right-tree tree)))))

(define my-tree (make-tree 1
                           (make-tree 3
                                      (leaf 4)
                                      (leaf 5))
                           (make-tree 2
                                      (leaf 6)
                                      (leaf 1))))
