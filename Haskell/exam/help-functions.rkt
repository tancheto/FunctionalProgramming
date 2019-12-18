#lang racket

;streams-------------------------------------------

(define empty-stream '())

(define-syntax cons-stream
  (syntax-rules ()
                ((cons-stream h t)
                 (cons h (delay t)))))

(define (empty-stream? s)
  (equal? s empty-stream))

(define head car)

(define (tail s)
  (force (cdr s)))

(define (take-stream n s)
  (if (or (= n 0)
          (empty-stream? s))
      empty-stream
      (cons-stream (head s)
            (take-stream (- n 1) (tail s)))))

(define (stream->list s)
  (if (empty-stream? s)
      '()
      (cons (head s)
            (stream->list (tail s)))))

;graphs----------------------------------------

(define empty-graph '())

(define (make-graph vs)
  (make-alist (lambda (_) '())
              vs))

(define vertices keys)
(define (children v g)
  (cdr (assoc v g)))
(define (edge? u v g)
  (member v (children u g)))

(define (map-children v f g)
  (map f (children v g)))

(define (any? p l)
  (and (not (null? l))
       (or (p (car l))
           (any? p (cdr l)))))

(define (search-child v p g)
  (any? p (children v g)))

(define (add-vertex v g)
  (if (assoc v g)
      g
      (add-assoc v '() g)))

(define (add-if-missing x l)
  (if (member x l)
      l
      (cons x l)))

(define (add-edge u v g)
  (let ((g-with-u-v (add-vertex v (add-vertex u g))))
    (add-assoc u
               (add-if-missing v (children u g-with-u-v))
               g-with-u-v)))

(define (remove-edge u v g)
  (add-assoc u
             (filter (lambda (u)
                       (not (equal? u v)))
                     (children u g))
             g))

;trees-------------------------------------------------------

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