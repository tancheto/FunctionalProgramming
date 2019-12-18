#lang racket

(define (adj graph vertex)
  (if (equal? (car (car graph))
              vertex)
      (cddar graph)
      (adj (cdr graph) vertex)))

(define (degrees+ graph vertex)
  (define (iter graph number)
    (cond ((null? graph) number)
          ((member vertex (cddar graph))
           (iter (cdr graph) (+ 1 number)))
          (else (iter (cdr graph) number))))

  (iter graph 0))

(define (degrees- graph vertex)
  (length (adj graph vertex)))

(define (take-all-vertexes graph)
  (map car graph))

(define (euler-cycle-exists? graph)
  (define vertexes-in-graph (take-all-vertexes graph))

  (define (helper-func vertexes)
    (or (null? vertexes)
        (and (= (degrees+ graph (car vertexes))
                (degrees- graph (car vertexes)))
             (helper-func (cdr vertexes)))))

  (helper-func vertexes-in-graph))

(define G '((a 2 b c d)
            (b 4 a c)
            (c 1 a b)
            (d 3 a)))

(define G2 '((a 2 b c)
             (b 4 a c)
             (c 1 a b)))

(define (find-cost vertex graph)
  (if (equal? vertex
              (caar graph))
      (cadar graph)
      (find-cost vertex (cdr graph))))

(euler-cycle-exists? G)

(define (find-cost-euler graph)
  (if (euler-cycle-exists? graph)
      (foldr + 0 (map (lambda (v)
                        (* (find-cost v graph)
                           (degrees+ graph v)))
                      (take-all-vertexes graph)))
      0))
  
(find-cost-euler G)
(find-cost-euler G2)

;2017

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

(define (transform-sum tree)
  (define (sum-tree tree)
    (cond ((empty-tree? tree) 0)
          ((leaf? tree) (root-tree tree))
          (else (+ (sum-tree (left-tree tree))
                   (sum-tree (right-tree tree))))))

  (if (or (empty-tree? tree)
          (leaf? tree))
      tree
      (make-tree (sum-tree tree)
                 (transform-sum (left-tree tree))
                 (transform-sum (right-tree tree)))))

(define T (make-tree 1
                     (make-tree 2
                                (make-tree 4
                                           (leaf 4)
                                           empty-tree)
                                empty-tree)
                     (make-tree 3
                                (leaf 1)
                                empty-tree)))

(transform-sum T)
  
