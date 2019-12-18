#lang racket

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

;---------------------------------------------------------

(define (is-progression? a b c)
  (= (- b a)
     (- c b)))

(define (sequence n)
  (define (seq n cur)
    (if (< n cur)
        '()
        (cons cur (seq n (+ 1 cur)))))

  (seq n 1))

(define (exists-k as n x)
  (define ks (sequence (quotient n 2)))
  
  (define (helper ks)
    (cond ((null? ks) #f)
          ((is-progression? (list-ref as (- n
                                            (* 2
                                               (car ks))
                                            1))
                            (list-ref as (- n
                                            (car ks)
                                            1))
                            x) #t)
          (else (helper (cdr ks)))))

  (helper ks))

(define (does-not-exist-k as n x)
  (not (exist as n x)))

(define (find-next-a as)
  (define n (length as))
  
  (define (find-help as n cur)
    (if (does-not-exist-k as n cur)
        cur
        (find-help as n (+ 1 cur))))

  (find-help as n 1))

(define (forest-fire)
  (cons-stream 1 (find-next-a (forest-fire))))



