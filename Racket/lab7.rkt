#lang racket
;1

(define (make-alist f keys)
  (map (lambda (key)
         (cons key (f key)))
       keys))

(define (keys alist)
  (map car alist))
(define (values alist)
  (map cdr alist))
; (assoc key alist), (assv key alist), (assq key alist)

(define (del-assoc key alist)
  (filter (lambda (kv)
            (not (equal? (car kv) key)))
          alist))

(define (add-assoc key value alist)
  (cons (cons key value)
        (del-assoc key alist)))

;1.1
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

(define (run-length-encode l)
  (if (null? l)
      l
      (cons (cons (car l)
                  (length (take-while (lambda (x)
                                          (= x (car l)))
                                        l)))
            (run-length-encode (drop-while (lambda (x)
                                             (= x (car l)))
                                           l)))))

(run-length-encode '(8 7 7 2 2 2 2 3 3 2)) ; -> ((8 . 1) (7 . 2) (2 . 4) (3 . 2) (2 . 1))

;1.2
(define (repeat x n)
  (if (= n 0)
      '()
      (cons x (repeat  x (- n 1)))))

(define (flatmap f l)
  (apply append (map f l)))

(define (run-length-decode code)
  (flatmap (lambda (kv) (repeat (car kv) (cdr kv)))
       code))

(run-length-decode '((1 . 2) (3 . 4) (5 . 2))) ; -> (1 1 3 3 3 3 5 5)

;1.3
(define (count-occurencies x l)
  (length (filter (lambda (y)
                    (equal? y x))
                  l)))

(define (histogram l)
  (if (null? l)
      '()
      (cons (cons (car l)
                  (count-occurencies (car l) l))
            (histogram (filter (lambda (y)
                                 (not (equal? (car l) y)))
                               l)))))

(histogram '(8 7 1 7 8 2 2 8 2 7 8 1)) ; -> ((8 . 4) (7 . 3) (1 . 2) (2 . 3))

(define (unique l)
  (if (null? l)
      '()
      (cons (car l)
            (unique (filter (lambda (x)
                              (not (equal? (car l) x)))
                            (cdr l))))))

(define (histogram-2 l)
  (make-alist (lambda (key)
                (count-occurencies key l))
              (unique l)))
    

(histogram-2 '(8 7 1 7 8 2 2 8 2 7 8 1)) ; -> ((8 . 4) (7 . 3) (1 . 2) (2 . 3))

;1.4
(define (group-by f l)
  (make-alist (lambda (key)
                (filter (lambda (x)
                          (equal? (f x) key))
                        l))
              (unique (map f l))))

(group-by (lambda (x) (remainder x 3))
          '(0 1 2 3 4 5 6 7 8)) ; -> ((0 0 3 6) (1 1 4 7) (2 2 5 8))



