#lang racket

(define (sum numbers)
  (cond
    [(empty? numbers) 0]
    [ else
      (+ (first numbers) (sum (rest numbers)))]))

(define (sum2 numbers)
  (define (iter numbers r)
    (cond
      [(empty? numbers) r]
      [ else
        (iter (rest numbers) (+ r (first numbers)))]))
  (iter numbers 0))

(define (member? elem numbers)
  (cond
    [(empty? numbers) #f]
    [else
     (if (equal? (first numbers) elem)
         #t
         (member? elem (rest numbers)))]))

(define (length2 items)
  (define (iter n items)
    (cond
      [(empty? items) n]
      [else
       (iter (+ n 1) (rest items))]))
  (iter 0 items))

(define (list-ref2 items n)
  (cond
    [(zero? n) (first items)]
    [else
     (list-ref2 (rest items) (- n 1))]))

(define (range2 a b)
  (cond
    [(> a b) (list)]
    [else
     (cons a (range2 (add1 a) b))]))

(define (build-list2 n f)
  (cond
    [(= (- n 1) 0) (list (f (- n 1)))]
    [else
     (append (build-list2 (- n 1) f) (list (f (- n 1))))]))

(define (range2f f a b)
  (cond
    [(= a b) (list (f a))]
    [else
     (cons (f a) (range2f f (add1 a) b))]))


(define (append2 l1 l2)
  (define (helper xs ys)
    (cond
      [(empty? xs) ys]
      [else
       (cons (car xs)
             (helper (cdr xs) ys))]))
  (helper l1 l2))

(define (reverse2 items)
  (if (empty? items)
      (list)
      (append (reverse2 (cdr items)) (list(car items)))))

(define (take2 n items)
  (cond
    [(> n (length2 items)) items]
    [else
     (if (equal? n 0)
      (list)
      (append (list(car items)) (take2 (sub1 n)(cdr items))))]))

(define (drop2 n items)
  (cond
    [(> n (length2 items)) (list)]
    [else
     (define (helper x new)
       (if (equal? x n)
           new
           (helper (add1 x)(rest new))))
     (helper 0 items)]))

(define (take-while2 p items)
  (define (helper x new)
    (cond
      [(or (= x (length items)) (not (p (list-ref items x)))) new]
      [(p (list-ref items x)) (helper (+ x 1) (cons (list-ref items x) new))]))
  (reverse2 (helper 0 '())))

(define (drop-while2 p items)
  (define (helper new)
  (cond
    [(or
      (not(p (first new)))
      (empty? new))
      new]
    [else (helper (rest new))]))
  (helper items))


(define (numbers->list numbers)
  (cond
    [(zero? numbers) (list)]
    [else (append (numbers->list (quotient numbers 10)) (list (remainder numbers 10)))]))

(define (list->number xs)
  (define (helper xs2 new)
    (cond
    [(empty? xs2) new]
    [else (helper (rest xs2) (+ (* new 10) (first xs2)))]))
  (helper xs 0))
