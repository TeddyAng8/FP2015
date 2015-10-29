#lang racket

(define (series a b n)
    (define (helper a b i)
    (if (> i n)
        b
        (helper b (+ a b) (+ i 1))))
  (helper a b 3))

(define (fibonacci  n)
  (if (= n 1)
  1
  (series 1 1 n)))

(define (lucas n)
  (cond
    [(= n 1) 2]
    [(= n 2) 1]
    [else
  (series 2 1 n)]))

(define (summed-member n)
  (+ (fibonacci  n) (lucas n) ))

(define (nth-lucas-sum n)
  (define (helper i result)
    (cond
      [(> i n) result]
      [(= n 1) 2]
      [(= n 2) 3]
      [ else
        (+ (lucas i) (helper (add1 i) result))]
      ))
  (helper 1 0))

(define (nth-fibonacci-sum n)
  (define (helper i result)
    (cond
      [(> i n) result]
      [(= n 1) 1]
      [(= n 2) 2]
      [ else
        (+ (fibonacci  i) (helper (add1 i) result))]
      ))
  (helper 1 0))

(define (lucas-fib-diff n)
   (- (lucas n)  (fibonacci  n)))
