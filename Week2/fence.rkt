#lang racket

(define (string-repeat str n)
    (define (helper i)
    (cond
      [(>= i n) str]
      [ else
        (string-append str (helper (add1 i)))]
      ))
  (helper 1))

(define (fence n)
  (string-append
   (string-append "{" (string-append
                       (string-append
                        (string-repeat "-" (round (+ 1 (log n))))
                        (string-append ">" (string-append (number->string n) "<")))
                       (string-repeat "-" (round (+ 1 (log n)))))) "}" ))
