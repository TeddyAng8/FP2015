(define (f p g h)
  (lambda (x)
    (and
     (p(g x))
     (p(h x)))))

(define (fst pair)
  (car pair))

(define (snd pair)
  (cdr pair))

(define (add-fract fract1 fract2)
  (if (= (snd fract1) (snd fract2))
      (cons (+ (fst fract1) (fst fract2)) (snd fract1))
      (cons
       (+
        (* (fst fract1) (snd fract2))
        (* (fst fract2) (snd fract1)))
       (* (snd fract1) (snd fract2)))))

(define (substract-fract fract1 fract2)
  (if (= (snd fract1)(snd fract2))
      (cons (- (fst fract1) (fst fract2)) (snd fract1))
      (cons
       (-
        (* (fst fract1) (snd fract2))
        (* (fst fract2) (snd fract1)))
       (* (snd fract1) (snd fract2))))) 

(define (mult-fract fract1 fract2)
  (cons (* (fst fract1) (fst fract2))
        (* (snd fract1) (snd fract2))))

(define (simplify-fract fract)
  (cons (/ (fst fract) (gcd (fst fract) (snd fract)))
        (/ (snd fract) (gcd (fst fract) (snd fract)))))
   
(define a (cons 1 2))
(define b (cons 2 3))
