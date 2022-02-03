(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

; 1.3
(define (two-larger-squares x y z)
  (cond ((and (< x y) (< x z)) (sum-of-squares y z))
        ((and (< y x) (< y z)) (sum-of-squares x z))
        (else (sum-of-squares x y))))

; Newton square root thing 
(define (sqrt x)
  (define (sqrt-iter guess)
    ;(print guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (average x y)
    (/ (+ x y) 2))

  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))

  (sqrt-iter 1.0))

;; Evi il
;(define (new-if predicate then-clause else-clause)
;  (cond (predicate then-clause)
;        (else else-clause)))
;
;(define (sqrt-iter guess x)
;  (new-if (good-enough? guess x)
;          guess
;          (sqrt-iter (improve guess x) x)))


; 1.7
(define (cube x)
  (* x x x))

(define (cube-root x)
  (define (cube-iter guess)
    ;(print guess)
    (if (good-enough? guess)
        guess
        (cube-iter (improve guess))))

  (define (improve guess)
    (/ (+ (/ x (* guess guess))
          (* 2 guess))
       3))

  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))
  (cube-iter 1.0))
