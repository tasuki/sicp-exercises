(define (square x) (* x x))

(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (divides? a b) (= (remainder b a) 0))

(define (identity x) x)

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (smallest-divisor n) (find-divisor n 2))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; 1.30 iterative sum

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum-squares a b)
  (sum square a inc b))

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

; 1.29 integrals

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (yk k)
    (f (+ a (* k h))))
  (define (add-sum k)
    (* (yk k)
       (cond ((or (= k 0) (= k n)) 1)
             ((divides? 2 k) 2)
             (else 4))))
  (* (/ h 3) (sum add-sum 0 inc n)))

; 1.31 product

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-term n)
  (if (even? n)
      (/ (+ n 2) (+ n 1)) 
      (/ (+ n 1) (+ n 2))))
; (* (product pi-term 1.0 inc 1000) 4)

; 1.32 accumulate

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (prod-acc term a next b)
  (accumulate * 1 term a next b))

; 1.33

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

(define (filtered-accumulate-elegant filter combiner null-value term a next b)
  (define (filtered x)
    (if (filter x) (term x) null-value))
  (accumulate combiner null-value filtered a next b))

(define (sum-of-prime-squares a b)
  (filtered-accumulate prime? + 0 square a inc b))

; 1.40

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))
;(newtons-method (cubic -4 10 6) 1)

; 1.41

(define (double f)
  (lambda (x)
    (f (f x))))

; 1.42

(define (compose f g)
  (lambda (x)
    (f (g x))))

; 1.43

(define (repeated f n)
  (if (> n 1)
      (repeated (double f) (- n 1))
      f))

; 1.44

(define (smooth f dx)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))
; ((smooth square 1.0) 2)

(define (n-fold-smoothed n f dx)
  (repeated (smooth f dx) n))

; 1.46

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))

(define (sqrt-ii x)
  (iterative-improve
    (lambda (guess)
      (< (abs (- (square guess) x)) 0.001))
    (lambda (guess)
      (/ (+ guess (/ x guess)) 2))))
