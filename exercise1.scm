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

; 1.10
(define (ackerman x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (ackerman (- x 1) (ackerman x (- y 1))))))

(define (f n) (ackerman 0 n))
(define (g n) (ackerman 1 n))
(define (h n) (ackerman 2 n))


; change

(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
;(count-change 100)


; 1.11

(define (f-rec n)
  (cond ((< n 3) n)
        (else (+ (* 1 (f-rec (- n 1)))
                 (* 2 (f-rec (- n 2)))
                 (* 3 (f-rec (- n 3)))))))

; 1:                 =  1
; 2:                 =  2
; 3: + 1* 2 2* 1 3*0 =  4
; 4: + 1* 4 2* 2 3*1 = 11
; 5: + 1*11 2* 4 3*2 = 25
; 6: + 1*25 2*11 3*4 = 59

(define (f-iter n)
  (define (helper a b c steps)
    (cond ((< n 3) n)
          ((< steps 3) a)
          (else (helper (+ a (* 2 b) (* 3 c)) a b (- steps 1)))))
  (helper 2 1 0 n))

; 1.12

(define (pascal-triangle row col)
  (cond ((= row 1) 1)
        ((= col 1) 1)
        ((= col row) 1)
        (else (+ (pascal-triangle (- row 1) (- col 1))
                 (pascal-triangle (- row 1) col)))))

; 1.16

; (fast-expt 2 10)
; (square (fast-expt 2 5))
; (square (2 * (fast-expt 2 4)))
; (square (2 * (square (fast-expt 2 2))))
; (square (2 * (square (square (fast-expt 2 1)))))
; (square (2 * (square (square (2 * (fast-expt 2 0))))))
; (square (2 * (square (square (2 * (1))))))
; (square (2 * (square (square (2)))))
; (square (2 * (square (4))))
; (square (2 * (16)))
; (square (32))
; (1024)

(define (fast-expt b n)
  ;(print b " " n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; 1 * 2^10
; 1 * (2*2)^5
; 1 * (4)^5
; 4 * (4)^4
; 4 * (4*4)^2
; 4 * (16)^2
; 4 * (16*16)^1
; 4 * (256)^1
; 1024 * (256)^0

(define (fast-expt-iter b n)
  (define (iter a b n)
    ;(print a " " b " " n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

; 1.17

(define (mul a b)
  (if (= b 0)
      0
      (+ a (mul a (- b 1)))))

(define (double a)
  (* a 2))

(define (halve a)
  (/ a 2))

; (* 3 7)
; (+ 3 (* 3 6))
; (+ 3 (double(* 3 3)))
; (+ 3 (double(+ 3 (* 3 2))))
; (+ 3 (double(+ 3 (double(* 3 1)))))
; (+ 3 (double(+ 3 (double(3)))))

(define (mul-fast a b)
  ;(print a " " b)
  (cond ((= b 0) 0)
        ((even? b) (double (mul-fast a (halve b))))
        (else (+ a (mul-fast a (- b 1))))))

; 3 * 8
; (double 3) * 4
; 6 * 4
; (double 6) * 2
; 12 * 2
; (double 12) * 1
; 24

;     (3 * 7)
; 3 + (3 * 6)
; 3 + (double 3) * 3
; 3 + 6 * 3
; 9 + 6 * 2
; 9 + (double 6)
; 9 + 12

(define (mul-fast-iter a b)
  (define (iter n a b)
    (cond ((= b 0) n)
          ((even? b) (iter n (double a) (halve b)))
          (else (iter (+ n a) a (- b 1)))))
  (iter 0 a b))
