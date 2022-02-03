(import (chicken random))
(import (chicken time))

(define (square x)
  (* x x))

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

; 1.18

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

; 1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; 1.21

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
           (square (expmod base (/ exp 2) m))
           m))
        (else
          (remainder
            (* base (expmod base (- exp 1) m))
            m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (pseudo-random-integer (- n 1)))))

(define (fast-prime? times n)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? (- times 1) n))
        (else #f)))

(define (fast-prime-thousand? n)
  (fast-prime? 1000 n))

(define (fast-prime-ten? n)
  (fast-prime? 10 n))

; 1.22

(define (timed-prime-test n prime-test-fun)
  (start-prime-test n (current-milliseconds) prime-test-fun))

(define (start-prime-test n start-time prime-test-fun)
  (if (prime-test-fun n)
      (report-prime n (- (current-milliseconds) start-time))))

(define (report-prime n elapsed-time)
  (print " *** ")
  (print n)
  (print elapsed-time))

(define (search-for-primes from to prime-test-fun)
  (define (iter n)
    (cond ((< n to)
           (timed-prime-test n prime-test-fun)
           (iter (+ n 1)))))
  (iter from))

(define (test-hundred-for-primality from prime-test-fun)
  (search-for-primes from (+ from 100) prime-test-fun))
