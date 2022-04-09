; 2.1

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (make-rat n d)
  (define (normalize n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (cond ((< d 0) (normalize (- n) (- d)))
        (else (normalize n d))))

; 2.2

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (avg x y) (/ (+ x y) 2))
(define (midpoint-segment segment)
  (make-point
    (avg (x-point (start-segment segment)) (x-point (end-segment segment)))
    (avg (y-point (start-segment segment)) (y-point (end-segment segment)))))

; 2.3

(define (diff x y) (abs (- x y)))
(define (make-rectangle top-left bottom-right) (cons top-left bottom-right))
(define (top-left rect) (car rect))
(define (bottom-right rect) (cdr rect))
(define (perimeter-rectangle rect)
  (+ (* 2 (diff (x-point (top-left rect)) (x-point (bottom-right rect))))
     (* 2 (diff (y-point (top-left rect)) (y-point (bottom-right rect))))))
(define (area-rectangle rect)
  (* (diff (x-point (top-left rect)) (x-point (bottom-right rect)))
     (diff (y-point (top-left rect)) (y-point (bottom-right rect)))))

; 2.6

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add a b) ((a add-1) b)) 
