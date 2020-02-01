;;; Test cases for Scheme.
;;;
;;; In order to run only a prefix of these examples, add the line
;;;
;;; (exit)
;;;
;;; after the last test you wish to run.

;;; ********************************
;;; *** Add your own tests here! ***
;;; ********************************
; BEGIN PROBLEM 0
(+ 12 8)
; expect 20

(/ 9 0)
; expect Error

(/ (+ 12 11) 0)
; expect Error

(/ (+ 19 8) 3)
; expect 9

(* (/ (+ 9 12) 7) 3)
; expect 9

(* (/ (+ 3 7 9 2) 2) 5)
; expect 52.5

(+ 6 12 10 (/ 9 2) 35)
; expect 67.5

(define test (/ 3 0))
; expect Error

(eval (define test (/ 28 0)))
; expect Error

(define test (- 20 19))
; expect test

(eval (define test 2019))
; expect 2019

(eval (define test (/ (- 2019 19 ) 50)))
; expect 40

(define y 50)
; expect y

((define y (* y 19)) 5)
; expect Error

(eval (cons 'car '('(5 9))))
; expect 5

y
; expect 950

(car '(1 2))
; expect 1

(quote (2 0 1 9))
; expect (2 0 1 9)

(begin (- 14 9) (* 4 16))
; expect 64

(begin (* 14 291) (print 2000) '(/ 1491 12))
; expect (/ 1491 12)

(define (f x y) (* y x))
; expect f

f
; expect (lambda (x y) (* y x))

(lambda (x y z) (+ x y z))
; expect (lambda (x y z) (+ x y z))

(define f (lambda (x y z) (+ x y z)))
; expect f

(f 2000 10 9)
; expect 2019

(and 2 0 1 9)
; expect 9

(or 9 5)
; expect 9

(and 2 0 (+ 1 9))
; expect 10

(and #f #t (- 19 9))
; expect #f

(or #f (+ (* 500 4) 19))
; expect 2019

(and)
; expect #t

(or)
; expect #f

(cond ((= 61 (+ 60 1)) 'cs61a)
      ((= 55 55) 'is)
      (else 'sometimes tough)
      )
; expect cs61a

(cond ((= 61 61))
      ((= 2019 (+ (* 50 40) 19)))
      (else 'no)
      )
; expect #t

(define x 5500)
; expect x

(define y 'cs61a)
; expect y

(let ((x 'cs)
      (y 'rocks))
      (list x y))
; expect (cs rocks)

(list x y)
; expect (5500 cs61a)

(define x (mu () (* a b c d e)))
; expect x

(define f (lambda () (define a 1) (define b 2) (define c 3) (define d 4) (define e 5) (x)))
; expect f

(f)
; expect 120

(define (enumerate s)
 (define (helper s k)
  (cond
        ((null? s) nil)
        (else (cons (list k (car s)) (helper (cdr s) (+ k 1))))))
 (helper s 0))

(enumerate '())
; expect ()

(enumerate '(5 6 7 8 9))
; expect ((0 5) (1 6) (2 7) (3 8) (4 9))

(define (cons-all first rests)
  (map (lambda (x) (cons first x)) rests))

(define (partitions-list sum change)
  (cond ((null? change) '())
        ((= sum 0) '(()))
        ((< sum 0) '())
        ((> (car change) sum) (partitions-list sum (cdr change)))
        (else (append (cons-all (car change) (partitions-list (- sum (car change)) change)) (partitions-list sum (cdr change))))))

(partitions-list 0 '(3 2 1))
; expect (())

(partitions-list 5 '(3 2 1))
; expect ((3 2) (3 1 1) (2 2 1) (2 1 1 1) (1 1 1 1 1))

(partitions-list 10 '(5 1))
; expect ((5 5) (5 1 1 1 1 1) (1 1 1 1 1 1 1 1 1 1))

(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (zip pairs)
  (list (map (lambda (x) (car x)) pairs) (map (lambda (x) (cadr x)) pairs)))

(define (let-to-lambda expr)
  (cond ((atom? expr)
         expr
         )
        ((quoted? expr)
         expr
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           (cons form (cons params (let-to-lambda body)))
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr))) 
           (cons (cons 'lambda (cons (car (zip (let-to-lambda values)))
             (let-to-lambda body)))
             (cadr (zip (let-to-lambda values)))) 
           ))
        (else
         (map let-to-lambda expr)
         ; 
         )))


(let-to-lambda '(* 2 0 1 9))
; expect (* 2 0 1 9)

(let-to-lambda '(let ((x (- 1 2)) (y 3) (* x y))))
; expect ((lambda (x y *)) (- 1 2) 3 x)

(zip '((2 0) (1 9) (6 1)))
; expect((2 1 6) (0 9 1))

(zip '((6 1)))
; expect((6) (1))

(zip '())
; expect(() ())

(define-macro (list-of map-expr for var in lst if filter-expr)
  `(map (lambda (,var) ,map-expr) (filter (lambda (,var) ,filter-expr) ,lst))
)
; expect list-of

(list-of (* y y) for y in '(1 2 3 4 5 6 7) if (odd? y))
; (1 9 25 49)
; END PROBLEM 0

;;; These are examples from several sections of "The Structure
;;; and Interpretation of Computer Programs" by Abelson and Sussman.

;;; License: Creative Commons share alike with attribution

;;; 1.1.1

10
; expect 10

(+ 137 349)
; expect 486

(- 1000 334)
; expect 666

(* 5 99)
; expect 495

(/ 10 5)
; expect 2

(+ 2.7 10)
; expect 12.7

(+ 21 35 12 7)
; expect 75

(* 25 4 12)
; expect 1200

(+ (* 3 5) (- 10 6))
; expect 19

(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))
; expect 57

(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))
; expect 57

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Move the following (exit) line down the file to run additional tests. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(exit)


;;; 1.1.2

(define size 2)
; expect size
size
; expect 2

(* 5 size)
; expect 10

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
; expect 314.159

(define circumference (* 2 pi radius))
circumference
; expect 62.8318

;;; 1.1.4

(define (square x) (* x x))
; expect square
(square 21)
; expect 441

(define square (lambda (x) (* x x))) ; See Section 1.3.2
(square 21)
; expect 441

(square (+ 2 5))
; expect 49

(square (square 3))
; expect 81

(define (sum-of-squares x y)
  (+ (square x) (square y)))
(sum-of-squares 3 4)
; expect 25

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(f 5)
; expect 136

;;; 1.1.6

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(abs -3)
; expect 3

(abs 0)
; expect 0

(abs 3)
; expect 3

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 3 -2)
; expect 5

;;; 1.1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 9)
; expect 3.00009155413138

(sqrt (+ 100 37))
; expect 11.704699917758145

(sqrt (+ (sqrt 2) (sqrt 3)))
; expect 1.7739279023207892

(square (sqrt 1000))
; expect 1000.000369924366

;;; 1.1.8

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
(sqrt 9)
; expect 3.00009155413138

(sqrt (+ 100 37))
; expect 11.704699917758145

(sqrt (+ (sqrt 2) (sqrt 3)))
; expect 1.7739279023207892

(square (sqrt 1000))
; expect 1000.000369924366

;;; 1.3.1

(define (cube x) (* x x x))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(sum-cubes 1 10)
; expect 3025

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))
(sum-integers 1 10)
; expect 55

;;; 1.3.2

((lambda (x y z) (+ x y (square z))) 1 2 3)
; expect 12

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
(f 3 4)
; expect 456

(define x 5)
(+ (let ((x 3))
     (+ x (* x 10)))
   x)
; expect 38

(let ((x 3)
      (y (+ x 2)))
  (* x y))
; expect 21

;;; 2.1.1

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define x (cons 1 (cons 2 nil)))
(car x)
; expect 1

(cdr x)
; expect (2)

(define x (list 1 2))
(define y (list 3 4))
(define z (cons x y))
(car (car z))
; expect 1

(car (cdr z))
; expect 3

z
; expect ((1 2) 3 4)

(define (make-rat n d) (list n d))
(define (numer x) (car x))
(define (denom x) (car (cdr x)))
(define (print-rat x)
  (display (numer x))
  (display '/)
  (display (denom x))
  (newline))
(define one-half (make-rat 1 2))
(print-rat one-half)
; expect 1/2

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
; expect 5/6

(print-rat (mul-rat one-half one-third))
; expect 1/6

(print-rat (add-rat one-third one-third))
; expect 6/9

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))
(print-rat (add-rat one-third one-third))
; expect 2/3

(define one-through-four (list 1 2 3 4))
one-through-four
; expect (1 2 3 4)

(car one-through-four)
; expect 1

(cdr one-through-four)
; expect (2 3 4)

(car (cdr one-through-four))
; expect 2

(cons 10 one-through-four)
; expect (10 1 2 3 4)

(cons 5 one-through-four)
; expect (5 1 2 3 4)

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
(map abs (list -10 2.5 -11.6 17))
; expect (10 2.5 11.6 17)

(map (lambda (x) (* x x))
     (list 1 2 3 4))
; expect (1 4 9 16)

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))
(scale-list (list 1 2 3 4 5) 10)
; expect (10 20 30 40 50)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)
; expect 4

(count-leaves (list x x))
; expect 8

;;; 2.2.3

(define (odd? x) (= 1 (remainder x 2)))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(filter odd? (list 1 2 3 4 5))
; expect (1 3 5)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))
; expect 15

(accumulate * 1 (list 1 2 3 4 5))
; expect 120

(accumulate cons nil (list 1 2 3 4 5))
; expect (1 2 3 4 5)

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)
; expect (2 3 4 5 6 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
; expect (1 2 3 4 5)

;;; 2.3.1

(define a 1)

(define b 2)

(list a b)
; expect (1 2)

(list 'a 'b)
; expect (a b)

(list 'a b)
; expect (a 2)

(car '(a b c))
; expect a

(cdr '(a b c))
; expect (b c)

(define (memq item x)
  (cond ((null? x) #f)
        ((equal? item (car x)) x)
        (else (memq item (cdr x)))))
(memq 'apple '(pear banana prune))
; expect #f

(memq 'apple '(x (apple sauce) y apple pear))
; expect (apple pear)

(define (my-equal? x y)
  (cond ((pair? x) (and (pair? y)
                        (my-equal? (car x) (car y))
                        (my-equal? (cdr x) (cdr y))))
        ((null? x) (null? y))
        (else (equal? x y))))
(my-equal? '(1 2 (three)) '(1 2 (three)))
; expect #t

(my-equal? '(1 2 (three)) '(1 2 three))
; expect #f

(my-equal? '(1 2 three) '(1 2 (three)))
; expect #f

;;; Peter Norvig tests (http://norvig.com/lispy2.html)

(define double (lambda (x) (* 2 x)))
(double 5)
; expect 10

(define compose (lambda (f g) (lambda (x) (f (g x)))))
((compose list double) 5)
; expect (10)

(define apply-twice (lambda (f) (compose f f)))
((apply-twice double) 5)
; expect 20

((apply-twice (apply-twice double)) 5)
; expect 80

(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))
(fact 3)
; expect 6

(fact 50)
; expect 30414093201713378043612608166064768844377641568960512000000000000

(define (combine f)
  (lambda (x y)
    (if (null? x) nil
      (f (list (car x) (car y))
         ((combine f) (cdr x) (cdr y))))))
(define zip (combine cons))
(zip (list 1 2 3 4) (list 5 6 7 8))
; expect ((1 5) (2 6) (3 7) (4 8))

(define riff-shuffle (lambda (deck) (begin
    (define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq))))))
    (define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq)))))
    (define mid (lambda (seq) (/ (length seq) 2)))
    ((combine append) (take (mid deck) deck) (drop (mid deck) deck)))))
(riff-shuffle (list 1 2 3 4 5 6 7 8))
; expect (1 5 2 6 3 7 4 8)

((apply-twice riff-shuffle) (list 1 2 3 4 5 6 7 8))
; expect (1 3 5 7 2 4 6 8)

(riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8))))
; expect (1 2 3 4 5 6 7 8)

;;; Additional tests

(apply square '(2))
; expect 4

(apply + '(1 2 3 4))
; expect 10

(apply (if #f + append) '((1 2) (3 4)))
; expect (1 2 3 4)

(if 0 1 2)
; expect 1

(if '() 1 2)
; expect 1

(or #f #t)
; expect #t

(or)
; expect #f

(and)
; expect #t

(or 1 2 3)
; expect 1

(and 1 2 3)
; expect 3

(and #f (/ 1 0))
; expect #f

(and #t (/ 1 0))
; expect Error

(or 3 (/ 1 0))
; expect 3

(or #f (/ 1 0))
; expect Error

(or (quote hello) (quote world))
; expect hello

(if nil 1 2)
; expect 1

(if 0 1 2)
; expect 1

(if (or #f #f #f) 1 2)
; expect 2

(define (loop) (loop))
(cond (#f (loop))
      (12))
; expect 12

((lambda (x) (display x) (newline) x) 2)
; expect 2 ; 2

(define g (mu () x))
(define (high f x)
  (f))

(high g 2)
; expect 2

(define (print-and-square x)
  (print x)
  (square x))
(print-and-square 12)
; expect 12 ; 144

(/ 1 0)
; expect Error

(define addx (mu (x) (+ x y)))
(define add2xy (lambda (x y) (addx (+ x x))))
(add2xy 3 7)
; expect 13

(let ((x 2)) ((begin (define x (+ x 1)) +) 3 (begin (define x (+ x 1)) x)))
; expect 7

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scheme Implementations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; len outputs the length of list s
(define (len s)
  (if (eq? s '())
    0
    (+ 1 (len (cdr s)))))
(len '(1 2 3 4))
; expect 4


;;;;;;;;;;;;;;;;;;;;
;;; Extra credit ;;;
;;;;;;;;;;;;;;;;;;;;

(exit)

; Tail call optimization tests

(define (sum n sum)
  (if (zero? n) sum
    (sum (- n 1) (+ n sum))))
(sum 1001 0)
; expect 501501

(define (sum n sum)
  (cond ((zero? n) sum)
        (else (sum (- n 1) (+ n sum)))))
(sum 1001 0)
; expect 501501

(define (sum n sum)
  (begin 2 3
    (if (zero? n) sum
      (and 2 3
        (or #f
          (begin 2 3
            (let ((m n))
              (sum (- m 1) (+ m sum)))))))))
(sum 1001 0)
; expect 501501

(exit)

; macro tests

(define (map f lst)
    (if (null? lst)
        nil
        (cons
            (f (car lst))
            (map f (cdr lst)))))

(define-macro (for formal iterable body)
         (list 'map (list 'lambda (list formal) body) iterable))

(for i '(1 2 3)
    (if (= i 1)
        0
        i))
; expect (0 2 3)

(define (cadr s) (car (cdr s)))
(define (cars s) (map car s))
(define (cadrs s) (map cadr s))

(define-macro (leet bindings expr)
    (cons
        (list 'lambda (cars bindings) expr)
        (cadrs bindings)))

(define (square x) (* x x))
(define (hyp a b)
  (leet ((a2 (square a)) (b2 (square b))) (sqrt (+ a2 b2))))

(hyp 3 4)
; expect 5.000023178253949
