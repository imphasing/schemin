;; A demonstration of Church encodings.

;; Author: Matthew Might
;; Site:   http://matt.might.net/
;;         http://www.ucombinator.org/

;; The pure lambda calculus, in Scheme, has 
;; variables, lambda expressions and applications:

;; <exp> ::= <var>
;;        |  (lambda (<var>) <exp>)
;;        |  (<exp> <exp>)

;; It is possible to reduce any expression in Scheme
;; to this language.

;; This demo shows how to represent numbers, booleans
;; lists, errors and recursion in this simple language.



;; Testing.

; print-test : a macro for printing tests.
(define-macro (print-test exp) 
  `(begin
   (write (quote ,exp))
   (display " == ")
   (write ,exp)
   (newline)
   (newline)))



;; Multi-argument functions.

;; Currying can mimic multi-argument functions
;; with only single-argument functions.

; curry2 : (A B -> C) -> (A -> B -> C)
(define curry2 (lambda (f)
                 (lambda (x)
                   (lambda (y)
                     (f x y)))))

; f : num num -> num
(define f (lambda (x y) 
            (+ x y)))

; f^C : num -> num -> num
(define f^C (curry2 f))

(print-test (f 3 4))

(print-test ((f^C 3) 4))


;; Void.

;; To mimic no-argument functions, we pass
;; void to a single-argument function.

; void : void
(define void (lambda (x) x))



;; Errors.

;; To encode errors, we use non-termination, courtesy
;; of the "Omega" term.

; error : (-> *)
(define error (lambda ()
                ((lambda (f) (f f))
                 (lambda (f) (f f)))))



;; Booleans.

;; Booleans are encoded as if expressions, where
;; the true expression invokes the true branch,
;; and the false expression invokes the false branch.

; boolean = (-> A) (-> A) -> A

; true : boolean
(define true  (lambda (on-true on-false) (on-true)))

; false : boolean
(define false (lambda (on-true on-false) (on-false)))

; if! : boolean A A -> A
(define if! (lambda (condition true-value false-value) 
              (condition 
               (lambda () true-value)
               (lambda () false-value))))

; if* : boolean (-> A) (-> A) -> A
(define if*    (lambda (condition on-true on-false)
                 (condition on-true on-false)))
(print-test
 (if* true
     (lambda () 42) 
     (lambda () 1701)))

(print-test
 (if* false
    (lambda () 42)
    (lambda () 1701)))



;; Lists.

;; Lists are encoded as a match construct on lists.

; nil : list[*]
(define nil (lambda (on-null on-pair) 
              (on-null)))

; kons : A list[A] -> list[A]
(define kons (lambda (a b)
               (lambda (on-null on-pair)
                 (on-pair a b))))

; kar : list[A] -> A
(define kar (lambda (list)
              (list (lambda () (error))
                    (lambda (a b) a))))

; kdr : list[A] -> list[A]
(define kdr (lambda (list)
              (list (lambda () (error))
                    (lambda (a b) b))))

; match-list : list[A] (-> B) (A list[A] -> B) -> B
(define match-list (lambda (list on-null on-pair)
                     (list on-null on-pair)))

; kons? : list[A] -> boolean
(define kons? (lambda (list)
                (list (lambda () #f)
                      (lambda (a b) #t))))

; nil? : list[A] -> boolean
(define nil? (lambda (list)
               (list (lambda () #t)
                     (lambda (a b) #f))))


(print-test (car (cdr (cons 3 (cons 4 '())))))

(print-test (kar (kdr (kons 3 (kons 4 nil)))))

(print-test 
 (match-list (kons 3 4)
             (lambda () void)
             (lambda (a b) a)))


;; Iterative Church numerals.

;; A Church numeral applies its first argument to its 
;; second argument n times.

;; That is, n(f)(x) == f^n(x)

; number = (A -> A) -> A -> A

; zero : number
(define zero (lambda (f) 
               (lambda (zero) 
                 zero)))

; succ : number -> number 
(define succ (lambda (n)
               (lambda (f)
                 (lambda (zero) 
                   ((n f) (f zero))))))

; one : number
(define one (succ zero))

; two : number
(define two (succ one))

; add : number number -> number
(define add (lambda (n m)
              (lambda (f)
                (lambda (zero)
                  ((n f) ((m f) zero))))))

; mul : number number -> number
(define mul (lambda (n m)
              (lambda (f)
                (lambda (zero)
                  ((m (n f)) zero)))))

; inc : num -> num
(define inc (lambda (z) (+ z 1)))

(print-test
 (((succ zero) inc) 0))

(print-test
 (((succ (succ (succ zero))) 
   (lambda (x)
     (cons 2
           x)))
  '()))

(print-test
 (((add one one)
   inc)
  0))

(print-test
 (((mul two two)
   inc)
  0))


;; Let-binding.

;; Let forms may be encoded as immediately
;; applied lambda terms.

; (let ((v1 e1) ... (vN eN)) body) ==
;  ((lambda (v1 ... vN) body) e1 ... eN)



;; Recursion.

;; Recursion is enabled through the U combinator
;; or the Y combinator.  The U combinator passes its 
;; argument to itself, while the Y combinator finds
;; the fixed point of its functional argument.


;; The U combinator passes its argument to itself, 
;; enabling self-reference.

; (U F) = (F F)

; U : untypeable
(define U (lambda (F) (F F)))

; The Y Combinator computes the fixed point of a functional:
;  (Y F) = (F (Y F))
;  Y = (lambda (F) (F (Y F)))
;    = (lambda (F) (F (lambda (x) ((Y F) x))))
;    = (U (lambda (y) (lambda (F) (F (lambda (x) (((y y) F) x))))))
;    = ((lambda (y) (lambda (F) (F (lambda (x) (((y y) F) x)))))
;       (lambda (y) (lambda (F) (F (lambda (x) (((y y) F) x))))))

; Y : ((A -> B) -> (A -> B)) -> (A -> B)
(define Y ((lambda (y) (lambda (F) (F (lambda (x) (((y y) F) x))))) 
           (lambda (y) (lambda (F) (F (lambda (x) (((y y) F) x)))))))

; U-fact : num -> num
(define U-fact (U (lambda (f)
                    (lambda (n)
                      (if (<= n 0)
                          1
                          (* n ((U f) (- n 1))))))))

; Y-fact : num -> num
(define Y-fact (Y (lambda (fact)
                    (lambda (n)
                      (if (<= n 0)
                          1
                          (* n (fact (- n 1))))))))


;; Useful functions.

; map : (A -> B) list[A] -> list[B]
(define (map f lst)
  (if (pair? lst)
      (cons (f (car lst))
            (map f (cdr lst)))
      '()))

(print-test (map (lambda (x) (+ x 1)) (list 1 2 3 4 5)))

; foldr : (A B -> B) B list[A] -> B
(define (foldr kons nil lst)
  (if (pair? lst)
      (kons (car lst)
            (foldr kons nil (cdr lst)))
      nil))

(print-test (foldr (lambda (hd tl) (+ hd tl))
                   0
                   (list 1 2 3 4 5)))

; range : num num -> list[num]
(define (range lo hi)
  (if (<= lo hi)
      (cons lo (range (+ 1 lo) hi))
      '()))

(print-test (range 1 5))

; fact : num -> num
(define fact (lambda (n) (foldr * 1 (range 1 n))))

(print-test (fact 5))
