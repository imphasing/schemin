(define square (lambda (x) (* x x)))
(define squarelist (lambda (l) (map square l)))
(squarelist (quote (1 2)))

((lambda (l) (map (lambda (x) (* x x)) l)) '(100 200 300 400))

(define ifcarseq (lambda (list1 list2 fin) (if (= (car list1) (car list2)) (begin (display "true") fin) (begin (display "false") fin))))

(let countdown ((i 10)) (if (= i 0) "liftoff" (begin (display i)(newline) (countdown (- i 1)))))


(define fac (lambda (n) 
   (if (= 0 n) 
       1 
       (* n (fac (- n 1))))))


(define fib (lambda (n) 
	(if (< n 2) 
		n 
		(+ (fib (- n 1)) (fib (- n 2))))))

(define (fib-clever n)
  (define (fib-aux a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-aux a
                    b
                    (+ (* p p) (* q q))
                    (+ (* q q) (* 2 p q))
                    (/ count 2)))
          (else
           (fib-aux (+ (* b q) (* a q) (* a p))
                    (+ (* b p) (* a q))
                    p
                    q
                    (- count 1)))))
  (fib-aux 1 0 0 1 n))


(let ((x 10) (y 20)) (+ x y))

(define x (lambda (x y) (x y 1)))
(x (lambda (x y) (+ x y)) 2)

(map (lambda (number) (+ 1 number)) (quote (1 2 3 4)))

(foldr cons (quote ()) (quote (1 2 3 4)))

(let loop ((i 0))
  (if (not (= i 10))
     (begin
        (display i)(display " squared = ")(display (* i i))(newline)
        (loop (+ i 1)))))

(letrec ((factorial
	(lambda (n)
		(if (not (= n 0)) (* n (factorial (- n 1)))
			1))))
	(factorial 20))


(letrec ((is-even? (lambda (n)
                       (or (= n 0)
                           (is-odd? (- n 1)))))
           (is-odd? (lambda (n)
                      (and (not (= n 0))
                           (is-even? (- n 1))))))
    (is-odd? 10))


(letrec
  ((len (lambda (ls)
		  (if (null? ls) 0
			  (+ 1 (len (cdr ls)))))))
  (len '(1 2 3 5 6 7 8 9)))
