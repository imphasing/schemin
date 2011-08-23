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


(define bubblesort (lambda (l)
  (define swap-pass (lambda (l)
    (if (eq? (length l) 1) 
        l
        (let ((fst (car l))(snd (cadr l))(rest (cddr l)))
          (if (> fst snd) 
              (cons snd (swap-pass (cons fst rest)))
              (cons fst (swap-pass (cons snd rest))))))))
  (let for ((times (length l))
            (val l))
    (if (> times 1)
        (for (- times 1)(swap-pass val))
        (swap-pass val)))))


(let ((x 10) (y 20)) (+ x y))


(define x (lambda (x y) (x y 1)))
(x (lambda (x y) (+ x y)) 2)

(map (lambda (number) (+ 1 number)) (quote (1 2 3 4)))

(foldl cons (quote ()) (quote (1 2 3 4)))


(define quicksort (lambda (items) (if (null? items) (quote ()) (let ((pivot (car items))) (let ((less (filter (lambda (x) (< x pivot)) items)) (same (filter (lambda (x) (= x pivot)) items)) (more (filter (lambda (x) (> x pivot)) items))) (append (quicksort less) same (quicksort more)))))))


(define qs (lambda (l) (if (null? l) (quote ()) (append (qs (filter (lambda (x) (<= x (car l))) (cdr l))) (cons (car l) (qs (filter (lambda (x) (> x (car l))) (cdr l))) )))))

(qs (quote (23 5342 234 44 322 6654 44 223 44 11 3 1 23 4 1 3 55 2)))

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