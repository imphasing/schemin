;; Testing out macros with out own implementation of if, using lambdas as true and false

(define true
  (lambda (x y) x))

(define false
  (lambda (x y) y))

(define-rewriter if-test 
  (lambda (form rename)
    (let ((test (car (cdr form)))
          (then (car (cdr (cdr form))))
          (else (car (cdr (cdr (cdr form))))))
      `((,test (,(rename 'lambda) () ,then) (,(rename 'lambda) () ,else))))))


;; Implementation of a recursive begin macro, using lambdas to force order of evaluation

(define-rewriter mybegin
  (lambda (form rename)
    (let ((first (cadr form))
         (rest (cddr form))
	 (unused (gensym)))
      `((,(rename 'lambda) (,unused) ('mybegin ,rest)) ,first))))

 
;; Test stream implementation

(define-rewriter stream
  (lambda (form rename)
    (let ((arg1 (cadr form))
          (arg2 (caddr form)))
      `(,(rename 'cons) ,arg1 (,(rename 'lambda) () ,arg2)))))

(define stream-ref
  (lambda (s n)
    (if (zero? n)
      (car s)
      (stream-ref ((cdr s)) (- n 1)))))

(define integers
  (lambda (n)
    (stream n (integers (+ n 1)))))
