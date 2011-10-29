; basic variable-arity or defined as a lambda which rewrites the AST.
(define-macro (or-ar . args)
  (if (null? args)
    #f
    (if (null? (cdr args))
      (car args)
      (let ((next-arg (gensym)))
       `(let ((,next-arg ,(car args)))
	  (if ,next-arg
	  ,next-arg
	  (or-ar ,@(cdr args))))))))

; sample swap using gensym
(define-macro (cl-swap! var1 var2) 
   (let ((value-symbol (gensym))) 
     `(let ((,value-symbol ,var1)) 
        (set! ,var1 ,var2) 
        (set! ,var2 ,value-symbol)))) 

; simple rewriter that just makes a list of the arguments given
(define-macro (simple . args) `(list ,@args))

(define first "first")
(define second "second")

(define simple-results (simple 1 2 3))
(define should-display-results (or-ar #f #f ((lambda x (begin (display "should display") #t)))))
(define should-not-display-results (or-ar #t ((lambda x (begin (display "should not display") #t)))))
(cl-swap! first second)

(newline)
(newline)

(display "Simple results: ")
(display simple-results)
(newline)
(display "Should not display results: ")
(display should-not-display-results)
(newline)
(display "Should display results: ")
(display should-display-results)
(newline)
(display "Swapped results: ")
(newline)
(display "First: ")
(display first)
(newline)
(display "Second: ")
(display second)
(newline)
