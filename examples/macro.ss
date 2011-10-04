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


; this is broken. temp-name is the same *C#* object in the if statement, which means it quotes the 2nd argument
; by quoting the second argument, we also quote the 1st argument since they're the same obj, which always evaluates to true
; since a quoted datum's bool value is always true.

(define-rewriter or-2 (lambda (first-arg second-arg)
  (let ((temp-name (gensym))) 
    `(let ((,temp-name ,first-arg))
      (if ,temp-name
        ,temp-name
        ,second-arg)))))

; sample swap using gensym
(define-macro (cl-swap! var1 var2) 
   (let ((value-symbol (gensym))) 
     `(let ((,value-symbol ,var1)) 
        (set! ,var1 ,var2) 
        (set! ,var2 ,value-symbol)))) 

; simple rewriter that just makes a list of the arguments given
(define-macro (simple . args) `(list ,@args))

(define simple-results (simple 1 2 3))

(define should-display-results (or-ar #f #f ((lambda x (begin (display "should display") #t)))))
(define should-not-display-results (or-ar #t ((lambda x (begin (display "should not display") #t)))))

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
