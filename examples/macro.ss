; basic variable-arity or defined as a lambda which rewrites the AST
(define-macro (or-ar . args)
  (if (null? args)
    #f
    (if (null? (cdr args))
      (car args)
        `(let ((temp ,(car args)))
          (if temp
          temp
          (or-ar ,@(cdr args)))))))

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
