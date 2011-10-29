;;;; Shamelessly stolen from: https://bitbucket.org/tarballs_are_good/scheme-random

;;;; A little sample evaluator
;;;; Copyright (c) 2011 Robert Smith

;;; ERRORS

(define (error x)
  (/ 0))


;;; BINDINGS

(define binding cons)
(define binding.key car)
(define binding.val cdr)


;;; ENVIRONMENTS

(define (env-lookup env sym)
  (let ((p (assoc sym env)))
    (if p
        (cdr p)
        (error "unbound symbol"))))

(define (env-update env key val)
  (cond
    ((null? env)
     (list (binding key val)))
    ((eq? (binding.key (car env))
          key)
     (cons (binding key val)
           (cdr env)))
    (else (cons (car env)
                (env-update (cdr env) key val)))))


;;; UTILITY

(define (atom? arg)
  (or (number? arg)
      (boolean? arg)
      (symbol? arg)))

(define special-forms '(quote if begin set!))

(define (special? x)
  (if (member x special-forms)
      #t
      #f))


;;; EVALUATOR

(define evaluation cons)
(define eval.result car)
(define eval.env cdr)

(define (evaluate form env)
  (cond
    ((null? form) (evaluation form env)) ; return NIL as-is
    ((atom? form) (if (symbol? form)
                      (evaluation (env-lookup env form) env)
                      (evaluation form env)))  ; return the value itself
    ((special? (car form))
     (case (car form)
       ((quote) (evaluation (cadr form) env))
       ((if)    (let ((predicate (evaluate (cadr form) env))
                      (conseq-true (caddr form))
                      (conseq-false (cadddr form)))
                  (if (eval.result predicate)
                      (evaluate conseq-true (eval.env predicate))
                      (evaluate conseq-false (eval.env predicate)))))
       ((begin) (evaluate-begin (cdr form) env))
       ((set!)  (let ((var (cadr form))
                      (val (evaluate (caddr form) env)))
                  (evaluation (eval.result val)
                              (env-update (eval.env val)
                                          var
                                          (eval.result val)))))))
    (else ; must be a function call
     (let* ((func (evaluate (car form) env))
            (args (evaluate-list (cdr form) (eval.env func))))
       (evaluation (call-function (eval.result func)
                                  (eval.result args))
                   (eval.env args))))))

(define (call-function func args)
  (apply func args))

(define (evaluate-list lst env)
  (let loop ((evaluated-list '())
             (unevaluated-list lst)
             (envir env))
    (if (null? unevaluated-list)
        (evaluation (reverse evaluated-list) envir)
        (let ((ev-form (evaluate (car unevaluated-list) envir)))
          (loop (cons (eval.result ev-form) evaluated-list)
                (cdr unevaluated-list)
                (eval.env ev-form))))))

(define (evaluate-begin form env)
  (let loop ((ev-form form)
             (envir env))
    (cond
      ((null? ev-form) (evaluation '() envir))
      ((null? (cdr ev-form)) (evaluate (car ev-form) envir))
      (else (loop (cdr ev-form)
                  (eval.env (evaluate (car ev-form) envir)))))))


;;; INITIAL ENVIRONMENT

(define initial-environment `(,(cons '+ +)
                              ,(cons '- -)
                              ,(cons '* *)
                              ,(cons '/ /)
                              ,(cons 'cons cons)
                              ,(cons 'car car)
                              ,(cons 'cdr cdr)
                              ,(cons 'null? null?)))


;;; ENTRY

(define (eval* expression)
  (eval.result (evaluate expression initial-environment)))

(define test-add '(+ 2 (+ 2 2) (+ 2 2)))
(define test-mult '(* 2 (* 2 2) (* 2 2)))

(display "Evaluating: ")
(display test-add)
(newline)
(display "Result: ")
(display (eval* test-add))
(newline)

(display "Evaluating: ")
(display test-mult)
(newline)
(display "Result: ")
(display (eval* test-mult))
(newline)
