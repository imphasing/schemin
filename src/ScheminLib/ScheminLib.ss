;;
;; Copyright (c) 2011 Alex Fort 
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;; derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;; 


;; ScheminLib, parts of Schemin's standard library written in Schemin itself
;; List functions:

(define (cadr list) (car (cdr list)))
(define (cddr list) (cdr (cdr list)))
(define (caddr list) (car (cdr (cdr list))))

(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))

(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))

(define (filter pred lst) (foldr (lambda (x y) (if (pred x) (cons x y) y)) (quote ()) lst))

(define (map func lst) (foldr (lambda (x y) (cons (func x) y)) (quote ()) lst))

(define fold foldl)

(define reduce fold)

(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))

(define (reverse lst) (fold (flip cons) '() lst))

(define (member thing lis)
  (if (null? lis)
      #f
     (if (equal? (car lis) thing)
         lis
         (member thing (cdr lis)))))

(define (memv thing lis)
  (if (null? lis)
      #f
      (if (eqv? (car lis) thing)
          lis
          (member thing (cdr lis)))))

(define (memq thing lis)
  (if (null? lis)
      #f
      (if (eq? (car lis) thing)
          lis
          (member thing (cdr lis)))))

(define (assoc thing alist)
  (if (null? alist)
      #f
      (if (equal? (car (car alist)) thing)
          (car alist)
          (assoc thing (cdr alist)))))

(define (assq thing alist)
  (if (null? alist)
      #f
      (if (eq? (car (car alist)) thing)
          (car alist)
          (assoc thing (cdr alist)))))

(define (assv thing alist)
  (if (null? alist)
      #f
      (if (eqv? (car (car alist)) thing)
          (car alist)
          (assoc thing (cdr alist)))))

;; General functions:

(define (not x) (if x #f #t))

(define (id obj) obj)

(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))

(define (curry func arg1) (lambda (arg) (apply func (cons arg1 (list arg)))))

(define (compose f g) (lambda (arg) (f (apply g arg))))

(define zero? (curry = 0))

(define positive? (curry < 0))

(define negative? (curry > 0))

(define (odd? num) (= (mod num 2) 1))

(define (even? num) (= (mod num 2) 0))

(define call-with-current-continuation call/cc)

(define (error . elements) (display elements)) 

(define (sum . lst) (fold + 0 lst))

(define (product . lst) (fold * 1 lst))

(define (max first . num-list) (fold (lambda (old new) (if (> old new) old new)) first num-list))

(define (min first . num-list) (fold (lambda (old new) (if (< old new) old new)) first num-list))

(define-rewriter define-macro
  (lambda args
    `(define-rewriter ,(car (car args)) (lambda ,(cdr (car args)) ,(car (cdr args))))))

(define make-promise
  (lambda (proc)
    (let ((result-ready? #f)
         (result #f))
      (lambda ()
        (if result-ready?
            result
            (let ((x (proc)))
              (if result-ready?
                  result
                  (begin (set! result-ready? #t)
                         (set! result x)
                         result))))))))

(define-macro (delay expr) `(make-promise (lambda () ,expr)))

(define-macro (force expr) `(apply ,expr '()))

(define-macro (unless expr consequent) `(if (not ,expr) ,consequent))

(define-macro (when expr consequent) `(if ,expr ,consequent))

(define-macro (do init test . expr)
  (let ((bindings (map (lambda (x) (cons (car x) (list (cadr x)))) init))
       (steps (map (lambda (x) (if (< 2 (length x)) (caddr x) (car x))) init))
       (condition (car test))
       (endresult (cdr test))
       (loop-name (gensym)))

    `(let ,loop-name ,bindings
       (if (not ,condition)
           (begin ,(car expr) ,(cons loop-name steps))
	   ,@endresult))))
