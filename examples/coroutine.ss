; Stolen from: http://matt.might.net/articles/programming-with-continuations--exceptions-backtracking-search-threads-generators-coroutines/

; thread-queue : list[continuation]
(define thread-queue '())

; halt : continuation
(define halt #f)

; void : -> void
(define (void) (if #f #t))

; current-continuation : -> continuation
(define (current-continuation)
  (call-with-current-continuation
   (lambda (cc)
     (cc cc))))

; spawn : (-> anything) -> void
(define (spawn thunk)
  (let ((cc (current-continuation)))
    (if (procedure? cc)
        (set! thread-queue (append thread-queue (list cc)))
        (begin (thunk)
               (quit)))))

; yield : value -> void
(define (yield)
  (let ((cc (current-continuation)))
    (if (and (procedure? cc) (pair? thread-queue))
        (let ((next-thread (car thread-queue)))
          (set! thread-queue (append (cdr thread-queue) (list cc)))
          (next-thread 'resume))
        (void))))

; quit : -> ...
(define (quit)
  (if (pair? thread-queue)
      (let ((next-thread (car thread-queue)))
        (set! thread-queue (cdr thread-queue))
        (next-thread 'resume))
      (halt)))
   
; start-threads : -> ...
(define (start-threads)
  (let ((cc (current-continuation)))
    (if cc
        (begin
          (set! halt (lambda () (cc #f)))
          (if (null? thread-queue)
              (void)
              (begin
                (let ((next-thread (car thread-queue)))
                  (set! thread-queue (cdr thread-queue))
                  (next-thread 'resume)))))
        (void))))

;; Example cooperatively threaded program
(define counter 10)

(define (make-thread-thunk name)
  (letrec ((loop (lambda ()
                   (if (< counter 0)
                       (quit))
                   (display "in thread ")
                   (display name)
                   (display "; counter = ")
                   (display counter)
                   (newline)
                   (set! counter (- counter 1))
                   (yield)
                   (loop))))
    loop))

(spawn (make-thread-thunk 'a))
(spawn (make-thread-thunk 'b))
(spawn (make-thread-thunk 'c))

(start-threads)
