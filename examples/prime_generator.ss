;;;; Shamelessly stolen from: https://bitbucket.org/tarballs_are_good/scheme-random

;;;; call/cc stuff
;;;; (c) 2010 Robert Smith

;;;; Load this up and call (get-next-prime)

(define get-next-prime
  (letrec
      ((entry
        (lambda (v) ;; v can be used as a resume value if desired
          (let LOOP ((i 2))
            (if (prime? i)
                (yield i))
            (LOOP (+ i 1)))))
       (exit #f) ;; exit will be set by coroutine below
       (yield
        (lambda (yield-value)
          (call/cc
           (lambda (k)
             (set! entry k)
             (exit yield-value)))))
       (coroutine
        (lambda (v) ;; v can be used as a resume value since it's
                    ;; passed to ENTRY
          (call/cc
           (lambda (k)
             (set! exit k)
             (entry v))))))
    (lambda () (coroutine #f))))



(define test-list '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
(define primes (map get-next-prime test-list))
(display "Generated primes: ")
(display primes)
(newline)
