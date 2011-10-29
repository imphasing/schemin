;;;; Shamelessly stolen from: https://bitbucket.org/tarballs_are_good/scheme-random
;;;; (With some very slight modifications)

;;;; Infinite lists
;;;; (c) 2010 Robert Smith

;;; lazy cons... we don't want to evaluate Y!!
(define-macro (lcons x y) `(cons ,x (delay ,y)))

;;; Head of a lazy list
(define lhead car)

;;; Tail of a lazy list
(define (ltail llist) (force (cdr llist)))

;;; Is a lazy list empty?
(define lempty? null?)

;;; A canonical empty lazy list
(define lnil '())

;;; Take n items from a lazy list
(define (ltake n llist)
  (if (zero? n)
      lnil
      (cons (lhead llist)
            (ltake (- n 1)
                   (ltail llist)))))

;;; Remove the elements of lst that don't satisfy p.
(define (lfilter p lst)
  (cond ((lempty? lst) lnil)
        ((p (lhead lst)) (lcons (lhead lst) (lfilter p (ltail lst))))
        (else (lfilter p (ltail lst)))))

;;; For l1 = (a b c ...) and l2 = (x y z ...), return
;;; ((f a x) (f b y) (f c z) ...) lazily.
(define (lzipwith f l1 l2)
 (let ((h1 (lhead l1))
       (h2 (lhead l2)))
   (lcons (f h1 h2)
          (lzipwith f (ltail l1) (ltail l2)))))

(define (l+ l1 l2) (lzipwith + l1 l2))
(define (l- l1 l2) (lzipwith - l1 l2))
(define (l* l1 l2) (lzipwith * l1 l2))
(define (l/ l1 l2) (lzipwith / l1 l2))
(define (lzip l1 l2) (lzipwith cons l1 l2))

;;;;----------------------------------------------
;;;; EXAMPLES

;;; Example 1
(define (stream-of n)
  (lcons n (stream-of n)))

;;; Example 2
(define (numbers-above n)
  (lcons (+ n 1)
         (numbers-above (+ n 1))))

(define (numbers-below n)
  (lcons (- n 1)
         (numbers-below (- n 1))))

(define positive-numbers (numbers-above 0))
(define negative-numbers (numbers-below 0))

;;; Example 3
(define fibonaccis
  (lcons
   0
   (lcons
    1
    (l+ (ltail fibonaccis) fibonaccis))))

;;; Example 4
(define (divisible-by? x n)
  ;; is n divisible by x?
  (zero? (mod n x)))

(define (lsieve unsieved)
  (lcons
   (lhead unsieved)
   (lsieve
    (lfilter
     prime?
     (ltail unsieved)))))

(define primes
  (lsieve (numbers-above 1)))

;;; Execute examples
(define (do-examples)
  (display "Example 2: Multiply the first 10 positive numbers by") (newline)
  (display "           the 10 negative numbers after -1.")
  (newline)
  (display (ltake 10 positive-numbers)) (newline)
  (display (ltake 10 (ltail negative-numbers))) (newline)
  (display (ltake 10
                  (l* positive-numbers (ltail negative-numbers))))
  (newline)
  (newline)

  (display "Example 3: Get 20 Fibonacci numbers, lazily.") (newline)
  (display (ltake 20 fibonaccis))
  (newline)
  (newline)

  (display "Example 4: Get 50 primes, lazily.") (newline)
  (display (ltake 50 primes))
  (newline)
  (newline)
  )

  (do-examples)
