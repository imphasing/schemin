(define (succ n) (+ n 1))
(define (pred n) (- n 1))

(define (addc x y k)
  (if (zero? y)
    (k x)
    (addc (succ x) (pred y) k)))

(define (fibc x c)
  (if (zero? x)
    (c 0)
    (if (zero? (pred x))
      (c 1)
      (addc (call-with-current-continuation
             (lambda (c) (fibc (pred x) c)))
            (call-with-current-continuation
             (lambda (c) (fibc (pred (pred x)) c)))
            c))))

(fibc 30 id)
