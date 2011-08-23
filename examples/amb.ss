; current-continuation : -> continuation
(define (current-continuation) 
  (call-with-current-continuation 
   (lambda (cc)
     (cc cc))))

; fail-stack : list[continuation]
(define fail-stack '())

; fail : -> ...
(define (fail)
  (if (not (pair? fail-stack))
      (error "back-tracking stack exhausted!")
      (begin
        (let ((back-track-point (car fail-stack)))
          (set! fail-stack (cdr fail-stack))
          (back-track-point back-track-point)))))

; amb : list[a] -> a
(define (amb choices)
  (let ((cc (current-continuation)))
    (if (null? choices) (fail)
      (if (pair? choices)      (let ((choice (car choices)))
                              (set! choices (cdr choices))
                              (set! fail-stack (cons cc fail-stack))
                              choice)))))

; (assert condition) will cause
; condition to be true, and if there
; is no way to make it true, then
; it signals and error in the program.
(define (assert condition)
  (if (not condition)
      (fail)
      #t))


; The following prints (4 3 5)
(let ((a (amb (list 1 2 3 4 5 6 7)))
      (b (amb (list 1 2 3 4 5 6 7)))
      (c (amb (list 1 2 3 4 5 6 7))))
    
  ; We're looking for dimensions of a legal right
  ; triangle using the Pythagorean theorem:
  (assert (= (* c c) (+ (* a a) (* b b))))
  
  (display (list a b c))
  (newline)
  
  ; And, we want the second side to be the shorter one:
  (assert (< b a))

  ; Print out the answer:
  (display (list a b c))
  (newline))
