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
  ; And, we want the second side to be the shorter one:
  (assert (= (* c c) (+ (* a a) (* b b))))
  (assert (< b a))
  
  ; Print out the answer:
  (display (list a b c))
  (newline))


; Generate a prime from a list of numbers..
(let ((a (amb (list 14 15 16 17 18 19))))
  (assert (prime? a))
  ; Print out the answer:
  (display a)
  (newline))

; test out amb with strings
(let ((w-1 (amb (list "the" "that" "a")))
      (w-2 (amb (list "frog" "elephant" "thing")))
      (w-3 (amb (list "walked" "treaded" "grows")))
      (w-4 (amb (list "slowly" "quickly"))))
  (define (joins? left right)
    (eq? (string-ref left (- (string-length left) 1)) (string-ref right 0)))
  (if (joins? w-1 w-2) '() (amb))
  (if (joins? w-2 w-3) '() (amb))
  (if (joins? w-3 w-4) '() (amb))
  (list w-1 w-2 w-3 w-4))
