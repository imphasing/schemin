Schemin
=======


Schemin is a BSD-licensed scheme-ish interpreter, written in C#. Schemin is complete enough to run some simple scheme snippets, such as this example bubblesort routine:

    (define bubblesort (lambda (l)
      (define swap-pass (lambda (l)
        (if (eq? (length l) 1) 
            l
            (let ((fst (car l))(snd (cadr l))(rest (cddr l)))
              (if (> fst snd) 
                  (cons snd (swap-pass (cons fst rest)))
                  (cons fst (swap-pass (cons snd rest))))))))
      (let for ((times (length l)) (val l))
        (if (> times 1)
            (for (- times 1)(swap-pass val))
            (swap-pass val)))))

As well as some more complicated things, like this simple example of coroutines using call/cc:

    (define (superfluous-computation do-other-stuff) 
      (let loop () 
        (map (lambda (graphic) 
          (display graphic) 
          (newline) 
          (set! do-other-stuff (call/cc do-other-stuff))) 
        '("Straight up." "Quarter after." "Half past."  "Quarter til.")) 
      (loop))) 

    (define (hefty-computation do-other-stuff) 
     (let loop ((n 5)) 
      (display "Hefty computation: ") 
      (display n) 
      (newline) 
      (set! do-other-stuff (call/cc do-other-stuff)) 
      (display "Hefty computation (b)")  
      (newline) 
      (set! do-other-stuff (call/cc do-other-stuff)) 
      (display "Hefty computation (c)") 
      (newline) 
      (set! do-other-stuff (call/cc do-other-stuff)) 
      (if (> n 0) 
       (loop (- n 1))))) 

    (hefty-computation superfluous-computation)


Or this basic macro, which adds the arguments given to it, using a quasiquoting template:

    (define-macro (testadd . toadd) `(fold (lambda (acc x) (+ acc x)) 0 (list ,@toadd)))

Check out the examples directory for more examples of what Schemin is capable of.


Implemented featues:
--------------------

+ Basic unhygenic macros, using define-macro paired with quasiquoting.
+ All primitive types in r5rs besides vectors
+ First-class continuations and call/cc
+ IO Ports (File and console IO)
+ define and set!
+ lambdas with closures
+ Let, named let, Let\*, and letrec
+ Some flow control (if, cond, and, or and boolean primitives)
+ A few list operations like map, filter, and foldl (as well as cons, cdr, car, etc)
+ Some basic numerical primitives and decimal support


To do:
------

+ Possibly implement a parser generator instead of our recursive parser
+ Implement more common primitives
+ Implement vectors
+ Implement hygenic macros
