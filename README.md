Schemin
=======


Schemin is a scheme-ish interpreter, written in C#. This is a learning project to help me understand language design, so don't expect too much. Complete enough to run some simple sort routines and such, like this bubblesort:

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

Implemented featues:
--------------------

+ define and set!
+ lambdas with closures
+ Let, named let, letrec and let\*
+ Some flow control (if, cond and boolean primitives)
+ A few list operations like map, filter, and foldl (as well as cons, cdr, car, etc)
+ Some basic numerical primitives and decimal support


To do:
------

+ Implement continuations as a first-class type
+ Re-implement the evaluator so it's not recursive
+ Possibly implement a parser generator instead of our recursive parser
+ Implement more common primitives







