; List testing examples stolen from:
; Copyright (C) 2011 Toby Thain, toby@telegraphics.com.au

; returns a list of two elements:
;   first is smallest value found in list;
;   second is list with first occurrence removed
(define (remove-smallest lst)
  (let remove-smaller-than ((val (car lst))
                            (rest (cdr lst))
                            (acc (quote ())))
    (cond
      ((null? rest)
        (list val acc))
      ((< (car rest) val)
        (remove-smaller-than (car rest) (cdr rest) (cons val acc)))
      (else
        (remove-smaller-than val (cdr rest) (cons (car rest) acc))))))

(define (sel-sort lst)
  (if (null? lst)
    (quote ())
    (let ((smallest (remove-smallest lst)))
      (cons (car smallest) (sel-sort (cadr smallest))))))


; produce new list with E interleaved between each pair of elements in input list
(define (interleave E L)
  (if (null? L)
      (quote ())
      (cons (car L)              ; head of list
            (if (null? (cdr L))  ; followed by...
                (quote ())              ; nothing, if it was the last element,
                (cons E          ; otherwise E, then E interleaved with rest of list
                      (interleave E (cdr L)))))))


(define bubblesort (lambda (l)
  (define swap-pass (lambda (l)
    (if (= (length l) 1) 
        l
        (let ((fst (car l))(snd (cadr l))(rest (cddr l)))
          (if (> fst snd) 
              (cons snd (swap-pass (cons fst rest)))
              (cons fst (swap-pass (cons snd rest))))))))
  (let for ((times (length l))
            (val l))
    (if (> times 1)
        (for (- times 1)(swap-pass val))
        (swap-pass val)))))

(define qs 
  (lambda (l) 
    (if (null? l) 
      (quote ()) 
      (append (qs (filter (lambda (x) (<= x (car l))) (cdr l))) (cons (car l) (qs (filter (lambda (x) (> x (car l))) (cdr l))))))))


(define test_list (list 23 5342 234 44 322 6654 44 1 3 54 22 334 11 332 44 322 11 3 1 4 6 3 2 5 7 2 234 2))
(define sel_sorted_list (sel-sort test_list))
(define bubble_sorted_list (bubblesort test_list))
(define qs_sorted_list (qs test_list))
(define interleaved_list (interleave "I" test_list))

(display "Test list: ")
(display test_list)
(newline)
(display "Selection Sorted list: ")
(display sel_sorted_list)
(newline)
(display "Bubble sorted list: ")
(display bubble_sorted_list)
(newline)
(display "Quicksorted list: ")
(display qs_sorted_list)
(newline)
(display "Interleaved list: ")
(display interleaved_list)
(newline)
