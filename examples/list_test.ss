; List testing examples stolen from:
; Copyright (C) 2011 Toby Thain, toby@telegraphics.com.au

; returns a list of two elements:
;   first is smallest value found in list;
;   second is list with first occurrence removed
(define (remove-smallest lst)
  (let remove-smaller-than ((val (car lst))
                            (rest (cdr lst))
                            (acc '()))
    (cond
      ((null? rest)
        (list val acc))
      ((< (car rest) val)
        (remove-smaller-than (car rest) (cdr rest) (cons val acc)))
      (else
        (remove-smaller-than val (cdr rest) (cons (car rest) acc))))))

(define (sel-sort lst)
  (if (null? lst)
    '()
    (let ((smallest (remove-smallest lst)))
      (cons (car smallest) (sel-sort (cadr smallest))))))


; produce new list with E interleaved between each pair of elements in input list
(define (interleave E L)
  (if (null? L)
      '()
      (cons (car L)              ; head of list
            (if (null? (cdr L))  ; followed by...
                '()              ; nothing, if it was the last element,
                (cons E          ; otherwise E, then E interleaved with rest of list
                      (interleave E (cdr L)))))))

(define test_list '(9 8 7 6 5 4 3 2 1))
(define sorted_list (sel-sort test_list))
(define interleaved_list (interleave 'TEST test_list))

(display "Test list: ")
(display test_list)
(newline)
(display "Sorted list: ")
(display sorted_list)
(newline)
(display "Interleaved list: ")
(display interleaved_list)
(newline)
