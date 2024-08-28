; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Scheme #2
; Due September 3 2024

; For my own usage
(define println
  (lambda (n)
    (display n)
    (newline)))

(define print-l1
  (lambda (l1)
    (display "Current l1 --> ")
    (println l1)))


; ___________________________________________________________________________
; Function #1 sqr-list
; squares each element of the list

(define sqr
  (lambda (n)
    (* n n)))

(define sqr-list
  (lambda (lst)
    (cond
      ((null? lst) '())
      (else (cons (sqr (car lst)) (sqr-list (cdr lst)))))))


(define l1 '(1 2 3 4 5))
(println "Function #1 sqr-list")
(print-l1 l1)
(display "Applying the sqr-list function to l1 --> ")
(println (sqr-list l1))
(display "Testing with an empty list --> ")
(println (sqr-list '()))
(newline)
(define l1 (sqr-list l1)) ; Updating l1 in the background for continuity


; ___________________________________________________________________________
; Function #2 place
; inserts x into ordered list lst

(define place
  (lambda (x lst)
    (cond
      ((null? lst) (list x))
      ((<= x (car lst)) (cons x lst))
      (else (cons (car lst) (place x (cdr lst)))))))


(println "Function #2 place")
(print-l1 l1)
(display "Inserting 3 into l1 --> ")
(define l1 (place 3 l1))
(print-l1 l1)
(display "Inserting -1 into l1 --> ")
(define l1 (place -1 l1))
(print-l1 l1)
(display "Inserting 100 into l1 --> ")
(define l1 (place 100 l1))
(print-l1 l1)
(newline)

; ___________________________________________________________________________
; Function #3 & #4
; #3 --> if x is negative it returns x^2, if 0 it returns 0, if positive it returns x + 1.
; #4 --> applies change to each element in the list

(define change
  (lambda (x)
    (cond
      ((< x 0) (* x x))
      ((= x 0) 0)
      ((> x 0) (+ x 1)))))

(define change-list
  (lambda (lst)
    (cond
      ((null? lst) '())
      (else (cons (change (car lst)) (change-list (cdr lst)))))))


(display "Function #3 [change] and #4 [change-list]")
(print-l1 l1)
(println "Applying the change-list function to l1 --> ")
(define l1 (change-list l1))
(print-l1 l1)
(newline)





(define flatten
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((atom? (car lst)) (cons (car lst) (flatten (cdr lst))))
      (else (cons (flatten (car lst)) (flatten (cdr lst)))))))

(define l2 '(1 2 (3 4 5 6)))
(println (flatten l2))