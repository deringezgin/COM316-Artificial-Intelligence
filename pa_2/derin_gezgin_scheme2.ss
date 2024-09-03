; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Scheme #2
; Due September 3 2024

; For my own usage
(define println
  (lambda (n)
    (display n)
    (newline)))

(define print-l
  (lambda (l n)
    (display (string-append "Current l" n " --> "))
    (println l)))


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
(print-l l1 "1")
(display "Applying the sqr-list function to l1 --> ")
(println (sqr-list l1))
(display "Testing with an empty list --> ")
(println (sqr-list '()))
(define l1 (sqr-list l1)) ; Updating l1 in the background for continuity
(newline)


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
(print-l l1 "1")
(display "Inserting 3 into l1 --> ")
(define l1 (place 3 l1))
(print-l l1 "1")
(display "Inserting -1 into l1 --> ")
(define l1 (place -1 l1))
(print-l l1 "1")
(display "Inserting 100 into l1 --> ")
(define l1 (place 100 l1))
(print-l l1 "1")
(display "Inserting 0 into l1 --> ")
(define l1 (place 0 l1))
(print-l l1 "1")
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

(println "Function #3 [change] and #4 [change-list]")
(print-l l1 "1")
(display "Applying the change-list function to l1 --> ")
(define l1 (change-list l1))
(print-l l1 "1")
(newline)

; ___________________________________________________________________________
; Function #5 closest-point
; Returns the point in the list of points that is closest to the input point

(define sqr
  (lambda (n)
    (* n n)))

(define dist_pt
  (lambda (p1 p2)
    (sqrt (+ (sqr (- (car p1) (car p2)))
             (sqr (- (cadr p1) (cadr p2)))))))

(define min-point
  (lambda (point p1 p2)
    (cond
      ((< (dist_pt point p1) (dist_pt point p2)) p1)
      (else p2)
    )))

(define closest-point
  (lambda (point lst)
    (cond
      ((null? lst) '())
      ((null? (cdr lst)) (car lst))
      (else (min-point point (car lst) (closest-point point (cdr lst)))))))

(println "Function #5 closest-point")

(define l1 '((2 3) (4 6) (1 8) (7 2)))
(define target '(3 4))
(print-l l1 "1")
(display "Current target --> ")
(println target)
(display "Closest point --> ")
(println (closest-point target l1))
(newline)

(define l1 '((1 7) (7 1) (7 7)))
(define target '(4 4))
(print-l l1 "1")
(display "Current target --> ")
(println target)
(display "Closest point --> ")
(println (closest-point target l1))
(newline)

(define l1 '((5 5)))
(define target '(4 4))
(print-l l1 "1")
(display "Current target --> ")
(println target)
(display "Closest point --> ")
(println (closest-point target l1))
(newline)

(define l1 '((0 0) (3 4) (5 12) (8 15)))
(define target '(6 8))
(print-l l1 "1")
(display "Current target --> ")
(println target)
(display "Closest point --> ")
(println (closest-point target l1))
(newline)

; ___________________________________________________________________________
; Function #6 add-list
; adds each element of the 2 lists

(define add-list
  (lambda (lst1 lst2)
    (cond
      ((or (null? lst1) (null? lst2)) '())
      (else (cons (+ (car lst1) (car lst2)) (add-list (cdr lst1) (cdr lst2)))))))

(define l2 '(1 2 3))
(define l3 '(4 5 6))

(println "Function #6 add-list")
(print-l l2 "2")
(print-l l3 "3")
(display "Adding l2 and l3 --> ")
(println (add-list l2 l3))
(newline)


; ___________________________________________________________________________
; Function #7 delete-lists
; deletes the sub-lists from this list.

(define delete-lists
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((atom? (car lst)) (cons (car lst) (delete-lists (cdr lst))))
      (else (delete-lists (cdr lst))))))

(define l4 '(1 2 (3 4) (5 (6 7)) 8 (9)))

(println "Function #7 delete-lists")
(print-l l4 "4")
(display "Applying delete-lists into l4 --> ")
(println (delete-lists l4))
(newline)


; ___________________________________________________________________________
; Function #8 flatten
; removes sub-list structure but keeps the list elements

(define flatten
  (lambda (lst)
    (reverse_lst (flattener lst '()))))

(define flattener
  (lambda (lst new)
    (cond
      ((null? lst) new)
      ((atom? (car lst)) (flattener (cdr lst) (cons (car lst) new)))
      (else (flattener (cdr lst) (flattener (car lst) new))))))


(define reverse_lst
  (lambda (lst)
    (reverser lst '())))

(define reverser
  (lambda (lst new)
    (cond
      ((null? lst) new)
      (else (reverser (cdr lst) (cons (car lst) new))))))


(define l5 '(1 2 (3 (4 5) 6)))
(define l6 '(((((1)))) (2 3) (4 5 6) ((7 8)) 9))

(println "Function #8 flatten")
(print-l l5 "5")
(display "Applying the flatten function to l5 --> ")
(println (flatten l5))
(print-l l6 "6")
(display "Applying the flatten function to l6 --> ")
(println (flatten l6))
(newline)

