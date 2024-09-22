; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #3
; Due September 24 2024
; File that has the complete code for the boss file
; This file has the code to run tests over and over again and at the end displays the average result

(define test-count 5)
(define total-rta '())
(define total-hc '())

(define run-tests
  (lambda (test-count)
    (cond
      ((> test-count 0) (load "grid-main.ss") (run-tests (- test-count 1)))
      (else (display "TEST-RUNS-DONE") (newline)))))

(define sum
  (lambda (lst)
    (cond
      ((null? lst) 0)
      (else (+ (car lst) (sum (cdr lst)))))))

(define (sum-list lst)
  (cond
    ((null? lst) 0) ; base case: if the list is empty, return 0
    (else (+ (car lst) (sum-list (cdr lst)))))) ; add the first element to the sum of the rest

(run-tests test-count)

(define remove-negatives (lambda (l1 l2) (helper l1 l2 '() '())))

(define helper
  (lambda (l1 l2 result1 result2)
    (cond
      ((null? l1) (list (reverse result1) (reverse result2)))
      ((or (= (car l1) -1) (= (car l2) -1)) (helper (cdr l1) (cdr l2) result1 result2))
      (else (helper (cdr l1) (cdr l2) (cons (car l1) result1) (cons (car l2) result2))))))

(define cleaned-lists (remove-negatives total-rta total-hc))
(set! total-rta (car cleaned-lists))
(set! total-hc (cadr cleaned-lists))
(set! test-count (length total-rta))

(newline)
(newline)
(display "AVERAGE RTA: ")
(display (inexact(/ (sum total-rta) test-count)))
(newline)
(display "AVERAGE HC: ")
(display (inexact(/ (sum total-hc) test-count)))
