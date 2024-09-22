; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #3
; Due September 24 2024
; File that has the complete code for the boss file
; This file has the code to run tests over and over again and at the end displays the average result

(define test-count 1)
(define total-rta '())
(define total-hc '())

(define run-tests
  (lambda (test-count)
    (cond
      ((> test-count 0) (load "grid-main.ss") (run-tests (- test-count 1)))
      (else (display "TEST-RUNS-DONE")))))

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
(newline)
(newline)
(display "AVERAGE RTA: ")
(display (inexact(/ (sum total-rta) test-count)))
(newline)
(display "AVERAGE HC: ")
(display (inexact(/ (sum total-hc) test-count)))
