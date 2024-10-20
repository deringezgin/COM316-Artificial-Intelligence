; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #3
; Due September 24 2024
; File that has the complete code for the boss file
; This file has the code to run tests over and over again and at the end displays the average result
; Please run this file rather than "grid-main.ss" to see the complete result

(define test-count 5)
(define total-rta '())
(define total-hc '())

(define run-tests
  (lambda (test-count)
    ; Function to run multiple tests by calling load on grid-main.
    ; You can modify test-count variable to increase/decrease the test-count
    (cond
      ((> test-count 0) (load "grid-main.ss") (run-tests (- test-count 1)))
      (else (display "TEST-RUNS-DONE") (newline)))))

(define sum
  (lambda (lst)
    ; Function to take sum of the values in a list
    (cond
      ((null? lst) 0)
      (else (+ (car lst) (sum (cdr lst)))))))

(define remove-negatives
  (lambda (l1 l2)
    ; In my implementation, invalid test runs (No way to reach to goal, etc.) are stored as -1.
    ; This function cleares from both result lists
    (negative-helper l1 l2 '() '())))

(define negative-helper
  (lambda (l1 l2 result1 result2)
    ; Helper function for remove negatives
    ; If list is null, reverse it and return both lists
    ; If either of the (car) is -1, skip that index in both and call again
    ; Otherwise call recursively
    (cond
      ((null? l1) (list (reverse result1) (reverse result2)))
      ((or (= (car l1) -1) (= (car l2) -1)) (negative-helper (cdr l1) (cdr l2) result1 result2))
      (else (negative-helper (cdr l1) (cdr l2) (cons (car l1) result1) (cons (car l2) result2))))))

(run-tests test-count)  ; Running the tests

; Clear -1, update the lists and get the total amount of valid tests
(define cleaned-lists (remove-negatives total-rta total-hc))
(set! total-rta (car cleaned-lists))
(set! total-hc (cadr cleaned-lists))
(set! valid-test-count (length total-rta))

(newline)
(newline)
(display "AVERAGE RTA: ")
(display (inexact(/ (sum total-rta) valid-test-count)))
(newline)
(display "AVERAGE HC: ")
(display (inexact(/ (sum total-hc) valid-test-count)))
