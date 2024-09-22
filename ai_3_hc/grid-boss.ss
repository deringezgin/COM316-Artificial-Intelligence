; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #3
; Due September 24 2024
; File that has the complete code for the boss file
; This file has the code to run tests over and over again and at the end displays the average result

(define test-count 10)
(define total-rta 0)
(define total-hc 0)

(define run-tests
  (lambda (test-count)
    (cond
      ((> test-count 0) (load "grid-main.ss") (run-tests (- test-count 1)))
      (else (display "TEST-RUNS-DONE")))))

(run-tests test-count)
(newline)
(newline)
(display "AVERAGE RTA: ")
(display (inexact(/ total-rta test-count)))
(newline)
(display "AVERAGE HC: ")
(display (inexact(/ total-hc test-count)))
