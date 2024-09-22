(define test-count 5)
(define total-rta 0)
(define total-hc 0)

(define run-tests
  (lambda (test-count)
    (cond
      ((> test-count 0) (load "grid-main.ss") (run-tests (- test-count 1)))
      (else (display "TEST-RUNS-DONE")))))

(run-tests test-count)

(display "AVERAGE RTA: ")
(display (exact->inexact (/ total-rta test-count)))
(newline)
(display "AVERAGE HC: ")
(display (exact->inexact (/ total-hc test-count)))
