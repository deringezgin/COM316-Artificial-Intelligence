(define scores '())
(define run-tests
  (lambda (test-count)
    (cond
      ((= test-count 0) '())
      (else
        (load "grid-main.ss")
        (run-tests (- test-count 1))))))

(run-tests 3)
(display scores)
