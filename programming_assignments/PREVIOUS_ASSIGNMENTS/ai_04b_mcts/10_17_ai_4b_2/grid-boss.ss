(define scores '())
(define run-tests
  (lambda (test-count)
    (cond
      ((= test-count 0) '())
      (else
        (load "grid-main.ss")
        (run-tests (- test-count 1))))))

(define sum
  (lambda (lst)
    (cond
      ((null? lst) 0)
      (else (+ (car lst) (sum (cdr lst)))))))

(run-tests 10)
(display scores)
(display (inexact(/ (sum scores) (length scores))))
