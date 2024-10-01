(define rules
  '(((visited00 goal00) (finished))


(define facts
  '(goal22 visited00 obstacle11 obstacle21))

(define ModusPonens
  (lambda (rule)
    (ModusPonens2 (car rule) (cadr rule))))
      
(define ModusPonens2 
  (lambda (b a)
    (if (null? b)
        a
    ;else
        (let 

(define search
  (lambda (count)
    (cond
      ((member 'finished facts)
          (display "goal found")
          (newline))
      ((<= count 0)
        (display "not found")
        (newline))
      (else
        (let* ((firstRule (car rules))
               (remainingRules (append (cdr rules) (list firstRule)))
               (newFact (ModusPonens firstRule)))
          (cond 

