; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #5
; Due September 24 2024
; File that has the complete code for Simple Propositional Logic Example

(define rules
  '(
     ; Moving right
     ((visited00 (not obstacle01)) (visited01)) ((visited01 (not obstacle02)) (visited02))
     ((visited10 (not obstacle11)) (visited11)) ((visited11 (not obstacle12)) (visited12))
     ((visited20 (not obstacle21)) (visited21)) ((visited21 (not obstacle22)) (visited22))

     ; Moving left
     ((visited01 (not obstacle00)) (visited00)) ((visited02 (not obstacle01)) (visited01))
     ((visited11 (not obstacle10)) (visited10)) ((visited12 (not obstacle11)) (visited11))
     ((visited21 (not obstacle20)) (visited20)) ((visited22 (not obstacle21)) (visited21))

     ; Moving up
     ((visited10 (not obstacle00)) (visited00)) ((visited11 (not obstacle01)) (visited01))
     ((visited12 (not obstacle02)) (visited02)) ((visited20 (not obstacle10)) (visited10))
     ((visited21 (not obstacle11)) (visited11)) ((visited22 (not obstacle12)) (visited12))

     ; Moving down
     ((visited00 (not obstacle10)) (visited10)) ((visited01 (not obstacle11)) (visited11))
     ((visited02 (not obstacle12)) (visited12)) ((visited10 (not obstacle20)) (visited20))
     ((visited11 (not obstacle21)) (visited21)) ((visited12 (not obstacle22)) (visited22))

     ; Goal reached
     ((visited00 goal00) (finished)) ((visited01 goal01) (finished)) ((visited02 goal02) (finished))
     ((visited10 goal10) (finished)) ((visited11 goal11) (finished)) ((visited12 goal12) (finished))
     ((visited20 goal20) (finished)) ((visited21 goal21) (finished)) ((visited22 goal22) (finished))
  ))

(define facts'(goal21 visited00 obstacle01))

(define ModusPonens
  (lambda (rule)
    (ModusPonens2 (car rule) (cadr rule))))
      
(define ModusPonens2 
  (lambda (b a)
    (if (null? b)
        a
    ;else
        (let ((head (car b)))
          (if (or (and (pair? head) (eq? 'not (car head)) (not (member (cadr head) facts))) (member head facts))
            (ModusPonens2 (cdr b) a)
            '())))))

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
          (set! rules remainingRules)  ; Rotate the rules
          (cond ((not (null? newFact)) (set! facts (append newFact facts))))
          (search (- count 1)))))))

(search 1000)
(display "END FACTS COMPLETE --> ")
(display facts)
(newline)
(display "END RULES COMPLETE --> ")
(display rules)
(newline)