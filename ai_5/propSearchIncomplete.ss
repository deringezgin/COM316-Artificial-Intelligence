; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #5
; Due September 24 2024
; File that has the complete code for Simple Propositional Logic Example
; As an extra, I added some testing code that'd generate a random grid and arrange the facts and rules accordingly.
; It can run multiple tests and display the results. I over-engineered it a little bit:))

(define rules '())
(define reset-rules
  (lambda ()
    (set! rules
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
      ))))

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


; ========== MY CODE FOR TESTING ==========
; It selects random obstacle locations and
(define possible-obstacles '())
(define possible-starts '())
(define possible-goals '())
(define facts '())
(define obstacle-count 0)
(define test-count 10)
(define pick-random-index (lambda (lst) (random (length lst))))

(define remove-element-all
  (lambda (index)
    (set! possible-obstacles (remove (list-ref possible-obstacles index) possible-obstacles))
    (set! possible-goals (remove (list-ref possible-goals index) possible-goals))
    (set! possible-starts (remove (list-ref possible-starts index) possible-starts))))

(define add-random-element
  (lambda (lst)
    (let* ((index (pick-random-index lst))
           (element (list-ref lst index)))
      (remove-element-all index)
      (set! facts (cons element facts)))))

(define construct-grid
  (lambda (obstacle-count)
    (cond
      ((= obstacle-count 0)
        (add-random-element possible-starts)
        (add-random-element possible-goals))
      (else
        (add-random-element possible-obstacles)
        (construct-grid (- obstacle-count 1))))))

(define run-test
  (lambda (test)
    (cond
      ((= test (+ test-count 1)) '())
      (else
        (set! facts '())
        (set! rules '())
        (reset-rules)
        (set! possible-obstacles '(obstacle00 obstacle01 obstacle02 obstacle10 obstacle11 obstacle12 obstacle20 obstacle21 obstacle22))
        (set! possible-goals '(goal00 goal01 goal02 goal10 goal11 goal12 goal20 goal21 goal22))
        (set! possible-starts '(visited00 visited01 visited02 visited10 visited11 visited12 visited20 visited21 visited22))
        (set! obstacle-count (random 7))
        (newline)
        (display "TEST: ")
        (display test)
        (newline)
        (construct-grid obstacle-count)
        (display "FACTS --> ")
        (display facts)
        (newline)
        (search 1000)
        (run-test (+ test 1))))))

(run-test 1)
