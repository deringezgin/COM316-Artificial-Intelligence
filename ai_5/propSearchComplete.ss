; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #5
; Due October 3 2024
; File that has the complete code for Simple Propositional Logic Example
; As an extra, I added some testing code that'd generate a random grid and arrange the facts and rules accordingly.
; It can run multiple tests and display the results. I over-engineered it a little bit:))

; Defining all the possible rules on all 4 directions. That would be 6 rules per direction and a total of 24.
; At the same time, added the rules for checking all the possible goals. This gives 9 more rules for a grand-total of 33.
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
     ((visited20 goal20) (finished)) ((visited21 goal21) (finished)) ((visited22 goal22) (finished))))

(define ModusPonens
  (lambda (rule)
    ; (car rule) is the premises and (cadr rule) is the conclusion
    (ModusPonens2 (car rule) (cadr rule))))
      
(define ModusPonens2 
  (lambda (b a)
    (if (null? b)  ; If all premises is true, just return the conclusion as it'd be true
        a
    ;else
        (let ((head (car b)))  ; Check the first premise in the current b.
          ; Checks 1. if it has a not in front of it, it means that this is an obstacle. The program checks if it is NOT in the facts.
          ; If it's not in the facts as an obstacle, it continues to check if it's in the fact list.
          ; If it is in the fact list, it calls the function again to check the other premise.
          ; Any point in this process, if we have a False situaiton, it returns an empty list.
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
        ; Getting the first rule in the rules list, and also adding it to the end to cycle the rules list without losing any rules
        (let* ((firstRule (car rules))
               (remainingRules (append (cdr rules) (list firstRule)))
               (newFact (ModusPonens firstRule)))
          (set! rules remainingRules)  ; Rotate the rules
          (cond ((not (null? newFact)) (set! facts (append newFact facts))))  ; If the newFact is not null, add it to the end of the facts as it is true
          (search (- count 1)))))))  ; Continue the search


; ========== MY CODE FOR TESTING ==========
; It selects random obstacle, goal and start locations and add them to the facts.
(define possible-obstacles '())
(define possible-starts '())
(define possible-goals '())
(define facts '())
(define obstacle-count 0)
(define test-count 10)  ; How many tests would you like to run?
(define pick-random-index (lambda (lst) (random (length lst))))

; Removing the element in the specific "index" from all 3 possible obstacles/starts/goals
(define remove-element-all
  (lambda (index)
    (set! possible-obstacles (remove (list-ref possible-obstacles index) possible-obstacles))
    (set! possible-goals (remove (list-ref possible-goals index) possible-goals))
    (set! possible-starts (remove (list-ref possible-starts index) possible-starts))))

; Function to remove a random element from a list and adding it to facts.
(define add-random-element
  (lambda (lst)
    (let* ((index (pick-random-index lst))
           (element (list-ref lst index)))
      (remove-element-all index)
      (set! facts (cons element facts)))))

; Function to randomly construct a grid with specific obstacle count, one start and goal
(define construct-grid
  (lambda (obstacle-count)
    (cond
      ((= obstacle-count 0)
        (add-random-element possible-starts)
        (add-random-element possible-goals))
      (else
        (add-random-element possible-obstacles)
        (construct-grid (- obstacle-count 1))))))

; Function to run multiple tests. It'll reset possible obstacles, goals, and starts and call the test again with random obstacle density
(define run-test
  (lambda (test)
    (cond
      ((= test (+ test-count 1)) '())
      (else
        (set! facts '())
        (set! possible-obstacles '(obstacle00 obstacle01 obstacle02 obstacle10 obstacle11 obstacle12 obstacle20 obstacle21 obstacle22))
        (set! possible-goals '(goal00 goal01 goal02 goal10 goal11 goal12 goal20 goal21 goal22))
        (set! possible-starts '(visited00 visited01 visited02 visited10 visited11 visited12 visited20 visited21 visited22))
        (set! obstacle-count (random 7))
        (display "TEST: ")
        (display test)
        (newline)
        (construct-grid obstacle-count)  ; This constructs the facts
        (display "FACTS --> ")
        (display facts)
        (newline)
        (search 1000)  ; Calling the search here
        (newline)
        (run-test (+ test 1))))))

(run-test 1)
