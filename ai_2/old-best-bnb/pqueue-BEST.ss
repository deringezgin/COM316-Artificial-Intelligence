; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #2
; Due September 17 2024
; File that has the complete code for priority queue implemented for Best-First Search

; Creating an empty priority queue
(define pqueue '())

; Function to return the first node in the priority queue
(define pqueue_front
  (lambda ()
    (cond
      ((null? pqueue) '())
      (else (car pqueue)))))

; Dequeue function for the priority queue
(define pqueue_dequeue
  (lambda ()
    (cond
      ((null? pqueue) '())  ; If the pequeue is empty, return an empty list
      ; Calculate the heuristic value of the front
      ; After this randomly select a value from the locations with the same heuristic value.
      (else (let* ((heuristic_value (heuristic (pqueue_front)))
                   (selected (random_select (filter_elements pqueue heuristic_value))))
              (set! pqueue (remove_element pqueue selected)) selected)))))  ; Remove the location from the pqueue

; Function to randomly select a value in a list and return it
(define random_select
  (lambda (lst)
    (cond
      ((null? lst) '())
      (else (list-ref lst (random (length lst)))))))

; Function to remove an element -to_remove- from a list and return the list
(define remove_element
  (lambda (lst to_remove)
    (cond
      ((null? lst) '())
      ((equal? (car lst) to_remove) (remove_element (cdr lst) to_remove))
      (else (cons (car lst) (remove_element (cdr lst) to_remove))))))

; Function to return a sub-list of items that has the same heuristic value from a list
(define filter_elements
  (lambda (lst heuristic_value)
    (cond
      ((null? lst) '())  ; Base-case, return an empty list
      ; If the heuristic is the same, add it to the returning list
      ((= (heuristic (car lst)) heuristic_value) (cons (car lst) (filter_elements (cdr lst) heuristic_value)))
      ; Otherwise continue search with the rest
      (else (filter_elements (cdr lst) heuristic_value)))))

; Function to enqueue a list of items to a priority queue
(define pqueue_enqueue
  (lambda (to_insert_lst)
    (cond
      ((null? to_insert_lst) '())
      (else
        ; Add the first element to the pqueue using the helper function
        (set! pqueue (pqueue_enqueuer (car to_insert_lst) pqueue))
        ; Call the function again by rest of the insert list
        (pqueue_enqueue (cdr to_insert_lst))))))

; Helper function to insert a location into the priority queue
(define pqueue_enqueuer
  (lambda (to_insert current_pqueue)
    (cond
      ((null? current_pqueue) (list to_insert))
      ; Compare the heuristic and if #t, insert the location
      ((compare_heuristic to_insert (car current_pqueue)) (cons to_insert current_pqueue))
      ; Otherwise, continue checking
      (else (cons (car current_pqueue) (pqueue_enqueuer to_insert (cdr current_pqueue)))))))

; Function to compare the heuristic of current and goal locations and return #t if current location has priority
(define compare_heuristic
  (lambda (to_insert current)
    (< (heuristic to_insert) (heuristic current))))

; Function to calculate the block distance between the current and the goal locations
(define heuristic
  (lambda (current)
    (let ((current_x (car current)) (current_y (cadr current)) (goal_x (car goal)) (goal_y (cadr goal)))
      (+ (abs (- current_x goal_x)) (abs (- current_y goal_y))))))
