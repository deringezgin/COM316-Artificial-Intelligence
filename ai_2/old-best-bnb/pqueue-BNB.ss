; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #2
; Due September 17 2024
; File that has the complete code for priority queue implemented for Branch and Bound Search

(define pqueue '())  ; Define an empty priority queue

; Function to return the first element in the priority queue
(define pqueue_front
  (lambda ()
    (cond
      ((null? pqueue) '())
      (else (car pqueue)))))

; Function to dequeue an element from the priority queue
(define pqueue_dequeue
  (lambda ()
    (cond
      ((null? pqueue) '())  ; Return null if the pqueue is empty
      ; Get the heuristic value of the first node, select a random element among the same heuristic values
      (else (let* ((heuristic_value (get_node_heuristic (pqueue_front))) (selected (random_select (filter_elements pqueue heuristic_value))))
              ; Remove the element from the pqueue and return the selected element
              (set! pqueue (remove_element pqueue selected)) selected)))))

; Randomly select an element from a list
(define random_select
  (lambda (lst)
    (cond
      ((null? lst) '())
      (else (list-ref lst (random (length lst)))))))

; Remove an element from a list
(define remove_element
  (lambda (lst to_remove)
    (cond
      ((null? lst) '())
      ((equal? (car lst) to_remove) (remove_element (cdr lst) to_remove))
      (else (cons (car lst) (remove_element (cdr lst) to_remove))))))

; Construct a list of elements that has the same heuristic value
(define filter_elements
  (lambda (lst heuristic_value)
    (cond
      ((null? lst) '())
      ((= (get_node_heuristic (car lst)) heuristic_value) (cons (car lst) (filter_elements (cdr lst) heuristic_value)))
      (else (filter_elements (cdr lst) heuristic_value)))))

; Add all the elements in a list one by one into a priority queue
(define pqueue_enqueue
  (lambda (to_insert_lst)
    (cond
      ((null? to_insert_lst) '())
      (else
        (set! pqueue (pqueue_enqueuer (car to_insert_lst) pqueue))
        (pqueue_enqueue (cdr to_insert_lst))))))

; Helper function for priority queue enqueue
(define pqueue_enqueuer
  (lambda (to_insert current_pqueue)
    (let ((node (car to_insert)) (depth (cadr to_insert)))
      (cond
        ((null? current_pqueue) (list to_insert))
        ; Comparing the heuristic of the node we'dl ike to insert and the current first node
        ((compare_heuristic to_insert (car current_pqueue)) (cons to_insert current_pqueue))
        ; If it's not the right place, move to the next node
        (else (cons (car current_pqueue) (pqueue_enqueuer to_insert (cdr current_pqueue))))))))

; Function to get the heuristic of a specific node
(define get_node_heuristic
  (lambda (node)
    (let ((xy_vals (car node)) (depth (cadr node)))  ; Split the node into xy vals and it's depth
      (+ (heuristic xy_vals) depth))))  ; Calculate the heuristic of the node

; Function to compare heuristic of two nodes
(define compare_heuristic
  (lambda (to_insert current_pqueue)
    (let ((inserted_node (car to_insert)) (inserted_depth (cadr to_insert)) (current_node (car current_pqueue)) (current_depth (cadr current_pqueue)))
      (< (get_node_heuristic to_insert) (get_node_heuristic current_pqueue)))))

(define heuristic
  (lambda (current_node)
    (let* ((current_x (car current_node)) (current_y (cadr current_node)) (goal_x (car goal)) (goal_y (cadr goal)))
      (+ (abs (- current_x goal_x)) (abs (- current_y goal_y))))))
