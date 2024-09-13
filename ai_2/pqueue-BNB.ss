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

(define pqueue_dequeue
  (lambda ()
    (cond
      ((null? pqueue) '())
      (else (let* ((heuristic_value (get_node_heuristic (pqueue_front)))
                   (selected (random_select (filter_elements pqueue heuristic_value))))
              (set! pqueue (remove_element pqueue selected)) selected)))))

(define random_select
  (lambda (lst)
    (cond
      ((null? lst) '())
      (else (list-ref lst (random (length lst)))))))

(define remove_element
  (lambda (lst to_remove)
    (cond
      ((null? lst) '())
      ((equal? (car lst) to_remove) (remove_element (cdr lst) to_remove))
      (else (cons (car lst) (remove_element (cdr lst) to_remove))))))

(define filter_elements
  (lambda (lst heuristic_value)
    (cond
      ((null? lst) '())
      ((= (get_node_heuristic (car lst)) heuristic_value) (cons (car lst) (filter_elements (cdr lst) heuristic_value)))
      (else (filter_elements (cdr lst) heuristic_value)))))

(define pqueue_enqueue
  (lambda (to_insert_lst)
    (cond
      ((null? to_insert_lst) '())
      (else
        (set! pqueue (pqueue_enqueuer (car to_insert_lst) pqueue))
        (pqueue_enqueue (cdr to_insert_lst))))))

(define pqueue_enqueuer
  (lambda (to_insert current_pqueue)
    (let ((node (car to_insert)) (depth (cadr to_insert)))
      (cond
        ((null? current_pqueue) (list to_insert))
        ((compare_heuristic to_insert (car current_pqueue)) (cons to_insert current_pqueue))
        (else (cons (car current_pqueue) (pqueue_enqueuer to_insert (cdr current_pqueue))))))))

(define get_node_heuristic
  (lambda (node)
    (let ((xy_vals (car node)) (depth (cadr node)))
      (+ (heuristic xy_vals) depth))))

(define compare_heuristic
  (lambda (to_insert current_pqueue)
    (let ((inserted_node (car to_insert)) (inserted_depth (cadr to_insert)) (current_node (car current_pqueue)) (current_depth (cadr current_pqueue)))
      (< (get_node_heuristic to_insert) (get_node_heuristic current_pqueue)))))

(define heuristic
  (lambda (current_node)
    (let* ((current_x (car current_node)) (current_y (cadr current_node)) (goal_x (car goal)) (goal_y (cadr goal)))
      (+ (abs (- current_x goal_x)) (abs (- current_y goal_y))))))
