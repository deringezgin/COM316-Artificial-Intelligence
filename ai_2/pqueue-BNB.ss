(define pqueue '())

(define pqueue_front
  (lambda ()
    (cond
      ((null? pqueue) '())
      (else (car pqueue)))))

(define pqueue_dequeue
  (lambda ()
    (cond
      ((null? pqueue) '())
      (else (let* ((front (pqueue_front))
                   (heuristic_value (heuristic front))
                   (same_heuristic (filter_elements pqueue heuristic_value))
                   (selected (random_select same_heuristic)))
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
      ((= (heuristic (car lst)) heuristic_value) (cons (car lst) (filter_elements (cdr lst) heuristic_value)))
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
    (cond
      ((null? current_pqueue) (list to_insert))
      ((compare_heuristic to_insert (car current_pqueue)) (cons to_insert current_pqueue))
      (else (cons (car current_pqueue) (pqueue_enqueuer to_insert (cdr current_pqueue)))))))

(define compare_heuristic
  (lambda (to_insert current)
    (< (heuristic to_insert) (heuristic current))))

(define heuristic
  (lambda (current)
    (block_distance current goal)))

(define block_distance
  (lambda (current goal)
    (let ((current_x (car current))
          (current_y (cadr current))
          (goal_x (car goal))
          (goal_y (cadr goal)))
      (+ (abs (- current_x goal_x))
         (abs (- current_y goal_y))))))
