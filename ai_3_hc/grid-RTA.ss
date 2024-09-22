; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #3
; Due September 24 2024
; File that has the complete code for Real-Time A* Search

(define pqueue '())  ; Priority queue I use while going from one frontier to another
(define path-lst '())  ; List that I use to store (parent.child) pairs
(define goal-frontier '())  ; Variable I keep the goal frontier
(define visited-nodes-lst '())  ; List that I keep track of the visited nodes
(define unexplored-frontiers '())  ; List that I store the unexplored frontiers
(define explored-in-frontier-search '())  ; List I store the explored nodes when I'm moving one frontier to other
(define rta-count 0)  ; Counter to keep track of the step count of the robot
(define visited 1)

(define expand-rta
  (lambda (point)
    ; I modified the existing expand function for Real-Time Search
    (let ((new-frontiers (adjacentv point)))
      (set-lst-visited new-frontiers)  ; Setting the frontiers visited as usual
      ; Adding the new frontiers to the unexplored frontiers list and sorting it by heuristic value
      (set! unexplored-frontiers (list-sort (lambda (p1 p2) (< (heuristic p1) (heuristic p2))) (append new-frontiers unexplored-frontiers))))))

; Function to pick a random point among all the points with the same heuristic value in a list of points
(define pick-next-point (lambda () (random-select (filter (lambda (node) (= (heuristic node) (heuristic (car unexplored-frontiers)))) unexplored-frontiers))))
; Heuristic function to calculate the block-wise distance between the target & current robot and target & the goal
(define heuristic (lambda (target) (+ (block-wise-distance target robot) (block-wise-distance target goal))))
; Function to calculate the block-wise distance between two nodes
(define block-wise-distance (lambda (c t) (+ (abs (- (car c) (car t))) (abs (- (cadr c) (cadr t))))))
; Randomly select an element from the list and return it
(define random-select (lambda (lst) (cond ((null? lst) '()) (else (list-ref lst (random (length lst)))))))

(define remove-point
  (lambda (point lst)
    ; Function to remove a point from the list and return the updated list
    (cond
      ((null? lst) '())
      ((equal? point (car lst)) (cdr lst))
      (else (cons (car lst) (remove-point point (cdr lst)))))))

(define draw-frontiers
  (lambda (lst)
    (cond
      ((null? lst) '())
      (else
        (draw-frontier (car (car lst)) (cadr (car lst)))
        (draw-frontiers (cdr lst))))))

(define search-rta
  (lambda (grid stop-count)
    ; Main search function
    (display "REAL-TIME A* SEARCH")
    (newline)
    (block-set! start visited)
    (search2 grid stop-count)))

(define search2
  (lambda (grid stop-count)
    (set! visited-nodes-lst (cons robot visited-nodes-lst))  ; Adding the robot to visited nodes
    (expand-rta robot)  ; Expanding to frontiers
    (let ((next-robot (pick-next-point)))  ; Getting the next point from the frontiers list
      (cond
        ((null? next-robot) (display "Cannot reach the goal") (newline))  ; If no options it means that we can't reach the goal
        ((equal? next-robot goal)  ; If the next robot is the goal, we found it!
          (pause pause-num)
          ; Move the robot to the goal and draw it
          (set! robot next-robot)
          (draw-moved-robot (robot-x) (robot-y))
          (increment-count)
          (display "FOUND!")
          (newline)
          (display "FINAL MOVE COUNT:")
          (display rta-count)
          (newline))
        ((>= rta-count stop-count) (display "Took too long") (newline))
        (else
          (set! goal-frontier next-robot)  ; Setting the next-robot as frontier
          (cond
            ; If the node we want to go is not the adjacent of the robot, go to the frontier using the move-to-frontier function
            ((not (member next-robot (adjacent robot))) (move-to-frontier))
            (else
              ; Otherwise move the robot directly
              (set! robot next-robot)
              (increment-count)
              (pause pause-num)))
          ; Draw the moved robot and visited
          (draw-visited (car robot) (cadr robot))
          (draw-moved-robot (robot-x) (robot-y))
          (draw-frontiers (adjacentv robot))  ; Draw the frontiers
          ; Remove the current robot from the unexplored frontiers and update the list
          (set! unexplored-frontiers (remove-point robot unexplored-frontiers))
          (search2 grid stop-count))))))

(define move-to-frontier
  (lambda ()
    ; Function to move the robot to the goal frontier
    (set! pqueue '())  ; Reseting the priority queue
    (set! path-lst '())  ; Reseting the path list
    (set! explored-in-frontier-search '())  ; Reseting the list that saves the explored nodes in frontier search
    (let ((current (list (car robot) (cadr robot))))  ; Copying the robot so any modification we do won't move the robot itself
      (frontier-finder current)  ; Find the goal frontier starting from the current node
      ; Recursively call the function
      (move-robot-to-path (get-path goal-frontier current)))))

(define frontier-finder
  (lambda (current)
    ; Function that'll take the current node we're in and
    (expand-frontier-search current)  ; Expand the current robot
    (let ((next-current (pqueue-dequeue)))  ; Get the next node to go from the pqueue
      (set! pqueue (remove-point next-current pqueue))  ; Update the priority queue
      (set! current next-current)  ; Update the current node
      (cond
        ((member current (adjacent goal-frontier))  ; If the goal-frontier is next to current, WE FOUND THE FRONTIER!
          (add-to-path-lst (list goal-frontier) current))  ; Add the frontier to the path-lst
        ; Otherwise called the function again
        (else (frontier-finder current))))))

(define expand-frontier-search
  (lambda (point)
    ; Expand the current node to adjacent nodes
    (let ((lst (get-adjacent-points point)))
      (add-to-path-lst lst point)  ; Add the new point to the path-lst
      (set! explored-in-frontier-search (append lst explored-in-frontier-search))  ; Updated the explored nodes in the frontier search
      (pqueue-enqueue lst))))

(define get-adjacent-points
  (lambda (point)
    ; Special function I defined that'll take the adjacent nodes of a node.
    ; In this function it'll only return the adjacent nodes of the point that are
    ; 1. A member of the previously visited nodes by the robot
    ; 2. NOT a member of the previously explored nodes by the robot in the search process
    (let* ((steps (map (lambda (n) (step point n)) (adjacent point)))
           (valid-steps (filter (lambda (step) (not (equal? step #f))) steps))
           (visited-nodes (filter (lambda (node) (and (member node visited-nodes-lst) (not (member node explored-in-frontier-search)))) valid-steps)))
      visited-nodes)))

(define move-robot-to-path
  (lambda (directions)
    ; After I found the path to frontier, this function moves the robot through the path
    (draw-moved-robot (robot-x) (robot-y))  ; Draw the robot
    (pause pause-num)  ; Pause
    (cond
      ; If no more directions left, return
      ((null? directions) '())
      ; If it's already the robot, you don't have to move it
      ((equal? (car directions) robot) (move-robot-to-path (cdr directions)))
      ; Move robot and call it again
      (else (set! robot (car directions)) (increment-count) (move-robot-to-path (cdr directions))))))

(define pqueue-enqueue
  (lambda (nodes)
    ; Add the nodes to pqueue and sort it again
    (set! pqueue (custom-append pqueue nodes))
    (set! pqueue (sort-pqueue))))

(define custom-append
  (lambda (pqueue nodes)
    ; Function to append a node to priority queue only if it's not in the pqueue
    (cond
      ((null? nodes) pqueue)
      ((member (car nodes) pqueue) (custom-append pqueue (cdr nodes)))
      (else (custom-append (cons (car nodes) pqueue) (cdr nodes))))))

(define sort-pqueue
  (lambda ()
    ; Sort the priority queue based on the heuristic value
    (let* ((compute-heuristic (lambda (node) (+ (block-wise-distance node robot) (block-wise-distance node goal-frontier)))))
      (list-sort (lambda (node1 node2) (< (compute-heuristic node1) (compute-heuristic node2))) pqueue))))

(define pqueue-dequeue
  (lambda ()
    ; Dequeue an element from the priority queue
    (let* ((compute-heuristic (lambda (node) (+ (block-wise-distance node robot) (block-wise-distance node goal-frontier))))
           (first-heuristic (compute-heuristic (car pqueue)))
           (same-heuristic-nodes (filter (lambda (node) (= (compute-heuristic node) first-heuristic)) pqueue))
           (selected-node (random-select same-heuristic-nodes)))
      selected-node)))

(define add-to-path-lst
  (lambda (lst point)
    ; Add list of points to path list. They all have a parent of point
    (if (not (null? lst))
       (let ((child-parent (list (car lst) point)))
         (set! path-lst (cons child-parent path-lst))
         (add-to-path-lst (cdr lst) point)))))

(define set-lst-visited
  (lambda (lst)
    ; Set a complete list visited and draw their frontiers
    (if (null? lst) '()
        (let ((x (car lst)))
          (draw-frontier (car x) (cadr x))
          (block-set! x visited)
          (set-lst-visited (cdr lst))))))

(define get-path
  (lambda (last-node start-node)
    ; Get-path from the path list that we created while going from one frontier to other
    (if (equal? last-node start-node)
      (list start-node)
      (let ((next-node (cadr (assoc last-node path-lst))))
        (append (get-path next-node start-node) (list last-node))))))

(define increment-count
  (lambda ()
    ; Function to update and print the counter that keeps track of the length of the robot's track
    (set! rta-count (+ rta-count 1))
    (display "Current Move Count for RTA*: ")
    (display rta-count)
    (newline)))