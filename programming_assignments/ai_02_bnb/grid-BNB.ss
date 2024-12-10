; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #2
; Due September 17 2024
; File that has the complete code for Branch and Bound Search
; I started from Prof. Parker's BFS despite my BFS works fine. His solution is way simpler and clean. If this is an issue, please lmk I implemented the same thing with my BFS, as well.
; I created a priority queue and changed the queue functions in this file with priority queue functions.
; The other thing I changed was in the search2 function. In the let part, I changed the front to dequeue.
; The reason for this is when there are multiple nodes with the same heuristic value, our robot makes a random choice.
; So the first node in the p.queue doesn't supposed to be the picked node by the dequeue.
; This file has all the code and loads the priority queue. You just have to change (load "grid-BFS") to (load "grid-BNB") in grid-main

(load "pqueue-BNB.ss")
(define path-lst '())
(define visited 1)

(define expand 
  (lambda (point)
    (let ((lst (adjacentv point)))
      (set-lst-visited lst)
      (add-to-path-lst lst point)
      ; Change normal queue function to prioirty queue
      (pqueue_enqueue lst))))

(define add-to-path-lst
  (lambda (lst point)
    (if (not (null? lst))
       (let ((child-parent (list (car lst) point)))
         (set! path-lst (cons child-parent path-lst))
         (add-to-path-lst (cdr lst) point)))))

(define set-lst-visited 
  (lambda (lst)
    (if (null? lst)
        '()
        (let ((x (car lst)))
          (draw-pt-frontier x)
          (block-set! x visited)
          (set-lst-visited (cdr lst))))))
  
(define draw-pt-frontier
  (lambda (pt)
    (draw-frontier (car pt) (cadr pt))))

(define search
  (lambda (grid stop-count)
    (block-set! start visited)
    (set! path-lst (list (list start '())))
    (search2 grid 1 stop-count)))

(define search2
  (lambda (grid count stop-count)
    (pause pause-num)
    (display count)
    (newline)
    (expand robot)
    (let ((next-robot (pqueue_dequeue)))  ; Made front to dequeue as dequeue is random and it is not equal to front
      (cond
        ((null? next-robot)
          (display "Cannot reach the goal")
          (newline))
        ((equal? next-robot goal)
          (set! robot next-robot)
          (draw-moved-robot (robot-x) (robot-y))
          (display "Found")
          (newline)
          (let ((path (get-path goal)))
            (draw-path path)
            (display path))
          (newline))
        ((>= count stop-count)
          (display "Took too long")
          (newline))
        (else
          (draw-visited (car robot) (cadr robot))
          (set! robot next-robot)
          (draw-moved-robot (robot-x) (robot-y))
          (search2 grid (+ count 1) stop-count))))))
    
(define get-path
  (lambda (last-node)
    (if (equal? last-node start)
      (list start)
      (let ((next-node (cadr (assoc last-node path-lst))))
        (append (get-path next-node) (list last-node))))))

(define draw-path
  (lambda (path)
    (cond 
      ((not (null? path))
         (draw-pt-path-node (car path))
         (draw-path (cdr path))))))

(define draw-pt-path-node
  (lambda (point)
    (draw-path-node (car point) (cadr point))))