; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #2
; Due September 17 2024
; File that has the complete code for Branch and Bound Search
; I removed my comments from BFS to avoid having too many comments

(load "pqueue-BNB.ss")  ; This'll automatically load the Branch and Bound Search
(define path-lst '())
(define visited 1)

(define expand
  (lambda (point depth)
    (let ((lst (adjacentv point)))
      (set-lst-visited lst)
      (add-to-path-lst lst point)
      ; Before enqueue, I add the depth of the node to it so that we can enqueue them to the right place in pqueue
      (pqueue_enqueue (add_depth lst depth)))))

; Function to increase the depth level of a node
(define add_depth
  (lambda (lst depth)
    (cond
      ((null? lst) '())
      ; Here we create a new format for the nodes ((x y) d) so that our priority queue can find the right place to insert it
      (else (cons (list (car lst) (+ depth 1)) (add_depth (cdr lst) depth))))))

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
    (search2 grid 1 stop-count 0))) ; Our start node has a depth of 0

(define search2
  (lambda (grid count stop-count depth)
    (pause pause-num)
    (display count)
    (newline)
    (expand robot depth)  ; We call expand with the depth
    (let* ((next-robot-comb (pqueue_dequeue))
            (next-robot (car next-robot-comb))
            (next-depth (cadr next-robot-comb)))  ; We also get the depth of the next robot here so that we can increase the depth
      (cond
        ((null? next-robot) (display "No more paths to explore"))
        (else
          (draw-visited (car next-robot) (cadr next-robot))
          (draw-moved-robot (car next-robot) (cadr next-robot))))
      (cond
        ((check-pt next-robot goal) (goal-found next-robot))
        ((>= count stop-count) (display "No more moves allowed"))
        (else
          (set! robot next-robot)
          (search2 grid (+ count 1) stop-count next-depth))))))

(define check-pt
  (lambda (pt1 pt2)
    (and (= (car pt1) (car pt2)) (= (cadr pt1) (cadr pt2)))))

(define goal-found
  (lambda (next-robot)
    (let ((correct-path (get-path next-robot))) (draw-path correct-path))))

(define get-path
  (lambda (last-node)
    (cond
      ((null? last-node) '())
      (else
       (let ((parent (assoc last-node path-lst)))
             (cons last-node (get-path (cadr parent))))))))

(define draw-path
  (lambda (path)
    (cond
      ((not (null? path))
         (draw-pt-path-node (car path))
         (draw-path (cdr path))))))

(define draw-pt-path-node
  (lambda (point)
    (draw-path-node (car point) (cadr point))))