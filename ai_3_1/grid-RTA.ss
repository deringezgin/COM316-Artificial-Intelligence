(define pqueue '())
(define path-lst '())
(define goal-frontier '())
(define visited-nodes-lst '())
(define unexplored-frontiers '())
(define visited 1)

(newline)
(display "NEW RUNNNN")
(newline)
(define expand-rta
  (lambda (point)
    (let ((lst (adjacentv point)))
      (set-lst-visited lst)
      (add-to-frontiers lst)
      (set! unexplored-frontiers (list-sort compare-heuristic unexplored-frontiers)))))

(define pick-next-point (lambda () (random-select (filter is-min-heuristic unexplored-frontiers))))
(define get-min-heuristic (lambda () (heuristic (car unexplored-frontiers))))
(define is-min-heuristic (lambda (node) (= (heuristic node) (heuristic (car unexplored-frontiers)))))
(define add-to-frontiers (lambda (new-frontiers) (set! unexplored-frontiers (append new-frontiers unexplored-frontiers))))
(define compare-heuristic (lambda (p1 p2) (< (heuristic p1) (heuristic p2))))
(define heuristic (lambda (target) (+ (block-wise-distance target robot) (block-wise-distance target goal))))  ; YOU CHANGED CURRENT TO ROBOT

(define block-wise-distance
  (lambda (c t)
    (let ((c-x (car c)) (c-y (cadr c)) (t-x (car t)) (t-y (cadr t)))
      (+ (abs (- c-x t-x)) (abs (- c-y t-y))))))

(define remove-point
  (lambda (point lst)
    (cond
      ((null? lst) '())
      ((equal? point (car lst)) (cdr lst))
      (else (cons (car lst) (remove-point point (cdr lst)))))))

(define random-select (lambda (lst) (cond ((null? lst) '()) (else (list-ref lst (random (length lst)))))))

(define draw-frontiers
  (lambda (lst)
    (cond
      ((null? lst) '())
      (else
        (draw-pt-frontier (car lst))
        (draw-frontiers (cdr lst))))))

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
    (add-to-visited robot)
    (expand-rta robot)
    (let ((next-robot (pick-next-point)))
      (cond
        ((null? next-robot) (display "Cannot reach the goal") (newline))
        ((equal? next-robot goal)
          (pause pause-num)
          (display "FINAL COUNT:")
          (display (+ count 1))
          (newline)
          (set! robot next-robot)
          (draw-moved-robot (robot-x) (robot-y))
          (display "Found")
          (newline))
        ((>= count stop-count)
          (display "Took too long")
          (newline))
        (else
;          (move-robot-to-target robot next-robot count '())
          (set! goal-frontier next-robot)
          (if (not (member next-robot (adjacent robot))) (get-path-to-frontier))
          (set! robot next-robot)
          (pause pause-num)
          (draw-visited (car robot) (cadr robot))
          (draw-moved-robot (robot-x) (robot-y))
          (draw-frontiers (adjacentv robot))
          (set! unexplored-frontiers (remove-point robot unexplored-frontiers))
          (search2 grid (+ count 1) stop-count))))))


(define move-robot-to-path
  (lambda (directions)
    (display "CURRENT ROBOT: ")
    (display robot)
    (newline)
    (cond
      ((null? directions) (display "ROBOT IS ON THE FRONTIER!") (newline))
      ((equal? (car directions) robot) (move-robot-to-path (cdr directions)))
      (else
        (set! robot (car directions))
        (draw-moved-robot (robot-x) (robot-y))
        (move-robot-to-path (cdr directions))))))

(define get-path-to-frontier
  (lambda ()
    (set! pqueue '())
    (set! path-lst '())
    (display "HERE111")
    (let ((current (list (car robot) (cadr robot))))
      (frontier-finder current)
      (display "PATH_LST")
      (display path-lst)
      (newline)
      (move-robot-to-path (get-path goal-frontier current)))))

(define frontier-finder
  (lambda (current)
    (display "CURRENT: ")
    (display current)
    (newline)
    (display "HERE222")
    (expand-frontier-search current)
    (display "HERE333")
    (newline)
    (display "CURRENT Pqueue")
    (display pqueue)
    (let ((next-current (pqueue-dequeue)))
      (set! pqueue (remove-point next-current pqueue))
      (set! current next-current)
      (cond
        ((check-frontier current)
          (add-to-path-lst (list goal-frontier) current)
          (display "FRONTIER FOUND!"))
        (else (frontier-finder current))))))

(define check-frontier
  (lambda (current)
    (member current (adjacent goal-frontier))))

(define expand-frontier-search
  (lambda (point)
    (let ((lst (get-adjacent-points point)))
      (display "FOUND ADJACENT POINTS: ")
      (display lst)
      (add-to-path-lst lst point)
      (pqueue-enqueue lst))))

(define pqueue-enqueue
  (lambda (nodes)
    (set! pqueue (append pqueue nodes))
    (set! pqueue (sort-pqueue))))

(define sort-pqueue
  (lambda ()
    (let* ((compute-heuristic (lambda (node) (+ (block-wise-distance node robot) (block-wise-distance node goal-frontier)))))
      (list-sort (lambda (node1 node2) (< (compute-heuristic node1) (compute-heuristic node2))) pqueue))))

(define pqueue-dequeue
  (lambda ()
    (let* ((compute-heuristic (lambda (node) (+ (block-wise-distance node robot) (block-wise-distance node goal-frontier))))
           (first-heuristic (compute-heuristic (car pqueue)))
           (same-heuristic-nodes (filter (lambda (node) (= (compute-heuristic node) first-heuristic)) pqueue))
           (selected-node (random-select same-heuristic-nodes)))
      selected-node)))

(define get-adjacent-points
  (lambda (point)
    (let* ((adjacent-nodes (adjacent point))                       ; Get all adjacent nodes
           (steps (map (lambda (n) (step point n)) adjacent-nodes))  ; Compute steps
           (valid-steps (filter (lambda (step) (not (equal? step #f))) steps))  ; Filter out invalid steps
           (visited-nodes (filter (lambda (node) (member node visited-nodes-lst)) valid-steps)))  ; Check visited nodes
      visited-nodes)))  ; Return the list of visited nodes

(define add-to-visited
  (lambda (node)
    (set! visited-nodes-lst (cons node visited-nodes-lst))))

(define complete-blockwise
  (lambda (s t n)
    (+ 0 (block-wise-distance t n))))

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

(define get-path
  (lambda (last-node start-node)
    (if (equal? last-node start-node)
      (list start-node)
      (let ((next-node (cadr (assoc last-node path-lst))))
        (append (get-path next-node start-node) (list last-node))))))
