(define path-lst '())
(define current start)
(define unexplored-frontiers '())
(define visited 1)

(define expand 
  (lambda (point)
    (let ((lst (adjacentv point)))
      (set-lst-visited lst)
      (add-to-frontiers lst)
      (add-to-path-lst lst point)
      (set! current point)
      (set! unexplored-frontiers (list-sort compare-heuristic unexplored-frontiers))
      (enqueue lst))))

(define pick-next-point
  (lambda ()
    (random-select (filter is-min-heuristic unexplored-frontiers))))

(define get-min-heuristic
  (lambda ()
    (heuristic (car unexplored-frontiers))))

(define is-min-heuristic
  (lambda (node)
    (= (heuristic node) (heuristic (car unexplored-frontiers)))))

(define add-to-frontiers
  (lambda (new-frontiers)
    (set! unexplored-frontiers (append new-frontiers unexplored-frontiers))))

(define compare-heuristic
  (lambda (p1 p2)
    (< (heuristic p1) (heuristic p2))))

(define heuristic
  (lambda (target)
    (+ (block-wise-distance target current) (block-wise-distance target goal))))

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

(define random-select
  (lambda (lst)
    (cond
      ((null? lst) '())
      (else (list-ref lst (random (length lst)))))))

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
    (display "START: ")
    (display start)
    (display "END: ")
    (display goal)
    (block-set! start visited)
    (set! path-lst (list (list start '())))
    (search2 grid 1 stop-count)))

(define search2
  (lambda (grid count stop-count)

    (newline)
    (expand robot)
    (let ((next-robot (pick-next-point)))
      (cond
        ((null? next-robot) (display "Cannot reach the goal") (newline))
        ((equal? next-robot goal)
          (pause pause-num)
          (set! robot next-robot)
          (draw-moved-robot (robot-x) (robot-y))
          (display "Found")
          (newline)
          (let ((path (get-path goal start)))
            (draw-path path)
            (display path))
          (newline))
        ((>= count stop-count)
          (display "Took too long")
          (newline))
        (else
          (move-robot-to-target robot next-robot count '())
          (display "OUT FROM HERE")
          (newline)
          (draw-visited (car robot) (cadr robot))
          (draw-moved-robot (robot-x) (robot-y))
          (draw-frontiers (adjacentv robot))
          (set! unexplored-frontiers (remove-point robot unexplored-frontiers))
          (search2 grid (+ count 1) stop-count))))))

(define move-robot-to-target
  (lambda (start-node target count visited-nodes)
    (display "COUNT: ")
    (display count)
    (newline)
    (pause pause-num)
    (display "MOVING THE ROBOT TO:")
    (display target)
    (newline)
    (display "CURRENT PATH: ")
    (display path-lst)
    (newline)
    (display "TRY GET PATH: ")
    (assoc start-node path-lst)
    (newline)
    (let* ((adjacent-nodes (adjacent robot))
            (legal-steps (map (lambda (n) (step robot n)) adjacent-nodes))
            (non-f-steps (filter (lambda (step) (and (not (equal? step #f)))) legal-steps))
            (unvisited-steps (filter (lambda (step) (and (not (member step visited-nodes)))) non-f-steps))
            (non-null (if (null? unvisited-steps) non-f-steps unvisited-steps))
            (sorted-steps (sort (lambda (n1 n2) (< (complete-blockwise robot target n1) (complete-blockwise robot target n2))) non-null)))
      (display "CURRENT ROBOT: ")
      (display robot)
      (newline)
      (display "NON-NULL:  ")
      (display non-null)
      (newline)
      (display "SORTED STEPS: ")
      (display sorted-steps)
      (set! robot (car sorted-steps))
      (draw-moved-robot (robot-x) (robot-y))
      (draw-visited (car robot) (cadr robot)))
    (cond
      ((not (equal? target robot)) (move-robot-to-target start target (+ count 1) (cons robot visited-nodes)))
      (else '()))))

;(define move-robot-to-target
;  (lambda (start-node target count)
;    (pause pause-num)
;    (display "CURRENT ROBOT: ")
;    (display robot)
;    (newline)
;    (display "MOVING THE ROBOT TO:")
;    (display target)
;    (newline)
;    (display "TRY GET PATH: ")
;    (display (assoc robot path-lst))
;    (newline)
;
;    (display "ADJACENT NODES TO THE ROBOT: ")
;    (display (adjacent robot))
;    (newline)
;    (let* ((adjacent-nodes (assoc robot path-lst))
;            (not-null-adjacent (filter (lambda (node) (and (not (equal? node '())) (not (equal? node robot)))) adjacent-nodes))
;            (sorted-steps (sort (lambda (n1 n2) (< (complete-blockwise robot target n1) (complete-blockwise robot target n2))) not-null-adjacent))
;            (final-steps (if (member target (adjacent robot)) (list target) sorted-steps)))
;      (display "SORTED STEPS: ")
;      (display final-steps)
;      (newline)
;      (newline)
;      (set! robot (car final-steps))
;      (draw-moved-robot (robot-x) (robot-y))
;      (draw-visited (car robot) (cadr robot)))
;    (cond
;      ((not (equal? target robot)) (move-robot-to-target start target (+ count 1)))
;      (else '()))))

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
(define draw-path
  (lambda (path)
    (cond
      ((not (null? path))
         (draw-pt-path-node (car path))
         (draw-path (cdr path))))))
(define draw-pt-path-node
  (lambda (point)
    (draw-path-node (car point) (cadr point))))