; Derin Gezgin | Russell Kosovsky | Jay Nash
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #4
; Due October 1 2024
; File that has the complete code for MiniMax

(define max-depth 5)  ; Please modify this variable to change how deep the minimax searches

(define get-next-goal (lambda (point) (minimax-big point)))  ; Function that returns the next node to go

(define minimax-big
  (lambda (point)
    ; MiniMax function we call. This function calls the helper MiniMax function on each adjacent node of the current goal.
    ; It sorts the values by their heuristic and returns the best option
    (let ((adjacents (get-adjacent point)))
      (set! adjacents (map (lambda (x) (minimax x)) adjacents))
      (caadar (list-sort (lambda (x y) (> (car x) (car y))) adjacents)))))

(define minimax
  (lambda (point)
    (define minimax-inner
      (lambda (node depth max-player)
        (cond
          ; If we're in the max-depth, return the heuristic value and the node itself.
          ((= depth 0) (list (main-heuristic (car node) (cadr node) depth) node))
          (else
            ; Otherwise, for each child node, call the heuristic function again. Depending on if we're looking for max/min, return that value.
            (let ((child-nodes (expand-max node max-player)))
              (cond
                (max-player (let ((c-val (list -1e9 '())))
                              (for-each (lambda (child) (let ((next-child (minimax-inner child (- depth 1) #f))) (cond ((< (car c-val) (car next-child)) (set! c-val next-child))))) child-nodes)
                              (list (car c-val) node)))
                ((not max-player) (let ((c-val (list 1e9 '())))
                                    (for-each (lambda (child) (let ((next-child (minimax-inner child (- depth 1) #t))) (cond ((> (car c-val) (car next-child)) (set! c-val next-child))))) child-nodes)
                                    (list (car c-val) node)))))))))
    (minimax-inner (list point robot) max-depth #t)))

(define expand-max
  (lambda (rg-pair max-player)
    ; Expand-max function that'll expand the goal/robot, depending on which layer we are in
    (let* ((robot (car rg-pair)) (goal (cadr rg-pair)))
      (cond
        (max-player (map (lambda (x) (append (list x) (list goal))) (get-adjacent robot)))
        (else (map (lambda (x) (append (list robot) (list x))) (get-adjacent goal)))))))

(define get-adjacent (lambda (node) (append (list node) (adjacento node))))  ; Function that returns the result of adjacento and the current node

(define heuristic-euclidian (lambda (point1 point2) (sqrt (+ (sqr (- (car point1) (car point2))) (sqr (- (cadr point1) (cadr point2)))))))

(define sqr (lambda (x) (* x x)))

(define main-heuristic
  (lambda (point1 point2 depth)
    ; Our main heuristic function. It returns the euclidian-distance.
    ; If robot is enough close to the goal, return -1 to completely penalize that possiblity
    (let* ((robot-xy (if (even? depth) point2 point1))
           (goal-xy (if (even? depth) point1 point2))
           (euclidian-difference (heuristic-euclidian robot-xy goal-xy))
           (goal-x (car goal-xy)) (goal-y (cadr goal-xy)))
      (cond
        ((<= euclidian-difference 2) -1)
        (else euclidian-difference)))))
