(define max-depth 4)

(define get-next-goal
  (lambda (point)
      (minimax-big point)))

(define minimax-big
  (lambda (point)
    (let ((adjacents (get-adjacent point)))
      (set! adjacents (map (lambda (x) (minimax x)) adjacents))
      (caadar (list-sort (lambda (x y) (> (car x) (car y))) adjacents)))))

(define minimax
  (lambda (point)
    (define minimax-inner
      (lambda (node depth max-player)
        (cond
          ((= depth 0) (list (main-heuristic (car node) (cadr node) depth) node))
          (else
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
    (let* ((robot (car rg-pair)) (goal (cadr rg-pair)))
      (cond
        (max-player (map (lambda (x) (append (list x) (list goal))) (get-adjacent robot)))
        (else (map (lambda (x) (append (list robot) (list x))) (get-adjacent goal)))))))

(define get-adjacent (lambda (node) (append (list node) (adjacento node))))

(define heuristic-block (lambda (point1 point2) (+ (abs (- (car point1) (car point2))) (abs (- (cadr point1) (cadr point2))))))

(define heuristic-euclidian (lambda (point1 point2) (sqrt (+ (sqr (- (car point1) (car point2))) (sqr (- (cadr point1) (cadr point2)))))))

(define main-heuristic
  (lambda (point1 point2 depth)
    (let* ((robot-xy (if (even? depth) point2 point1))
           (goal-xy (if (even? depth) point1 point2))
           (euclidian-difference (heuristic-euclidian robot-xy goal-xy))
           (goal-x (car goal-xy)) (goal-y (cadr goal-xy)))
      euclidian-difference)))

(define sqr (lambda (x) (* x x)))
