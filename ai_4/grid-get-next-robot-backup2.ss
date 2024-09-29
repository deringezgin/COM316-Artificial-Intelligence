(define max-depth 3)
(define get-next-robot
  (lambda (point)
    (let* ((lst1 (cons point (adjacento point)))
           (lst0 (randomize lst1))
           (flst (calculate-h-goal lst0))
           (lst (map list flst lst0)))
      (display "CURRENT ROBOT: ")
      (display robot)
      (display "CURRENT GOAL: ")
      (display goal)
      (newline)
      (minimax point)
      (pause (* 100000 pause-num))
      )))

(define calculate-h-goal
  (lambda (lst)
    (map h-goal lst)))

(define get-adjacent
  (lambda (node)
    (append (list node) (adjacento node))))

(define heuristic
  (lambda (point1 point2)
    (+ (abs (- (car point1) (car point2))) (abs (- (cadr point1) (cadr point2))))))

;(define heuristic-lst
;  (lambda (lst)
;    (map (lambda (x) (heuristic x goal)) lst)))

(define calculate-lst-heuristic
  (lambda (lst)
    (map (lambda (x) (heuristic (car x) (cadr x))) lst)))

(define minimax
  (lambda (point)
    (define minimax-inner
      (lambda (current-list current-depth max-player)
        (cond
          ((= current-depth max-depth)
            (newline)
            (display " CURRENT LIST: ")
            (display current-list)
            (newline)
            (display "CURRENT DEPTH ")
            (display current-depth)
            (newline)
            (display "HEURISTIC: ")

            (cond
              ((even? current-depth)
                (display (list-sort heuristic-compare-min current-list))
                (newline)
                (car (list-sort heuristic-compare-min current-list)))
              (else
                (display (list-sort heuristic-compare-max current-list))
                (newline)
                (car (list-sort heuristic-compare-max current-list)))))
          (else (for-each minimax-inner
                  (map (lambda (x) (expand-max x)) current-list)
                  (make-list (length current-list) (+ current-depth 1)))))))
    (car (minimax-inner (list (list point goal)) 0))))

(define heuristic-compare-max
  (lambda (l1 l2)
    (let ((h1 (heuristic (car l1) (cadr l1))) (h2 (heuristic (car l2) (cadr l2))))
      (< h1 h2))))

(define heuristic-compare-min
  (lambda (l1 l2)
    (let ((h1 (heuristic (car l1) (cadr l1))) (h2 (heuristic (car l2) (cadr l2))))
      (> h1 h2))))

(define expand-max
  (lambda (gr-pair)
    (let* ((first (car gr-pair))
            (second (cadr gr-pair))
            (result (get-adjacent first)))
      (map (lambda (x) (append (list second) (list x))) result))))

;(define expand-helper
;  (lambda (first-element second-points)
;    (cond
;      ((null? second-points) '())
;      (else
;        (map (lambda (x) (reverse x)))))))

(define h-goal
  (lambda (point)
    (+ (abs (- (car point) (car robot)))
       (abs (- (cadr point) (cadr robot))))))