; grid-search.ss

(define none -1)
(define n 0)
(define s 1)
(define e 2)
(define w 3)

(define search
  (lambda (grid stop-count)
    (search2 grid 1 stop-count)))


(define search2
  (lambda (grid count stop-count)
    (pause pause-num)
    (let ((x (robot-x))
          (y (robot-y)))
      (display count)
      (newline)
      (draw-visited x y)
      (set-node! grid x y visited)
      (move-robot grid x y 0)
      (draw-moved-robot (robot-x) (robot-y))
      (if (or
            (and (= x (robot-x)) (= y (robot-y)))
            (equal? robot goal)
            (>= count stop-count))
         #f
       ;else
         (search2 grid (+ count 1) stop-count)))))


(define len
  (lambda (lst)
    (cond
      ((null? lst) 0)
      (else (+ 1 (len (cdr lst)))))))


(define find_element
  (lambda (lst index)
    (cond
      ((= index 0) (car lst))
      (else (find_element (cdr lst) (- index 1))))))


(define move-robot
  (lambda (grid x y count)
    (let ((free_direction (check_cells grid x y free)))
      (if (not free_direction) (set! free_direction (check_cells grid x y visited)) "no move")
      (cond
        ((= free_direction n) (set! robot (list (- x 1) y)))
        ((= free_direction s) (set! robot (list (+ x 1) y)))
        ((= free_direction e) (set! robot (list x (+ y 1))))
        ((= free_direction w) (set! robot (list x (- y 1))))))))


(define check_cells
  (lambda (grid x y target)
    (let ((free_cells '()))
      (if (and
            (> x 0)
            (< (get-node grid (- x 1) y) obstacle)
            (<= (get-node grid (- x 1) y) target))
        (set! free_cells (cons 0 free_cells)))
      (if (and
            (< x (- num-col-row 1))
            (< (get-node grid (+ x 1) y) obstacle)
            (<= (get-node grid (+ x 1) y) target))
        (set! free_cells (cons 1 free_cells)))
      (if (and
          (< y (- num-col-row 1))
          (< (get-node grid x (+ y 1)) obstacle)
          (<= (get-node grid x (+ y 1)) target))
        (set! free_cells (cons 2 free_cells)))
      (if (and
            (> y 0)
            (< (get-node grid x (- y 1)) obstacle)
            (<= (get-node grid x (- y 1)) target))
        (set! free_cells (cons 3 free_cells)))
      (if (not (null? free_cells)) (find_element free_cells (random (len free_cells))) #f))))


(define pause
  (lambda (count)
    (if (<= count 0)
       0
     ;else
       (pause (- count 1)))))
