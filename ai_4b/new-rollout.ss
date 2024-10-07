(define copy-grid (lambda (original-grid) (let ((rows (vector-length original-grid))) (copy-helper original-grid (make-vector rows) 0))))
(define copy-helper (lambda (original-grid new-grid i) (let ((rows (vector-length original-grid))) (cond ((< i rows) (vector-set! new-grid i (copy-row (vector-ref original-grid i))) (copy-helper original-grid new-grid (+ i 1))) (else new-grid)))))
(define copy-row (lambda (row) (let* ((cols (vector-length row)) (new-row (make-vector cols))) (copy-element row new-row 0 cols))))
(define copy-element (lambda (row new-row j cols) (cond ((>= j cols) new-row) (else (vector-set! new-row j (vector-ref row j)) (copy-element row new-row (+ j 1) cols)))))

(define rollout
  (lambda (state)
    (let ((goal (car state)) (robot (cadr state)))
      (display "CURRENT GOAL ")
      (display goal)
      (newline)
      (display "POSSIBLE MOVES ")
      (display (get-adjacent goal))
      (newline)
      (display "RANDOM MOVE ")
      (display (get-random-move goal))
      )))


(define get-adjacent (lambda (point) (append (list point) (adjacento point))))

(define get-random-move
  (lambda (point)
    (let ((possible-moves (get-adjacent point)))
      (list-ref possible-moves (random (length possible-moves))))))