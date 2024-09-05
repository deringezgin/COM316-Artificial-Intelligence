(define path-lst '())
(define visited 1)

(define expand
  (lambda (point)
    (let ((lst (adjacentv point)))
      (set-lst-visited lst)
      (add-to-path-lst lst point)
      (push lst))))

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
    ;else
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
    (let ((next-robot (pop)))
      (cond
        ((null? next-robot) (display "No more paths to explore"))
        ((check-pt next-robot goal) (goal-found next-robot))
        ((>= count stop-count) (display "No more moves allowed"))
        (else
          (set! robot next-robot)
          (draw-moved-robot (car robot) (cadr robot))
          (search2 grid (+ count 1) stop-count))))))

(define check-pt
  (lambda (pt1 pt2)
    ((and (= (car pt1) (car pt2)) (= (cadr pt1) (cadr pt2))))))

(define goal-found
  (lambda (next-robot)
    (draw-moved-robot (car next-robot) (cadr next-robot))
    (let ((correct-path (get-path next-robot))) (draw-path correct-path))))

(define get-path
  (lambda (last-node)
    (cond
      ((null? last-node) '())
      (else (let ((parent (assoc last-node path-lst)))
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