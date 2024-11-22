(define path-lst '())

(define expand 
  (lambda (point)
    (let ((lst (adjacentv point)))
      (set-lst-visited lst)
      (add-to-path-lst lst point)
      (enqueue lst))))

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
    ;(display queue)
    (pause pause-num)
    (display count)
    (newline)
    (expand robot)
    (let ((next-robot (front)))
      (cond
        ((null? next-robot)
          (display "Cannot reach the goal")
          (newline))
        ((equal? next-robot goal)
          (set! robot (dequeue))
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
          (set! robot (dequeue))
          (draw-moved-robot (robot-x) (robot-y))
          (search2 grid (+ count 1) stop-count))))))
    
(define get-path
  (lambda (last-node)
    (if (equal? last-node start)
      (list start)
    ;else
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