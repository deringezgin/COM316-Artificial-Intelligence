(define path-lst '())

(define expand 
  (lambda (point)
    (let* ((lst0 (adjacentv point))
           (lst (randomize lst0)))
      (set-lst-visited lst)
      (add-to-path-lst lst point)
      (push lst))))

(define randomize
  (lambda (lst)
    (display lst)
    (let ((size (length lst)))
      (display size)
      (cond 
        ((< size 2) lst)
        (else
          (let* ((node-num (random size))
                 (nodex (list-ref lst node-num))
                 (new-lst (removex node-num lst)))
            (display " ") (display node-num) 
            (display " ") (display nodex) 
            (display " ") (display new-lst)      
            (newline)
            (cons nodex (randomize new-lst))))))))

(define removex
  (lambda (num lst)
    (display "in remove")
    (display lst) (display " ") (display num) (newline)
    (if (= num 0)
      (cdr lst)
    ;else
      (cons (car lst) (removex (- num 1) (cdr lst)))))) 

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
    (let ((next-robot (top)))
      (cond
        ((null? next-robot)
          (display "Cannot reach the goal")
          (newline))
        ((equal? next-robot goal)
          (set! robot (pop))
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
          (set! robot (pop))
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