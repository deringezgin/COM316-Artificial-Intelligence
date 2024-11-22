; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #1
; Due September 9 2024
; File that has the complete code for Breadth First Search (BFS)

(define path-lst '())
(define visited 1)  ; Visited wasn't defined so I added this.

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

; Explanation of my addition to the search2 function
; First of all I changed front to dequeue because when I work on a node, I should remove it from the queue
; I added several conditions
;   1. First I check if there are paths to explore. If there's I'll draw the visited square and the robot. If not, I print an error message
;   2. Second, I check if we're on a goal-node. If so I call the goal-found function I created. I'll explain it below.
;   3. If the count (our limitor) more than the stop-count I display no more moves allowed.
;   4. In the else statement, I set the current robot as the next robot, and recursively call the function
(define search2
  (lambda (grid count stop-count)
    (pause pause-num)
    (display count)
    (newline)
    (expand robot)
    (let ((next-robot (dequeue)))
      (cond
        ((null? next-robot) (display "No more paths to explore"))
        (else
          (draw-visited (car next-robot) (cadr next-robot))
          (draw-moved-robot (car next-robot) (cadr next-robot))))
      (cond
        ((check-pt next-robot goal) (goal-found next-robot))
        ((>= count stop-count) (display "No more moves allowed"))
        (else
          (set! robot next-robot)
          (search2 grid (+ count 1) stop-count))))))

; Function to check if two points p1 (x y) and p2 (x y) are equal.
(define check-pt
  (lambda (pt1 pt2)
    (and (= (car pt1) (car pt2)) (= (cadr pt1) (cadr pt2)))))

; Function I call when the goal is found.
; I calculate the correct path using the get-path function and I draw the path using the draw-path function.
(define goal-found
  (lambda (next-robot)
    (let ((correct-path (get-path next-robot))) (draw-path correct-path))))

; Function that recursively constructs the path backwards.
; The base case is if the list is null. In this case I return an empty list.
; Otherwise I found the parent of the current node and cons it with the recursive call
; In the class, Prof. Parker said there's a specific function for this use case so I assume -and hope- using assoc is ok.
; It's a function in the book on page 165, a part of the 6th chapter.
(define get-path
  (lambda (last-node)
    (cond
      ((null? last-node) '())
      (else
       (let ((parent (assoc last-node path-lst)))
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