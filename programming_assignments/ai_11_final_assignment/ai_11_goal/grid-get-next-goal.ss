; Derin Gezgin | Russell Kosovsky | Jay Nash | Sarah Goyette
; COM316: Artificial Intelligence | Fall 2024
; Final Programming Assignment
; Due December 5th, 2024
; File that has the complete code for MCTS - Goal - For the Final Project


;;;;;;;;;;;;;;;;;;;; HYPERPARAMETERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define gmax-rollout-depth 50)  ; How far a single rollout will go
(define gsingle-rollout-count 200)  ; How many rollouts a leaf node will perform before starting the backpropagation
(define gstart-time 0)
(define gmax-time 3)  ; Maximum number of seconds the MCTS can think
(define gconst-exp 2)  ; Exploration constant


;;;;;;;;;;;;;;;;;;;; MAIN FUNCTION TO GET THE NEXT GOAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Calling the MCTS in the current board state
(define get-next-goal
  (lambda (point)
    (let ((b-point (calc-dist goal robot)))
      (cond
      ((not (eq? b-point -1)) (gbuild (list b-point)) point)
      (else
        (let ((new-location (cadr (gmcts-search (list #t goal robot)))))
          (cond
            ((eq? point new-location)  ; If the point is our current location, blast all 4 directions
            (gblast-real (adjacent new-location))))
          new-location))))))

(define gbuild
  (lambda (lst)
    (if (not (null? lst))
      (let* ((blst (randomize lst))
             (pt (car blst))
             (x (car pt))
             (y (cadr pt)))
        (cond
          ((= (get-node grid x y) free)
            (set-node! grid x y obstacle)
            (send canvas make-obstacle x y))
          (else
            (gbuild (cdr blst))))))))

(define calc-dist
  (lambda (p1 p2)
    (let* ((p1-x (car p1)) (p1-y (cadr p1)) (p2-x (car p2)) (p2-y (cadr p2)))
      (cond
        ((and (eq? (+ p1-x 2) p2-x) (eq? p1-y p2-y)) (list (+ p1-x 1) p1-y))
        ((and (eq? (- p1-x 2) p2-x) (eq? p1-y p2-y)) (list (- p1-x 1) p1-y))
        ((and (eq? (+ p1-y 2) p2-y) (eq? p1-x p2-x)) (list p1-x (+ p1-y 1)))
        ((and (eq? (- p1-y 2) p2-y) (eq? p1-x p2-x)) (list p1-x (- p1-y 1)))
        (else -1)))))

;;;;;;;;;;;;;;;;;;;; STRUCTURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; We found this function definition from page 318 of the book. It allows us to create structures with fields for easy representation
(define-syntax struct
  (lambda (x)
    (define gen-id
      (lambda (template-id . args)
        (datum->syntax template-id
          (string->symbol
            (apply string-append
              (map
                (lambda (x)
                    (if (string? x)
                      x
                      (symbol->string (syntax->datum x))))
                    args))))))
      (syntax-case x ()
        [(_ name field ...)
          (with-syntax
            ([constructor (gen-id #'name "make-" #'name)]
             [predicate (gen-id #'name #'name "?")]
             [(access ...)
             (map
              (lambda (x)
                (gen-id x #'name "-" x)) #'(field ...))]
             [(assign ...)
              (map
                (lambda (x)
                  (gen-id x "set-" #'name "-" x "!")) #'(field ...))]
                  [structure-length (+ (length #'(field ...)) 1)]
                  [(index ...)
                    (let f ([i 1] [ids #'(field ...)])
                      (if (null? ids)
                        '()
                        (cons i (f (+ i 1) (cdr ids)))))])
            #'(begin
                (define constructor
                  (lambda (field ...)
                    (vector 'name field ...)))
                (define predicate
                  (lambda (x)
                    (and
                      (vector? x)
                      (= (vector-length x) structure-length)
                        (eq? (vector-ref x 0) 'name))))
                (define access
                  (lambda (x)
                    (vector-ref x index))) ...
                (define assign
                  (lambda (x update)
                    (vector-set! x index update))) ...))])))

(struct gtree root children)  ; This is our tree structure which has a root node and its children
(struct gnode state t0 t n ucb node-id blastList)  ; This is the node structure which has the necessarry fields for the MCTS

(define gtree '())  ; Our MCTS tree


;;;;;;;;;;;;;;;;;;;; MATH FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define gid-count 0)  ; ID variable that assigns a unique id to nodes

(define gid
	(lambda ()
    ; Function to get the next available ID
		(set! gid-count (+ gid-count 1)) (- gid-count 1)))

(define gUCB
	(lambda (eval current-eval-count parent-eval-count)
    ; Function that returns the UCB of a specific node
		(if (or (= current-eval-count 0) (= parent-eval-count 0))
			1e9
			(let* ((vi-bar (/ eval current-eval-count))
					(c gconst-exp)
					(exp-term (sqrt (/ (log parent-eval-count) current-eval-count))))
				(+ vi-bar (* c exp-term))))))

(define greturn-best-move
	(lambda (current-tree)
    ; Function that returns the state of the node with the highest UCB
		(let ((children (gtree-children current-tree)))
			(car (list-sort (lambda (c1 c2) (> (gnode-ucb (gtree-root c1)) (gnode-ucb (gtree-root c2)))) children)))))


;;;;;;;;;;;;;;;;;;;; TREE FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define gexpand
	(lambda (current-root-tree)
    ; Setting the children of a tree the newly generated children (which are also trees) from the gcreate-children function
		(set-gtree-children! current-root-tree (gcreate-children current-root-tree))))

(define gcreate-children
	(lambda (root)
    ; Function to generate a children from a root. A child is also a tree with many a root and an empty children list
    ; If the child is the same point as its parent, this means that it is going to blast.
    ; To keep track of the blasted nodes throughout the MCTS tree we keep track of a list of blasted nodes which also
    ; carries through the children of the parent.
		(let* ((adjacents (gexpand-max (gnode-state (gtree-root root))))
            (previous-state (gnode-state (gtree-root root))) (previous-turn (car previous-state))
            (previous-goal (cadr previous-state)) (previous-robot (caddr previous-state)))
			(map
        (lambda (state)
          (let ((previous-blastList (gnode-blastList (gtree-root root))))
            (cond
            (previous-turn (cond ((eq? previous-goal (cadr state)) (set! previous-blastList (append (list previous-goal) previous-blastList)))))
            (else (cond ((eq? previous-robot (caddr state)) (set! previous-blastList (append (list previous-robot) previous-blastList))))))
          (make-gtree (make-gnode state 0 0 0 1e9 (gid) previous-blastList) '())))
        adjacents))))

(define gexpand-max
	(lambda (tgr-pair)
    ; Function that takes a (turn goal robot) pair and expands the relevant player depending on whose turn it is
		(let* ((turn (car tgr-pair))
						(goal (cadr tgr-pair))
						(robot (caddr tgr-pair)))
					(cond
						(turn
							(map (lambda (x) (append (list (not turn)) (list x) (list robot))) (gget-adjacent goal)))
					(else
						(map (lambda (x) (append (list (not turn)) (list goal) (list x))) (gget-adjacent robot)))))))


;;;;;;;;;;;;;;;;;;;; GRID FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define gshuffle-lst
	(lambda (lst)
    ; Simple function to shuffle a list
		(cond
			((null? lst) '())
			(else
				(let ((picked-element (list-ref lst (random (length lst)))))
					(cons picked-element (gshuffle-lst (gremove-element lst picked-element))))))))

(define gremove-element
	(lambda (lst item)
    ; Simple function to remove an element from a list
		(cond ((null? lst) '()) ((equal? (car lst) item) (cdr lst)) (else (cons (car lst) (gremove-element (cdr lst) item))))))

(define gget-adjacent
	(lambda (point)
    ; Function to get the adjacento and the current node
		(gshuffle-lst (append (list point) (gadjacento grid point)))))

(define gget-random-move
	(lambda (grid-copy point)
    ; Function to get a random move from a point among all the possible moves
		(let ((possible-moves (append (list point) (gadjacento grid-copy point))))
			(list-ref possible-moves (random (length possible-moves))))))

; We rewrite some of the built-in functions as the grid we are working on can change depending on the blasted squares
(define gadjacento
  (lambda (grid-copy block)
    (let* ((adj-lst0 (gadjacent block))
           (adj-lst1 (map (lambda (z) (gstepo grid-copy block z)) adj-lst0)))
      (remove-f adj-lst1))))

(define gadjacent
  (lambda (block)
    (let ((x (car block))
          (y (cadr block)))
      (append
        (if (< y 1) '() (list (list x (- y 1))))
        (if (< x 1) '() (list (list (- x 1) y)))
        (if (>= y (- num-col-row 1)) '() (list (list x (+ y 1))))
        (if (>= x (- num-col-row 1)) '() (list (list (+ x 1) y)))))))

(define gstepo
  (lambda (grid-copy b c)
    (let ((b-status (gblock-status grid-copy b))
          (c-status (gblock-status grid-copy c))
          (x-diff (abs (- (car b) (car c))))
          (y-diff (abs (- (cadr b) (cadr c)))))
      (if (or (= b-status obstacle)
              (= c-status obstacle)
              (not (= (+ x-diff y-diff) 1)))
          #f
      ;else
          c))))

(define gblock-status
  (lambda (grid-copy block)
    (get-node grid-copy (car block) (cadr block))))


;;;;;;;;;;;;;;;;;;;; SEARCH FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define gmcts-search
  (lambda (root)
    ; Our main MCTS search function. It logs the start time, creates a new tree and calls the helper function
    (let ((root-node (make-gnode root 0 0 0 -1 (gid) '())))
      (set! gstart-time (time-second (current-time)))
      (set! gtree (make-gtree root-node '()))
      (gexpand gtree)
      (gnode-state (gtree-root (gmcts 0))))))

(define gmcts
  (lambda (count)
    ; The helper mcts function.
    (define mcts-inner
      (lambda (current-tree parent-eval-count)
        (cond
          ((null? (gtree-children current-tree))  ; If it is a leaf
            (cond
              ((= 0 (gnode-n (gtree-root current-tree)))  ; Check if it is never explored. If so, perform rollout
                (set-gnode-t0! (gtree-root current-tree)
                  (gperform-rollout (gnode-state (gtree-root current-tree)) 0 (gnode-blastList (gtree-root current-tree)))))
              (else (gexpand current-tree))))  ; If it was explored, expand it
          (else (mcts-inner (greturn-best-move current-tree) (gnode-n (gtree-root current-tree)))))  ; Otherwise call the function again
        (gupdate current-tree parent-eval-count)))  ; When a single tour is done, update the value
    (cond
      ; After each iteration, check for the time limit and if we're over the limit return the best move
      ((>= (- (time-second (current-time)) gstart-time) gmax-time) (greturn-best-move gtree))
      (else (mcts-inner gtree 0) (gmcts (+ count 1))))))  ; This is the recursive call

(define gperform-rollout
  (lambda (state current-rollout-count blast-list)
    ; Function that performs multiple rollouts and take a sum of the scores
    (cond
      ((eq? current-rollout-count gsingle-rollout-count) 0)
      (else (+ (grollout state 0 blast-list) (gperform-rollout state (+ current-rollout-count 1) blast-list))))))

(define grollout
  (lambda (state current-count blast-list)
    ; Function that performs rollout using the helper inner function.
    (define grollout-inner
      (lambda (state current-count grid-copy)
        ; Helper function to perform a single rollout until the end condition.
        ; This also blasts the nodes in the used grid for a better estimate.
        (cond
          ((gis-caught? state) -1)
          ((equal? gmax-rollout-depth current-count) 1)
        (else
          (let ((turn (car state)) (current-goal (cadr state)) (current-robot (caddr state)))
          (cond
            (turn
              (let ((new-location (gget-random-move grid-copy current-goal)))
                (cond ((eq? new-location current-goal) (set! grid-copy (gblast grid-copy (adjacent current-goal)))))
                (grollout-inner (list (not turn) new-location current-robot) (+ current-count 1) grid-copy)))
            (else
              (let ((new-location (gget-random-move grid-copy current-robot)))
                (cond ((eq? new-location current-robot) (set! grid-copy (gblast grid-copy (adjacent current-robot)))))
                (grollout-inner (list (not turn) current-goal new-location) (+ current-count 1) grid-copy)))))))))
    (let ((new-grid (copy-grid grid))) ; Copy the current grid
      (set! new-grid (gblast new-grid blast-list)) ; Blast the points that were blasted in the tree
      (grollout-inner state current-count new-grid))))  ; Perform rollout


;;;;;;;;;;;;;;;;;;;; SEARCH HELPER FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define gis-caught?
  (lambda (state)
    ; Checks if the goal is caught or not
    (let ((goal (cadr state)) (robot (caddr state)))
      (<= (+ (abs (- (car goal) (car robot))) (abs (- (cadr goal) (cadr robot)))) 1))))

(define gtsum
	(lambda (current-tree)
    ; Function that finds the sum of the t values of direct-children of a root-node
		(let ((children (gtree-children current-tree)) (total 0))
			(for-each
				(lambda (child-tree) (set! total (+ total (gnode-t (gtree-root child-tree)))))
			children)
	  total)))

(define gupdate
	(lambda (current-tree parent-eval-count)
    ; Function to update the fields of a node depending on the rollout outcome
		(let* ((current-root (gtree-root current-tree)) (n (gnode-n current-root)) (t0 (gnode-t0 current-root)) (t (gnode-t current-root)))
				(set-gnode-n! current-root (+ n 1))
			  (set-gnode-t! current-root (+ (gtsum current-tree) t0))
				(set-gnode-ucb! current-root (gUCB t n parent-eval-count)))))

(define gblast
  (lambda (grid-copy lst)
    ; Function to blast a list of nodes in a specific grid --rather than the global grid
    (if (not (null? lst))
      (let* ((pt (car lst))
             (x (car pt))
             (y (cadr pt)))
        (cond ((= (get-node grid-copy x y) obstacle)
          (set-node! grid-copy x y free)))
        (gblast grid-copy (cdr lst))) grid-copy)))

(define gblast-real
  (lambda (lst)
    ; Function to really blast a node and show it in the canvas
    (if (not (null? lst))
      (let* ((pt (car lst))
             (x (car pt))
             (y (cadr pt)))
        (cond ((= (get-node grid x y) obstacle)
          (set-node! grid x y free)
          (send canvas make-now-free x y)))
        (gblast-real (cdr lst))))))


;;;;;;;;;;;;;;;;;;;; GRID-COPY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define copy-grid
  (lambda (original-grid)
    ; Main function I use to create a copy of the grid
    (let ((rows (vector-length original-grid)))  ; Getting the number of rows
      ; Calling the helper with the original grid, an empty grid, and the starting row index (0)
      (copy-helper original-grid (make-vector rows) 0))))

(define copy-helper
  (lambda (original-grid new-grid i)
    ; Helper function which will access to each single row and copy it using the next helper function
    (let ((rows (vector-length original-grid)))  ; Getting the length of the grid
      (cond
        ((< i rows)  ; While there are still more rows to copy
          ; Set the relevant raw in the grid to a copy of the origianl row using the copy-row
          (vector-set! new-grid i (copy-row (vector-ref original-grid i)))
          ; Calling the function recursively but incrementing the row count
          (copy-helper original-grid new-grid (+ i 1)))
        ; If we're done, return the new grid
        (else new-grid)))))

(define copy-row
  (lambda (row)
    ; Helper function to copy a complete row into a new one
    (let* ((cols (vector-length row)) (new-row (make-vector cols)))  ; Calculate the length of the row and create an empty one
      ; Call the next helper function with row, new row, index and number of columns
      (copy-element row new-row 0 cols))))

(define copy-element
  (lambda (row new-row j cols)
    ; Helper function to copy a row
    (cond
      ((>= j cols) new-row)  ; If all elements were copied, return the new row
      ; Otherwise update the new row and call the function again with the next index
      (else (vector-set! new-row j (vector-ref row j)) (copy-element row new-row (+ j 1) cols)))))

