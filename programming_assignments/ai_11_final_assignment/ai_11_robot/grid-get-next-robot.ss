; Derin Gezgin | Russell Kosovsky | Jay Nash | Sarah Goyette
; COM316: Artificial Intelligence | Fall 2024
; Final Programming Assignment
; Due December 5th, 2024
; File that has the complete code for MCTS - Robot - For the Final Project


;;;;;;;;;;;;;;;;;;;; HYPERPARAMETERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define rmax-rollout-depth 50)  ; How far a single rollout will go
(define rsingle-rollout-count 200)  ; How many rollouts a leaf node will perform before starting the backpropagation
(define rstart-time 0)
(define rmax-time 3)  ; Maximum number of seconds the MCTS can think
(define rconst-exp 2)  ; Exploration constant


;;;;;;;;;;;;;;;;;;;; MAIN FUNCTION TO GET THE NEXT GOAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Calling the MCTS in the current board state
(define get-next-robot
  (lambda (point)
    (cond
      ((> (block-wise-distance robot) 4)
        (let ((new-location (get-best-move)))
        (cond
          ((= (rblock-status grid new-location) obstacle) (rblast-real (adjacent point)) point)
          (else new-location))))
      (else
        (let ((new-location (cadr (rmcts-search (list #t robot goal)))))
          (cond ((eq? point new-location) (rblast-real (adjacent new-location)))) new-location)))))

(define get-best-move
  (lambda ()
    (let* ((adjacent-points (adjacent robot))
            (sorted-points (list-sort (lambda (node1 node2) (< (block-wise-distance node1) (block-wise-distance node2))) adjacent-points))
            (best-heuristic (block-wise-distance (car sorted-points)))
            (best-points (filter (lambda (point) (= (block-wise-distance point) best-heuristic)) sorted-points)))
      (random-select best-points))))

(define block-wise-distance
  (lambda (pt)
    ; Function to calculate the block-wise distance between two nodes
    (+ (abs (- (car goal) (car pt))) (abs (- (cadr goal) (cadr pt))))))

(define random-select
  (lambda (lst)
    ; Randomly select an element from the list and return it
    (cond
      ((null? lst) '())
      (else (list-ref lst (random (length lst)))))))


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

(struct rtree root children)  ; This is our tree structure which has a root node and its children
(struct rnode state t0 t n ucb node-id blastList)  ; This is the node structure which has the necessarry fields for the MCTS

(define rtree '())  ; Our MCTS tree


;;;;;;;;;;;;;;;;;;;; MATH FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define rid-count 0)  ; ID variable that assigns a unique id to nodes

(define rid
	(lambda ()
    ; Function to get the next available ID
		(set! rid-count (+ rid-count 1)) (- rid-count 1)))

(define rUCB
	(lambda (eval current-eval-count parent-eval-count)
    ; Function that returns the UCB of a specific node
		(if (or (= current-eval-count 0) (= parent-eval-count 0))
			1e9
			(let* ((vi-bar (/ eval current-eval-count))
					(c rconst-exp)
					(exp-term (sqrt (/ (log parent-eval-count) current-eval-count))))
				(+ vi-bar (* c exp-term))))))

(define rreturn-best-move
	(lambda (current-tree)
    ; Function that returns the state of the node with the highest UCB
		(let ((children (rtree-children current-tree)))
			(car (list-sort (lambda (c1 c2) (> (rnode-ucb (rtree-root c1)) (rnode-ucb (rtree-root c2)))) children)))))


;;;;;;;;;;;;;;;;;;;; TREE FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define rexpand
	(lambda (current-root-tree)
    ; Setting the children of a tree the newly generated children (which are also trees) from the rcreate-children function
		(set-rtree-children! current-root-tree (rcreate-children current-root-tree))))

(define rcreate-children
	(lambda (root)
    ; Function to generate a children from a root. A child is also a tree with many a root and an empty children list
    ; If the child is the same point as its parent, this means that it is going to blast.
    ; To keep track of the blasted nodes throughout the MCTS tree we keep track of a list of blasted nodes which also
    ; carries through the children of the parent.
		(let* ((adjacents (rexpand-max (rnode-state (rtree-root root))))
            (previous-state (rnode-state (rtree-root root))) (previous-turn (car previous-state))
            (previous-robot (cadr previous-state)) (previous-goal (caddr previous-state)))
			(map
        (lambda (state)
          (let ((previous-blastList (rnode-blastList (rtree-root root))))
            (cond
            (previous-turn (cond ((eq? previous-robot (cadr state)) (set! previous-blastList (append (list previous-robot) previous-blastList)))))
            (else (cond ((eq? previous-goal (caddr state)) (set! previous-blastList (append (list previous-goal) previous-blastList))))))
          (make-rtree (make-rnode state 0 0 0 1e9 (rid) previous-blastList) '())))
        adjacents))))

(define rexpand-max
	(lambda (tgr-pair)
    ; Function that takes a (turn goal robot) pair and expands the relevant player depending on whose turn it is
		(let* ((turn (car tgr-pair))
						(robot (cadr tgr-pair))
						(goal (caddr tgr-pair)))
					(cond
						(turn
							(map (lambda (x) (append (list (not turn)) (list x) (list goal))) (rget-adjacent robot)))
					(else
						(map (lambda (x) (append (list (not turn)) (list robot) (list x))) (rget-adjacent goal)))))))


;;;;;;;;;;;;;;;;;;;; GRID FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define rshuffle-lst
	(lambda (lst)
    ; Simple function to shuffle a list
		(cond
			((null? lst) '())
			(else
				(let ((picked-element (list-ref lst (random (length lst)))))
					(cons picked-element (rshuffle-lst (rremove-element lst picked-element))))))))

(define rremove-element
	(lambda (lst item)
    ; Simple function to remove an element from a list
		(cond ((null? lst) '()) ((equal? (car lst) item) (cdr lst)) (else (cons (car lst) (rremove-element (cdr lst) item))))))

(define rget-adjacent
	(lambda (point)
    ; Function to get the adjacento and the current node
		(rshuffle-lst (append (list point) (radjacento grid point)))))

(define rget-random-move
	(lambda (grid-copy point)
    ; Function to get a random move from a point among all the possible moves
		(let ((possible-moves (append (list point) (radjacento grid-copy point))))
			(list-ref possible-moves (random (length possible-moves))))))

; We rewrite some of the built-in functions as the grid we are working on can change depending on the blasted squares
(define radjacento
  (lambda (grid-copy block)
    (let* ((adj-lst0 (radjacent block))
           (adj-lst1 (map (lambda (z) (rstepo grid-copy block z)) adj-lst0)))
      (remove-f adj-lst1))))

(define radjacent
  (lambda (block)
    (let ((x (car block))
          (y (cadr block)))
      (append
        (if (< y 1) '() (list (list x (- y 1))))
        (if (< x 1) '() (list (list (- x 1) y)))
        (if (>= y (- num-col-row 1)) '() (list (list x (+ y 1))))
        (if (>= x (- num-col-row 1)) '() (list (list (+ x 1) y)))))))

(define rstepo
  (lambda (grid-copy b c)
    (let ((b-status (rblock-status grid-copy b))
          (c-status (rblock-status grid-copy c))
          (x-diff (abs (- (car b) (car c))))
          (y-diff (abs (- (cadr b) (cadr c)))))
      (if (or (= b-status obstacle)
              (= c-status obstacle)
              (not (= (+ x-diff y-diff) 1)))
          #f
      ;else
          c))))

(define rblock-status
  (lambda (grid-copy block)
    (get-node grid-copy (car block) (cadr block))))


;;;;;;;;;;;;;;;;;;;; SEARCH FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define rmcts-search
  (lambda (root)
    ; Our main MCTS search function. It logs the start time, creates a new tree and calls the helper function
    (let ((root-node (make-rnode root 0 0 0 -1 (rid) '())))
      (set! rstart-time (time-second (current-time)))
      (set! rtree (make-rtree root-node '()))
      (rexpand rtree)
      (rnode-state (rtree-root (rmcts 0))))))

(define rmcts
  (lambda (count)
    ; The helper mcts function.
    (define mcts-inner
      (lambda (current-tree parent-eval-count)
        (cond
          ((null? (rtree-children current-tree))  ; If it is a leaf
            (cond
              ((= 0 (rnode-n (rtree-root current-tree)))  ; Check if it is never explored. If so, perform rollout
                (set-rnode-t0! (rtree-root current-tree)
                  (rperform-rollout (rnode-state (rtree-root current-tree)) 0 (rnode-blastList (rtree-root current-tree)))))
              (else (rexpand current-tree))))  ; If it was explored, expand it
          (else (mcts-inner (rreturn-best-move current-tree) (rnode-n (rtree-root current-tree)))))  ; Otherwise call the function again
        (rupdate current-tree parent-eval-count)))  ; When a single tour is done, update the value
    (cond
      ; After each iteration, check for the time limit and if we're over the limit return the best move
      ((>= (- (time-second (current-time)) rstart-time) rmax-time) (rreturn-best-move rtree))
      (else (mcts-inner rtree 0) (rmcts (+ count 1))))))  ; This is the recursive call

(define rperform-rollout
  (lambda (state current-rollout-count blast-list)
    ; Function that performs multiple rollouts and take a sum of the scores
    (cond
      ((eq? current-rollout-count rsingle-rollout-count) 0)
      (else (+ (rrollout state 0 blast-list) (rperform-rollout state (+ current-rollout-count 1) blast-list))))))

(define rrollout
  (lambda (state current-count blast-list)
    ; Function that performs rollout using the helper inner function.
    (define rrollout-inner
      (lambda (state current-count grid-copy)
        ; Helper function to perform a single rollout until the end condition.
        ; This also blasts the nodes in the used grid for a better estimate.
        (cond
          ((ris-caught? state) 1)
          ((equal? rmax-rollout-depth current-count) -1)
        (else
          (let ((turn (car state)) (current-robot (cadr state)) (current-goal (caddr state)))
          (cond
            (turn
              (let ((new-location (rget-random-move grid-copy current-robot)))
                (cond ((eq? new-location current-robot) (set! grid-copy (rblast grid-copy (adjacent current-robot)))))
                (rrollout-inner (list (not turn) new-location current-goal) (+ current-count 1) grid-copy)))
            (else
              (let ((new-location (rget-random-move grid-copy current-goal)))
                (cond ((eq? new-location current-goal) (set! grid-copy (rblast grid-copy (adjacent current-goal)))))
                (rrollout-inner (list (not turn) current-robot new-location) (+ current-count 1) grid-copy)))))))))
    (let ((new-grid (copy-grid grid))) ; Copy the current grid
      (set! new-grid (rblast new-grid blast-list)) ; Blast the points that were blasted in the tree
      (rrollout-inner state current-count new-grid))))  ; Perform rollout


;;;;;;;;;;;;;;;;;;;; SEARCH HELPER FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define ris-caught?
  (lambda (state)
    ; Checks if the goal is caught or not
    (let ((robot (cadr state)) (goal (caddr state)))
      (<= (+ (abs (- (car robot) (car goal))) (abs (- (cadr robot) (cadr goal)))) 1))))

(define rtsum
	(lambda (current-tree)
    ; Function that finds the sum of the t values of direct-children of a root-node
		(let ((children (rtree-children current-tree)) (total 0))
			(for-each
				(lambda (child-tree) (set! total (+ total (rnode-t (rtree-root child-tree)))))
			children)
	  total)))

(define rupdate
	(lambda (current-tree parent-eval-count)
    ; Function to update the fields of a node depending on the rollout outcome
		(let* ((current-root (rtree-root current-tree)) (n (rnode-n current-root)) (t0 (rnode-t0 current-root)) (t (rnode-t current-root)))
				(set-rnode-n! current-root (+ n 1))
			  (set-rnode-t! current-root (+ (rtsum current-tree) t0))
				(set-rnode-ucb! current-root (rUCB t n parent-eval-count)))))

(define rblast
  (lambda (grid-copy lst)
    ; Function to blast a list of nodes in a specific grid --rather than the global grid
    (if (not (null? lst))
      (let* ((pt (car lst))
             (x (car pt))
             (y (cadr pt)))
        (cond ((= (get-node grid-copy x y) obstacle)
          (set-node! grid-copy x y free)))
        (rblast grid-copy (cdr lst))) grid-copy)))

(define rblast-real
  (lambda (lst)
    ; Function to really blast a node and show it in the canvas
    (if (not (null? lst))
      (let* ((pt (car lst))
             (x (car pt))
             (y (cadr pt)))
        (cond ((= (get-node grid x y) obstacle)
          (set-node! grid x y free)
          (send canvas make-now-free x y)))
        (rblast-real (cdr lst))))))


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

