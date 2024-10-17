; Derin Gezgin | Russell Kosovsky | Jay Nash
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #4b | MCTS
; Due October 17, 2024
; File that has the complete code for MCTS - Robot


;;;;;;;;;;;;;;;;;;;; HYPERPARAMETERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define rmax-rollout-depth 50)  ; How far a single rollout will go
(define rsingle-rollout-count 200)  ; How many rollouts a leaf node will perform before starting the backpropagation
(define rstart-time 0)
(define rmax-time 5)  ; Maximum number of seconds the MCTS can think
(define rconst-exp 2)  ; Exploration constant


;;;;;;;;;;;;;;;;;;;; MAIN FUNCTION TO GET THE NEXT ROBOT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Calling the MCTS in the current board state
(define get-next-robot
  (lambda (point)
    (cadr (rmcts-search (list #t robot goal)))))


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
(struct rnode state t0 t n ucb node-id)  ; This is the node structure which has the necessarry fields for the MCTS

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


(define rcreate-children
	(lambda (root)
    ; Function to generate a children from a root. A child is also a tree with many a root and an empty children list
		(let* ((adjacents (rexpand-max (rnode-state (rtree-root root)))))
			(map (lambda (state) (make-rtree (make-rnode state 0 0 0 1e9 (rid)) '())) adjacents))))
			
(define rexpand
	(lambda (current-root-tree)
    ; Setting the children of a tree the newly generated children (which are also trees) from the rcreate-children function
		(set-rtree-children! current-root-tree (rcreate-children current-root-tree))))
		
(define rexpand-max
	(lambda (tgr-pair)
    ; Function that takes a (turn goal robot) pair and expands the relevant player depending on whose turn it is
		(let* ((turn (car tgr-pair))
						(goal (cadr tgr-pair))
						(robot (caddr tgr-pair)))
					(cond
						(turn
							(map (lambda (x) (append (list (not turn)) (list x) (list robot))) (rget-adjacent goal)))
					(else
						(map (lambda (x) (append (list (not turn)) (list goal) (list x))) (rget-adjacent robot)))))))


;;;;;;;;;;;;;;;;;;;; GRID FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define rshuffle-list
	(lambda (lst)
    ; Simple function to shuffle a list
		(cond
			((null? lst) '())
			(else
				(let ((picked-element (list-ref lst (random (length lst)))))
					(cons picked-element (rshuffle-list (rremove-element lst picked-element))))))))

(define rremove-element
	(lambda (lst item)
    ; Simple function to remove an element from a list
		(cond ((null? lst) '()) ((equal? (car lst) item) (cdr lst)) (else (cons (car lst) (rremove-element (cdr lst) item))))))

(define rget-adjacent
	(lambda (point)
    ; Function to get the adjacento and the current node
		(rshuffle-list (append (list point) (adjacento point)))))

(define rget-random-move
	(lambda (point)
    ; Function to get a random move from a point among all the possible moves
		(let ((possible-moves (adjacento point)))
			(list-ref possible-moves (random (length possible-moves))))))


;;;;;;;;;;;;;;;;;;;; SEARCH FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define rmcts-search
  (lambda (root)
    ; Our main MCTS search function. It logs the start time, creates a new tree and calls the helper function
    (let ((root-node (make-rnode root 0 0 0 -1 (rid))))
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
                (set-rnode-t0! (rtree-root current-tree) (rperform-rollout (rnode-state (rtree-root current-tree)) 0)))
              (else (rexpand current-tree))))  ; If it was explored, expand it
          (else (mcts-inner (rreturn-best-move current-tree) (rnode-n (rtree-root current-tree)))))  ; Otherwise call the function again
        (rupdate current-tree parent-eval-count)))  ; When a single tour is done, update the value
    (cond
      ; After each iteration, check for the time limit and if we're over the limit return the best move
      ((> (- (time-second (current-time)) rstart-time) rmax-time) (rreturn-best-move rtree))
      (else (mcts-inner rtree 0) (rmcts (+ count 1))))))  ; This is the recursive call


(define rperform-rollout
  (lambda (state current-rollout-count)
    ; Function that performs multiple rollouts and take a sum of the scores
    (cond
      ((eq? current-rollout-count rsingle-rollout-count) 0)
      (else (+ (rrollout state 0) (rperform-rollout state (+ current-rollout-count 1)))))))


(define rrollout
  (lambda (state current-count)
    ; Function that performs a completely random rollout with a length of "rsingle-rollout-count"
    (cond
      ((ris-caught? state) 1)
      ((equal? rmax-rollout-depth current-count) -1)
      (else
        (let ((turn (car state)) (current-goal (cadr state)) (current-robot (caddr state)))
          (cond
            (turn (rrollout (list (not turn) (rget-random-move current-goal) current-robot) (+ current-count 1)))
            (else (rrollout (list (not turn) current-goal (rget-random-move current-robot)) (+ current-count 1)))))))))


;;;;;;;;;;;;;;;;;;;; SEARCH HELPER FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define ris-caught?
  (lambda (state)
    ; Checks if the goal is caught or not
    (let ((goal (cadr state)) (robot (caddr state)))
      (<= (+ (abs (- (car goal) (car robot))) (abs (- (cadr goal) (cadr robot)))) 1))))

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
