; Derin Gezgin | Russell Kosovsky | Jay Nash
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #4b | MCTS
; Due October 17, 2024
; File that has the complete code for MCTS - Goal


;;;;;;;;;;;;;;;;;;;; HYPERPARAMETERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define gmax-rollout-depth 50)  ; How far a single rollout will go
(define gsingle-rollout-count 200)  ; How many rollouts a leaf node will perform before starting the backpropagation
(define gstart-time 0)
(define gmax-time 5)  ; Maximum number of seconds the MCTS can think
(define gconst-exp 2)  ; Exploration constant


;;;;;;;;;;;;;;;;;;;; MAIN FUNCTION TO GET THE NEXT GOAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Calling the MCTS in the current board state
(define get-next-goal
  (lambda (point)
    (cadr (gmcts-search (list #t goal robot)))))


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
(struct gnode state t0 t n ucb node-id)  ; This is the node structure which has the necessarry fields for the MCTS

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
		(let* ((adjacents (gexpand-max (gnode-state (gtree-root root)))))
			(map (lambda (state) (make-gtree (make-gnode state 0 0 0 1e9 (gid)) '())) adjacents))))

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
		(gshuffle-lst (append (list point) (adjacento point)))))

(define gget-random-move
	(lambda (point)
    ; Function to get a random move from a point among all the possible moves
		(let ((possible-moves (adjacento point)))
			(list-ref possible-moves (random (length possible-moves))))))


;;;;;;;;;;;;;;;;;;;; SEARCH FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define gmcts-search
  (lambda (root)
    ; Our main MCTS search function. It logs the start time, creates a new tree and calls the helper function
    (let ((root-node (make-gnode root 0 0 0 -1 (gid))))
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
                (set-gnode-t0! (gtree-root current-tree) (gperform-rollout (gnode-state (gtree-root current-tree)) 0)))
              (else (gexpand current-tree))))  ; If it was explored, expand it
          (else (mcts-inner (greturn-best-move current-tree) (gnode-n (gtree-root current-tree)))))  ; Otherwise call the function again
        (gupdate current-tree parent-eval-count)))  ; When a single tour is done, update the value
    (cond
      ; After each iteration, check for the time limit and if we're over the limit return the best move
      ((> (- (time-second (current-time)) gstart-time) gmax-time) (greturn-best-move gtree))
      (else (mcts-inner gtree 0) (gmcts (+ count 1))))))  ; This is the recursive call


(define gperform-rollout
  (lambda (state current-rollout-count)
    ; Function that performs multiple rollouts and take a sum of the scores
    (cond
      ((eq? current-rollout-count gsingle-rollout-count) 0)
      (else (+ (grollout state 0) (gperform-rollout state (+ current-rollout-count 1)))))))


(define grollout
  (lambda (state current-count)
    ; Function that performs a completely random rollout with a length of "gsingle-rollout-count"
    (cond
      ((gis-caught? state) -1)
      ((equal? gmax-rollout-depth current-count) 1)
      (else
        (let ((turn (car state)) (current-goal (cadr state)) (current-robot (caddr state)))
          (cond
            (turn (grollout (list (not turn) (gget-random-move current-goal) current-robot) (+ current-count 1)))
            (else (grollout (list (not turn) current-goal (gget-random-move current-robot)) (+ current-count 1)))))))))


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
