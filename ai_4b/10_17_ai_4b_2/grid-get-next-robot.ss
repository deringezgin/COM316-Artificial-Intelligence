; Derin Gezgin | Russell Kosovsky | Jay Nash
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #4b | MCTS
; Due October 17, 2024
; File that has the complete code for MCTS - Goal


;;;;;;;;;;;;;;;;;;;; HYPERPARAMETERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define gmax-rollout-depth 50)
(define gsingle-rollout-count 100)
(define gstart-time 0)
(define gmax-time 5)


;;;;;;;;;;;;;;;;;;;; MAIN FUNCTION TO GET THE NEXT GOAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define get-next-robot
  (lambda (point)
    (let ((return (cadr (gmcts-search (list #t robot goal)))))
      ;(pause (* pause-num 1000000000))
      return)))


;;;;;;;;;;;;;;;;;;;; STRUCTURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(struct gtree root children)
(struct gnode state t0 t n ucb node-id)

(define gtree '())


;;;;;;;;;;;;;;;;;;;; MATH FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define gid-count 0)
(define gconst-exp 2)

(define gid
	(lambda ()
		(set! gid-count (+ gid-count 1)) (- gid-count 1)))

(define gUCB
	(lambda (eval current-eval-count parent-eval-count)
		(if (or (= current-eval-count 0) (= parent-eval-count 0))
			1e9
			(let* ((vi-bar (/ eval current-eval-count))
					(c gconst-exp)
					(exp-term (sqrt (/ (log parent-eval-count) current-eval-count))))
				(+ vi-bar (* c exp-term))))))

(define greturn-best-move
	(lambda (current-tree)
		(let ((children (gtree-children current-tree)))
			(car (list-sort (lambda (c1 c2) (> (gnode-ucb (gtree-root c1)) (gnode-ucb (gtree-root c2)))) children)))))


;;;;;;;;;;;;;;;;;;;; TREE FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define gcreate-children
	(lambda (root)
		(let* ((adjacents (gexpand-max (gnode-state (gtree-root root)))))
			(map (lambda (state) (make-gtree (make-gnode state 0 0 0 1e9 (gid)) '())) adjacents))))

(define gexpand
	(lambda (current-root-tree)
		(set-gtree-children! current-root-tree (gcreate-children current-root-tree))))

(define gexpand-max
	(lambda (tgr-pair)
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
		(cond
			((null? lst) '())
			(else
				(let ((picked-element (gfind-element lst (random (length lst)))))
					(cons picked-element (gshuffle-lst (gremove-element lst picked-element))))))))

(define gremove-element
	(lambda (lst item)
		(cond ((null? lst) '()) ((equal? (car lst) item) (cdr lst)) (else (cons (car lst) (gremove-element (cdr lst) item))))))

(define gget-adjacent
	(lambda (point)
		(gshuffle-lst (append (list point) (adjacento point)))))

(define gfind-element
	(lambda (lst index)
		(cond
			((= index 0) (car lst))
			(else (gfind-element (cdr lst) (- index 1))))))

(define gget-random-move
	(lambda (point)
		(let ((possible-moves (adjacento point)))
			(list-ref possible-moves (random (length possible-moves))))))


;;;;;;;;;;;;;;;;;;;; SEARCH FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define gmcts-search
  (lambda (root)
    (let ((root-node (make-gnode root 0 0 0 -1 (gid))))
      (set! gstart-time (time-second (current-time)))
      (set! gtree (make-gtree root-node '()))
      (gexpand gtree)
      (gnode-state (gtree-root (gmcts 0))))))

(define gmcts
  (lambda (count)
    (define mcts-inner
      (lambda (current-tree parent-eval-count)
        (cond
          ((gis-leaf? current-tree)
            (cond
              ((= 0 (gnode-n (gtree-root current-tree)))
                (set-gnode-t0! (gtree-root current-tree) (gperform-rollout (gnode-state (gtree-root current-tree)) 0)))
              (else (gexpand current-tree))))
          (else (mcts-inner (greturn-best-move current-tree) (gnode-n (gtree-root current-tree)))))
        (gupdate current-tree parent-eval-count)))
    (cond
      ((> (- (time-second (current-time)) gstart-time) gmax-time) (greturn-best-move gtree))
      (else (mcts-inner gtree 0) (gmcts (+ count 1))))))


(define gperform-rollout
  (lambda (state current-rollout-count)
    (cond
      ((eq? current-rollout-count gsingle-rollout-count) 0)
      (else (+ (grollout state 0) (gperform-rollout state (+ current-rollout-count 1)))))))


(define grollout
  (lambda (state current-count)
    (cond
      ((gis-caught? state) 1)
      ((equal? gmax-rollout-depth current-count) -1)
      (else
        (let ((turn (car state)) (current-goal (cadr state)) (current-robot (caddr state)))
          (cond
            (turn (grollout (list (not turn) (gget-random-move current-goal) current-robot) (+ current-count 1)))
            (else (grollout (list (not turn) current-goal (gget-random-move current-robot)) (+ current-count 1)))))))))


;;;;;;;;;;;;;;;;;;;; SEARCH HELPER FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define gis-caught?
  (lambda (state)
    (let ((goal (cadr state)) (robot (caddr state)))
      (<= (+ (abs (- (car goal) (car robot))) (abs (- (cadr goal) (cadr robot)))) 1))))

(define gtsum
	(lambda (current-tree)
		(let ((children (gtree-children current-tree)) (total 0))
			(for-each
				(lambda (child-tree) (set! total (+ total (gnode-t (gtree-root child-tree)))))
			children)
	  total)))

(define gis-leaf?
	(lambda (current-tree)
		(null? (gtree-children current-tree))))

(define gupdate
	(lambda (current-tree parent-eval-count)
		(let* ((current-root (gtree-root current-tree)) (n (gnode-n current-root)))
				(set-gnode-n! current-root (+ n 1))
			  (set-gnode-t! current-root (+ (gtsum current-tree) (gnode-t0 current-root)))
				(set-gnode-ucb! current-root (gUCB (gnode-t current-root) (gnode-n current-root) parent-eval-count)))))
