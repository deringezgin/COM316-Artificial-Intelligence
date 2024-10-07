; Structures
(define-syntax struct (lambda (x) (define gen-id (lambda (template-id . args) (datum->syntax template-id (string->symbol (apply string-append (map (lambda (x) (if (string? x) x (symbol->string (syntax->datum x)))) args)))))) (syntax-case x () [(_ name field ...) (with-syntax ([constructor (gen-id #'name "make-" #'name)] [predicate (gen-id #'name #'name "?")] [(access ...) (map (lambda (x) (gen-id x #'name "-" x)) #'(field ...))] [(assign ...) (map (lambda (x) (gen-id x "set-" #'name "-" x "!")) #'(field ...))] [structure-length (+ (length #'(field ...)) 1)] [(index ...) (let f ([i 1] [ids #'(field ...)]) (if (null? ids) '() (cons i (f (+ i 1) (cdr ids)))))]) #'(begin (define constructor (lambda (field ...) (vector 'name field ...))) (define predicate (lambda (x) (and (vector? x) (= (vector-length x) structure-length) (eq? (vector-ref x 0) 'name)))) (define access (lambda (x) (vector-ref x index))) ... (define assign (lambda (x update) (vector-set! x index update))) ...))])))
(struct tree root children)
(struct node state t0 t n ucb node-id)
(define tree '())

; Math Functions
(define id-count 0)
(define const-exp 1.5)
(define id (lambda () (set! id-count (+ id-count 1)) (- id-count 1)))
(define UCB (lambda (eval current-eval-count parent-eval-count) (if (or (= current-eval-count 0) (= parent-eval-count 0)) 1e9 (let* ((vi-bar (/ eval current-eval-count)) (c const-exp) (exp-term (sqrt (/ (log parent-eval-count) current-eval-count)))) (+ vi-bar (* c exp-term))))))
(define return-best-move (lambda (current-tree) (let ((children (tree-children current-tree))) (car (list-sort (lambda (c1 c2) (> (node-ucb (tree-root c1)) (node-ucb (tree-root c2)))) children)))))
(define euclidian-dist (lambda (point1 point2) (sqrt (+ (sqr (- (car point1) (car point2))) (sqr (- (cadr point1) (cadr point2)))))))
(define sqr (lambda (x) (* x x)))
(define blockwise-dist (lambda (point1 point2) (+ (abs (- (car point1) (car point2))) (abs (- (cadr point1) (cadr point2))))))


; Tree Functions
(define create-children
  (lambda (root)
    (let* ((adjacents (expand-max (node-state (tree-root root)))))
      (map (lambda (state) (make-tree (make-node state 0 0 0 1e9 (id)) '())) adjacents))))

(define expand
  (lambda (current-root-tree)
    (set-tree-children! current-root-tree (create-children current-root-tree))))

(define expand-max
  (lambda (tgr-pair)
    (let* ((turn (car tgr-pair)) (goal (cadr tgr-pair)) (robot (caddr tgr-pair)))
      (cond
        (turn (map (lambda (x) (append (list (not turn)) (list x) (list robot))) (get-adjacent goal)))
        (else (map (lambda (x) (append (list (not turn)) (list goal) (list x))) (get-adjacent robot)))))))

; Grid Functions
(define copy-grid (lambda (original-grid) (let ((rows (vector-length original-grid))) (copy-helper original-grid (make-vector rows) 0))))
(define copy-helper (lambda (original-grid new-grid i) (let ((rows (vector-length original-grid))) (cond ((< i rows) (vector-set! new-grid i (copy-row (vector-ref original-grid i))) (copy-helper original-grid new-grid (+ i 1))) (else new-grid)))))
(define copy-row (lambda (row) (let* ((cols (vector-length row)) (new-row (make-vector cols))) (copy-element row new-row 0 cols))))
(define copy-element (lambda (row new-row j cols) (cond ((>= j cols) new-row) (else (vector-set! new-row j (vector-ref row j)) (copy-element row new-row (+ j 1) cols)))))
(define shuffle-lst (lambda (lst) (cond ((null? lst) '()) (else (let ((picked-element (find-element lst (random (length lst))))) (cons picked-element (shuffle-lst (remove-element lst picked-element))))))))
(define remove-element (lambda (lst item) (cond ((null? lst) '()) ((equal? (car lst) item) (cdr lst)) (else (cons (car lst) (remove-element (cdr lst) item))))))
(define get-adjacent (lambda (point) (shuffle-lst (append (list point) (adjacento point)))))

(define find-element (lambda (lst index) (cond ((= index 0) (car lst)) (else (find-element (cdr lst) (- index 1))))))
(define get-random-move (lambda (point) (let ((possible-moves (get-adjacent point))) (list-ref possible-moves (random (length possible-moves))))))

; Search Helper Functions
(define tsum (lambda (current-tree) (let ((children (tree-children current-tree)) (sum 0)) (for-each (lambda (child-tree) (set! sum (+ sum (node-t (tree-root child-tree))))) children) sum)))
(define is-leaf? (lambda (current-tree) (null? (tree-children current-tree))))

(define update
  (lambda (current-tree parent-eval-count)
    (let* ((current-root (tree-root current-tree))
            (n (node-n current-root)))
      (set-node-n! current-root (+ n 1))
      (set-node-t! current-root (+ (tsum current-tree) (node-t0 current-root)))
      (set-node-ucb! current-root (UCB (node-t current-root) (node-n current-root) parent-eval-count)))))

; Search Functions
(define mcts-search
  (lambda (root count rollout-time)
    (let ((root-node (make-node root 0 0 0 -1 (id))))
      (set! tree (make-tree root-node '()))
      (node-state (tree-root (mcts count rollout-time))))))

(define mcts
  (lambda (count rollout-time)
    (define mcts-inner
      (lambda (current-tree parent-eval-count)
        (cond
          ((is-leaf? current-tree)
            (cond
              ((= 0 (node-n (tree-root current-tree)))
                (set-node-t0! (tree-root current-tree) (rollout (node-state (tree-root current-tree)) rollout-time 0)))
              (else (expand current-tree))))
          (else (mcts-inner (return-best-move current-tree) (node-n (tree-root current-tree)))))
        (update current-tree parent-eval-count)))
    (cond
      ((= count 0) (return-best-move tree))
      (else (mcts-inner tree 0) (mcts (- count 1) rollout-time)))))

(define rollout
  (lambda (state count current-count)
    (cond
      ((= count current-count) (heuristic state))
      ((equal? (cadr state) (caddr state)) current-count)
      (else
        (let ((turn (car state)) (goal (cadr state)) (robot (caddr state)))
          (cond
            (turn (rollout (list (not turn) (get-random-move goal) robot) count (+ current-count 1)))
            (else (rollout (list (not turn) goal (get-random-move robot)) count (+ current-count 1)))))))))

(define heuristic (lambda (state) (let ((turn (car state)) (goal (cadr state)) (robot (caddr state))) (blockwise-dist goal robot))))

