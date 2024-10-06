; Structures
(define-syntax struct (lambda (x) (define gen-id (lambda (template-id . args) (datum->syntax template-id (string->symbol (apply string-append (map (lambda (x) (if (string? x) x (symbol->string (syntax->datum x)))) args)))))) (syntax-case x () [(_ name field ...) (with-syntax ([constructor (gen-id #'name "make-" #'name)] [predicate (gen-id #'name #'name "?")] [(access ...) (map (lambda (x) (gen-id x #'name "-" x)) #'(field ...))] [(assign ...) (map (lambda (x) (gen-id x "set-" #'name "-" x "!")) #'(field ...))] [structure-length (+ (length #'(field ...)) 1)] [(index ...) (let f ([i 1] [ids #'(field ...)]) (if (null? ids) '() (cons i (f (+ i 1) (cdr ids)))))]) #'(begin (define constructor (lambda (field ...) (vector 'name field ...))) (define predicate (lambda (x) (and (vector? x) (= (vector-length x) structure-length) (eq? (vector-ref x 0) 'name)))) (define access (lambda (x) (vector-ref x index))) ... (define assign (lambda (x update) (vector-set! x index update))) ...))])))
(struct tree root children)
(struct node state t n ucb node-id)
(define tree '())

; Math Functions
(define id-count 0)
(define const-exp 2)
(define rollout
  (lambda (state)
    (random 100)))
(define id (lambda () (set! id-count (+ id-count 1)) (- id-count 1)))
(define UCB (lambda (eval current-eval-count parent-eval-count) (if (or (= current-eval-count 0) (= parent-eval-count 0)) 1e9 (let* ((vi-bar (/ eval current-eval-count)) (c const-exp) (exp-term (sqrt (/ (log parent-eval-count) current-eval-count)))) (+ vi-bar (* c exp-term))))))

; Tree Functions
(define create-children (lambda (root) (let* ((adjacents (adjacent (node-state (tree-root root))))) (map (lambda (state) (make-tree (make-node state 0 0 1e9 (id)) '())) adjacents))))
(define adjacent (lambda (parent) (make-list 2 (list (* 2 (car parent)) (cadr parent)))))
(define expand
  (lambda (current-root-tree)
    (set-tree-children! current-root-tree (create-children current-root-tree))))

; Test Cases
(define test-case-1 (lambda () (let* ((root '(1 0)) (root-node (make-node root 0 0 -1 (id))) (test-tree (make-tree root-node '()))) test-tree)))
(define test-case-2 (lambda () (let ((root (test-case-1))) (expand root) root)))
(define test-case-3
  (lambda ()
      (set! tree (test-case-2))
      (mcts 1)))

; Search Functions
(define search
  (lambda (root)
    (let ((root-node (make-node root 0 0 -1 (id))))
      (set! tree (make-tree root-node (create-children root-node)))
      (mcts 3))))

(define mcts
  (lambda (count)
    (cond
      ((= count 0) (return-best-move))
      (else (mcts-inner tree 0)))))

; TODO: Shuffle
(define return-best-move
  (lambda (current-tree)
    (let ((children (tree-children current-tree)))
      (car (list-sort (lambda (c1 c2) (> (node-ucb (tree-root c1)) (node-ucb (tree-root c2)))) children)))))

(define mcts-inner
  (lambda (current-tree parent-eval-count)
    (cond
      ((is-leaf? current-tree)
        (cond
          ((= 0 (node-n (tree-root current-tree)))
            (display "BEFORE")
            (display tree)
            (newline)
            (set-node-t! (tree-root current-tree) (rollout current-tree))
            (display "AFTER")
            (display tree)
            (newline))
          (else (expand current-tree))))
      (else (mcts-inner (return-best-move current-tree) (node-n (tree-root current-tree)))))
    (update current-tree parent-eval-count)))

(define update
  (lambda (current-tree parent-eval-count)
    (let* ((current-root (tree-root current-tree))
            (n (node-n current-root)))
      (set-node-n! current-root (+ n 1))
      (display "TREEEEE BEFOREE ")
      (display tree)
      (newline)
      (display "TSUM ")
      (display (tsum current-tree))
      (newline)
      (display "NODE T ")
      (display (node-t current-root))
      (newline)
      (set-node-t! current-root (+ (tsum current-tree) (node-t current-root)))
      (display "TREEEEE AFTERR ")
      (display tree)
      (newline)
      (set-node-ucb! current-root (UCB (node-t current-root) (node-n current-root) parent-eval-count)))))

(define tsum
  (lambda (current-tree)
    (let ((children (tree-children current-tree)) (sum 0))
      (for-each (lambda (child-tree) (set! sum (+ sum (node-t (tree-root child-tree))))) children)
      sum)))

(define is-leaf?
  (lambda (current-tree)
    (null? (tree-children current-tree))))