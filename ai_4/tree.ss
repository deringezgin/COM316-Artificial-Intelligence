(define tree (make-vector 0))
(define branching-factor 5)
(define depth 3)

(define get-node-count
  (lambda (bf max-depth)
    (cond
      ((= bf 1) (+ 1 max-depth))
      (else (/ (- (expt bf (+ max-depth 1)) 1) (- bf 1))))))

(define tree-length (get-node-count branching-factor depth))

(define reset-tree
  (lambda (node)
    (set! tree (make-vector tree-length -1))
    (vector-set! tree 0 node)))

(define get-parent
  (lambda (index)
    (cond
      ((= index 0) 0)
      (else (floor (/ (- index 1) branching-factor))))))

(define get-parent-node
  (lambda (index)
    (vector-ref tree (get-parent index))))

(define range
  (lambda (start end)
    (cond
      ((>= start end) '())
      (else (cons start (range (+ start 1) end))))))

(define get-children
  (lambda (index)
    (range (+ (* branching-factor index) 1) (+ (* branching-factor index) 6))))

(define get-children-nodes
  (lambda (index)
    (map (lambda (child-index) (vector-ref tree child-index)) (get-children index))))

(define is-leaf-node
  (lambda (index)
    (>= (+ (* branching-factor index) 1) tree-length)))  ; Check if index has children in range

(define heuristic
  (lambda (node)
    (map (lambda (i) (+ (* node 100) i)) (range 1 6))))

(define update-children
  (lambda (indices new-values)
    (cond
      ((null? indices) '())
      (else
        (vector-set! tree (car indices) (car new-values))
        (update-children (cdr indices) (cdr new-values))))))

(define expand-tree
  (lambda ()
    (define expand-node
      (lambda (index)
        (if (not (is-leaf-node index))
            (let ((new-values (heuristic (vector-ref tree index))))
              (update-children (get-children index) new-values)
              (for-each expand-node (get-children index))))))  ; Recursively expand all children
    (expand-node 0)))  ; Start from root node

(reset-tree 1)
(expand-tree)
(newline)
(display tree)
(newline)