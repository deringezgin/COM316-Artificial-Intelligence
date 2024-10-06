(define rules
  '(
     (((visited p1) (adjacent p1 p2) (not (obstacle p2))) (visited p2))
     (((visited p1) (adjacent p1 p2) (not (obstacle p2))) (visited p2))
     (((visited p1) (goal p1)) (finished))))

(define facts
  '((goal '(2 2)) (obstacle '(1 1))))

(define (get-adjacent p1
  (let* ((x (car p1)) (y (cadr p1))
         (adjacents (filter
                      (lambda (p2) (and (>= (car p2) 0) (<= (car p2) 2) (>= (cadr p2) 0) (<= (cadr p2) 2)))
                      ((,(+ x 1) ,y) (,(+ x -1) ,y) (,x ,(+ y 1)) (,x ,(+ y -1))))))
    adjacents))

(define (clean-adjacency)
  (set! facts (filter (lambda (fact) (not (eq? (car fact) 'adjacent))) facts)))


(define (unify pred1 pred2)
  (cond ((equal? (ca(load r pred1) (car pred2))
         (unify-vars (cdr pred1) (cdr pred2)))
        (else #f)))

(define (unify-vars vars1 vars2)
  (cond ((null? vars1) '())
        ((symbol? (car vars1)) (cons (list (car vars1) (car vars2))
                                     (unify-vars (cdr vars1) (cdr vars2))))
        ((equal? (car vars1) (car vars2)) (unify-vars (cdr vars1) (cdr vars2)))
        (else #f)))

(define (ModusPonens rule)
  (let* ((conditions (car rule))
         (conclusion (cadr rule))
         (bindings (check-conditions conditions)))
    (if bindings
        (subst-bindings bindings conclusion)
        '())))

(define (check-conditions conditions)
  ;; Unify all conditions with the current facts
  (foldl (lambda (cond acc)
           (if acc
               (let ((fact (find-fact cond)))
                 (if fact (unify cond fact) #f))
               #f))
         '() conditions))

(define (find-fact cond)
  (find (lambda (fact) (unify cond fact)) facts))


(define (search count)
  (cond
    ((member 'finished facts)
     (display "goal found")
     (newline))
    ((<= count 0)
     (display "not found")
     (newline))
    (else
      (let* ((firstRule (car rules))
             (remainingRules (append (cdr rules) (list firstRule)))
             (newFact (ModusPonens firstRule)))
        (cond
          ((null? newFact)
           (set! rules remainingRules)
           (search (- count 1)))
          (else
           ;; Add new facts
           (set! facts (append newFact facts))
           ;; Clean up adjacency facts after move
           (clean-adjacency)
           ;; Generate new adjacency facts for the new move
           (generate-adjacents)
           (set! rules remainingRules)
           (search (- count 1))))))))

(define (generate-adjacents)
  (let ((visited-point (find (lambda (fact) (eq? (car fact) 'visited)) facts)))
    (when visited-point
      (let ((p1 (cadr visited-point)))
        (for-each
         (lambda (p2)
           (set! facts (cons `(adjacent ,p1 ,p2) facts)))
         (get-adjacent p1))))))

(search 1000)