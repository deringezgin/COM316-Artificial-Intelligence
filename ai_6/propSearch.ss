(define rules
  '(
     (((visited p1) (adjacent p1 p2) (not (obstacle p2))) (visited p2))
     (((visited p1) (adjacent p1 p2) (not (obstacle p2))) (visited p2))
     (((visited p1) (goal p1)) (finished))))

(define facts
  '((goal (2 2)) (obstacle (1 1))))

(define clean-adjacents
  (lambda ()
    (set! facts (filter (lambda (fact) (not (eq? (car fact) 'adjacent))) facts))))

  (define get-adjacent
    (lambda (p1)
      (let* ((x (car p1)) (y (cadr p1))
             (adjacents (filter (lambda (p2) (and (>= (car p2) 0) (<= (car p2) 2) (>= (cadr p2) 0) (<= (cadr p2) 2)))
                          (list (list (+ x 1) y) (list (- x 1) y) (list x (+ y 1)) (list x (- y 1))))))
        adjacents)))

(define generate-adjacents
  (lambda (p1)
    (let* ((adjacents (get-adjacent p1))
            (adjacent-facts (map (lambda (p2) (list 'adjacent p1 p2)) adjacents)))
      (set! facts (append facts adjacent-facts)))))

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
       ))))
