(define rules
  '(
     (((visited p1) (adjacent p1 p2) (not (obstacle p2)) (not (visited p2))) ((visited p2)))
     (((visited p1) (goal p1)) ((finished)))))

(define facts
  '((goal (2 2)) (obstacle (1 2)) (obstacle (2 1)) (visited (0 0))))

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
      (set! facts (append facts adjacent-facts))
      adjacent-facts)))

(define clean-adjacents
  (lambda ()
    (set! facts (filter (lambda (fact) (not (eq? (car fact) 'adjacent))) facts))))

(define ModusPonens
  (lambda (rule)
    (ModusPonens2 (car rule) (cadr rule))))

(define ModusPonens2
  (lambda (b a)
    (if (null? b)
        a
    ;else
        (let ((fact1 (car b)))
          (if (equal? (car fact1) 'not)
              (let ((fact1n (cadr fact1)))
                (if (member fact1n facts)
                    '()
                ;else
                    (ModusPonens2 (cdr b) a)))
           ;else
              (if (member fact1 facts)
                  (ModusPonens2 (cdr b) a)
              ;else
                  '()))))))

(define (search count)
  (display "FACTS IN THE BEGINNING OF SEARCH")
  (display facts)
  (newline)
  (newline)
  (cond
    ((member 'finished facts) (display "goal found") (newline))
    ((<= count 0) (display "not found") (newline))
    (else
      (let* ((firstFact (car facts)) (remainingFacts (append (cdr facts) (list firstFact))))
        (cond
          ((equal? (car firstFact) 'visited)
            (for-each (lambda (adjacent)
                        (let* ((bindedRule (replace (car rules) (cadr adjacent) (caddr adjacent)))
                                (newFact (ModusPonens bindedRule)))
                          (cond ((not (null? newFact)) (set! remainingFacts (append remainingFacts newFact)))))) (generate-adjacents (cadr firstFact)))
            (clean-adjacents)
            (set! facts remainingFacts))
          ((equal? (car firstFact) 'goal)
            (let* ((bindedRule (replace (cadr rules) (cadr firstFact) '()))
                    (newFact (ModusPonens bindedRule)))
              (cond
                ((not (null? newFact)) (set! facts (append (car newFact) remainingFacts)))
                (else (set! facts remainingFacts)))))
          (else
            (set! facts remainingFacts)))
        (search (- count 1))))))

(define replace
  (lambda (rule p1-val p2-val)
    (map (lambda (statement) (replace-statement statement p1-val p2-val '())) rule)))

(define replace-statement
  (lambda (rule p1-val p2-val new-rule)
    (cond
      ((null? rule) (reverse new-rule))
      (else
        (replace-statement (cdr rule) p1-val p2-val (append (replace-atom (car rule) p1-val p2-val '()) new-rule))))))

(define replace-atom
  (lambda (pred p1-val p2-val new-pred)
    (cond
      ((null? pred) (list (reverse new-pred)))
      (else
        (cond
          ((list? (car pred)) (set! new-pred (cons (car (replace-atom (car pred) p1-val p2-val '())) new-pred)))
          ((eq? (car pred) 'p1) (set! new-pred (cons p1-val new-pred)))
          ((eq? (car pred) 'p2) (set! new-pred (cons p2-val new-pred)))
          (else (set! new-pred (cons (car pred) new-pred))))
        (replace-atom (cdr pred) p1-val p2-val new-pred)))))

(search 100)