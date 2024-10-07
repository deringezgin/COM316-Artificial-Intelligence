(define get-next-goal (lambda (point) (cadr (mcts-search (list #t goal robot) 50 10000))))
