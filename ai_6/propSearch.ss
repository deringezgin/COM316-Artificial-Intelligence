; Derin Gezgin | Jay Nash
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #6
; Due October 10 2024
; File that has the complete code for First Order Logic Example
; We implemented a verison of First Order Logic that it targets the specific rules while searching.
; Another differnece from the original PropSearch is that we loop through facts rather than the rules.
; Prof. Parker said that this is ok but we still think that our implementation is not completely right
; as we're speicifically running a rule in our search function depending on what the (car) of the fact is
; rather than looping through all the rules while having a single fact.
; Our code can take care of height, visited and stability but it can't do all of these in a complete loop. 
; As the partial credit (up to %90) is extensive explanation with some code,
; we'll explain what we'd do different for a completely correct solution with comments on top of our ""working"" code.

; As the first order logic can create generalized rules, we have to rules in our rule set.
; One of them is to go from one square to another one. This also prevents us from going to another visited square.
; The other one checks if we're on the goal or not. 
(define rules
  '(
     (((visited p1) (adjacent p1 p2) (not (obstacle p2)) (not (visited p2))) ((visited p2)))
     (((visited p1) (goal p1)) ((finished)))))

; These are our facts and anything can be added here. 
(define facts
  '((goal (2 2)) (obstacle (1 1)) (obstacle (2 1)) (visited (0 0))))

; Function to get a literal point (x y) and return all of its adjacents.
; It also ensures that we don't return any points that are over the limits of the grid. (Like [3 3] [-1 1] etc.)
(define get-adjacent
  (lambda (p1)
    (let* ((x (car p1)) (y (cadr p1))
           (adjacents (filter (lambda (p2) (and (>= (car p2) 0) (<= (car p2) 2) (>= (cadr p2) 0) (<= (cadr p2) 2)))
                        (list (list (+ x 1) y) (list (- x 1) y) (list x (+ y 1)) (list x (- y 1))))))
      adjacents)))

; Function to generate adjacents of a point. It calls the get-adjacent function to generate the points and 
; with the input it gets from there, it creates facts like (adjacent (1 1) (2 1))
(define generate-adjacents
  (lambda (p1)
    (let* ((adjacents (get-adjacent p1))
            (adjacent-facts (map (lambda (p2) (list 'adjacent p1 p2)) adjacents)))
      (set! facts (append facts adjacent-facts))
      adjacent-facts)))

; Function to clean the facts list from the adjacents.
; We call this at each iteration of our facts list so that facts list doesn't overload. 
(define clean-adjacents
  (lambda ()
    (set! facts (filter (lambda (fact) (not (eq? (car fact) 'adjacent))) facts))))

; Same ModusPonens and ModusPonens2 as Prof. Parker, except we made a slight change but nothing structural. 
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

; This is our search function. It completely works right now but this implementation can be considered "half brute-force".
; The first to conditional checks are the same as the original propSearch.ss.
; In the else condition, rather than looping through the rules and checking for the facts, we loop through the rules and check for the rules.
; The bruteforce part of our solution is that, inside of the let (where we bind the firstFact), we check if our fact is a
; visited fact or a goal fact. If these two are the case, we check the 1st or the 2nd rule in the rule list according the which type of fact we have.
; If it's a neither kind of fact, we just skip to the next iteration of search and check the next fact.
; Our implementation checks the facts in a cycle so it won't check the same fact again before checking the other facts.
; At the same time then ew facts added to the fact list will be in the beginning of the list so the new facts will be prioritized rather than the old ones.
; While this works completely, to complete our implementation of the code, we would follow the following approach:
;    Rather than targeting specific kind of facts, we'd use the search-fact function we created.
;    In this case, we'll loop through all the facts and if search fact returns true (we have a match) we'd try to apply binding.
;    If we have a single point (like (goal p1) (visited p1) etc..) we'd use search true the fact list [with another function which we haven't create]
;    to find points with matching p1 or p2s. This also means that we shouldn't clear the (adjacent p1 p2) facts from the list in each iteration.
;    The only issue with this approach would be in the initial fact list, we don't have any adjacent facts by default. So we should start our program
;    with genarating the adjacents of the starting point.
;
;    Looping through the rules is also an option. In this case, for each rule we loop through we can also go through the facts and see if there's anything matching.
;    To check the matching case, we can use the search-fact function. For each match we can try to bind rules.
;
;    On the other hand, to implement the path tracking part of the problem, we can just have a stack.
;    When we visit new locations, we add them to the stack. Whenver we add a new location to the stack, we can check if it's adjacent to the previous one.
;    If so, we can add it directly. But if it's not adjacent (which means we're jumping to another point), we'd pop from the stack until the top of the stack is adjacent.
;    Using this method, we'll have a fully connected path (not the shortest) at the end of our implementation.
;    At the same time, we can just calcualte the path when we're done with the search.
;
;    Implementing height and stability would be the easiest among our missing parts as we can direclty add them to our current rulebase.
;    The only issue we can think of is we only operate through 2 rules so we have to add more conditions to cond or we have to go with the pseudo solution we explained.
;
;    In summary, functions we created (bind / search) would be vry helpful in the complete implementation to the problem.

(define (search count)
  (cond
    ((member 'finished facts) (display "goal found") (newline))
    ((<= count 0) (display "not found") (newline))
    (else
      (let* ((firstFact (car facts)) (remainingFacts (append (cdr facts) (list firstFact))))
        (cond
          ((equal? (car firstFact) 'visited)
            (for-each (lambda (adjacent)
                        (let* ((bindedRule (bind (car rules) (cadr adjacent) (caddr adjacent)))
                                (newFact (ModusPonens bindedRule)))
                          (cond
                            ((not (null? newFact)) (set! remainingFacts (append remainingFacts newFact))))))
              (generate-adjacents (cadr firstFact)))
            (clean-adjacents)
            (set! facts remainingFacts))
          ((equal? (car firstFact) 'goal)
            (let* ((bindedRule (bind (cadr rules) (cadr firstFact) '()))
                    (newFact (ModusPonens bindedRule)))
              (cond
                ((not (null? newFact)) (set! facts (append (car newFact) remainingFacts)))
                (else (set! facts remainingFacts)))))
          (else
            (set! facts remainingFacts)))
        (search (- count 1))))))


; The next 4 functions are a set of functions we coded, that perfectly works but didn't have time to implement completely.
; They are a crucial step for the complete implementation for First-Order Logic in which we loop through each rule while having a fact.
; Their implementation is similar to the binding functions.
; The search-fact function takes a complete set of rule and a fact.
; It returns if the predicate of the fact (visited / obstacle / goal / etc...) exists in the rule.
; This is crucial because when we use this and loop through the ruels, our program won't try to bind a fact to a rule that it doesn't exist in.
; It return #t if the fact exists somewhere in the rule and returns #f if it doesn't exist anywhere.
(define search-fact
  (lambda (rule fact)
    (let ((truth-values (map (lambda (statement) (check-statement statement fact '())) rule)))
      (check-list (map check-list truth-values)))))

; Helper function for check-list. It basically returns true if there's at least one #t in the list. Othewise it returns #f
(define check-list
  (lambda (lst)
    (cond
      ((null? lst) #f)
      ((eq? (car lst) #t) #t)
      (else (check-list (cdr lst))))))

; For each statement it checks for a match usign the helper function (check-atom). It returns #t / #f accordingly.
(define check-statement
  (lambda (statement fact checks)
    (cond
      ((null? statement) (reverse checks))
      (else (check-statement (cdr statement) fact (cons (check-atom (car statement) fact) checks))))))

; The base function for the checking part of the program. This checks individual atoms and returns #t if there's a match
(define check-atom
  (lambda (atom fact)
    (cond
      ((list? (cadr atom)) (display (cadr atom)) (eq? (car fact) (caadr atom)))
      (else (display atom) (eq? (car fact) (car atom))))))

; The next 3 functions are used to create bindings.
; In this "bind" function, we get the rule, the value of p1 and the value of p2. 
; For each statement in the rule, the helper function (bind-statement) is called.
; At the end, it'll return the complete rule set with the appropriate bindings.
(define bind
  (lambda (rule p1-val p2-val)
    (map (lambda (statement) (bind-statement statement p1-val p2-val '())) rule)))

; Helper function to "bind" that will take a statement, p1-value, p2-value, and a cumilative variable (new-rule).
; If the rule is empty it returns the new rule as our base case of the recursion.
; If the rule is not empty, it calls the 3rd helper function (bind-predicate) to bind every single predicate and
; accumulate the result in the 4th argument.
(define bind-statement
  (lambda (rule p1-val p2-val new-rule)
    (cond
      ((null? rule) (reverse new-rule))
      (else
        (bind-statement (cdr rule) p1-val p2-val (append (bind-predicate (car rule) p1-val p2-val '()) new-rule))))))

; This is the final helper function for binding that'll take a single statement (visited p1) and bind it.
; It takes the binding values like p1 p2 and replaces the p1s and p2s in the statement with the binding values.
; For example (visited p1) and p1 = (2 2) will return (visited (2 2))
(define bind-predicate
  (lambda (pred p1-val p2-val new-pred)
    (cond
      ((null? pred) (list (reverse new-pred)))
      (else
        (cond
          ((list? (car pred)) (set! new-pred (cons (car (bind-predicate (car pred) p1-val p2-val '())) new-pred)))
          ((eq? (car pred) 'p1) (set! new-pred (cons p1-val new-pred)))
          ((eq? (car pred) 'p2) (set! new-pred (cons p2-val new-pred)))
          (else (set! new-pred (cons (car pred) new-pred))))
        (bind-predicate (cdr pred) p1-val p2-val new-pred)))))

; We call the search here
(search 100)