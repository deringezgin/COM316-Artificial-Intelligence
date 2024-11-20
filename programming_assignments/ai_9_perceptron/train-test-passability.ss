(define training-input-passability '())
(define test-epsilon '(10 100 1000))
(define test-generation '(1000 10000 100000 1000000))
(define num-generation 0)
(define epsilon 0)

(define train-logic-function
  (lambda (training-input epsilon-val generation)
    ; Function to load the perceptron file and train the perceptron usinf the training input for certain number of
    ; generations with a pre-set learning rate
    (load "perceptron.ss")
    (set! epsilon epsilon-val)
    (display "Training the perceptron...")
    (newline)
    (display "Generation Count: ")
    (display generation)
    (display " ... Epsilon-Value: ")
    (display epsilon-val)
    (newline)
    (do-learning training-input generation)
    (display "Testing the perceptron that is trained...")
    (newline)
    (test-perceptron training-input)
    ))

(define test-perceptron
  (lambda (training-input)
    ; After we trained the perceptron, this function can test that perceptron by taking the training inputs
    ; It will go through each input and pass it through the perceptron. Finally, it will print the perceptron output
    (cond
      ((null? training-input) (newline) (newline) (newline))
      (else
        (display "Testing: ")
        (display (caar training-input))
        (display " ... Expected Output: ")
        (display (cdar training-input))
        (display " ... Perceptron Output: ")
        (display (perceptron (caar training-input)))
        (display " ... Check Over/Under: ")
        (display (normal-output (perceptron (caar training-input))))
        (display " ... True Output? ")
        (display (equal? (cadar training-input) (normal-output (perceptron (caar training-input)))))
        (newline)
        (test-perceptron (cdr training-input))
        ))
    ))

(define normal-output
  (lambda (num)
    ; Function that will return 0 if the number is less than 0.5 and will return 1 if number is larger than 0.5
    (cond
      ((< num 0.5) 0)
      (else 1))))

(define run-test-epsilon
  (lambda (test-input epsilon-lst generation-count)
    ; Function that will test all the epsilon combinations for a fixed generation count
    (cond
      ((null? epsilon-lst) '())
      (else
        (train-logic-function test-input (car epsilon-lst) generation-count)
        (run-test-epsilon test-input (cdr epsilon-lst) generation-count)
        ))))

(define run-test
  (lambda (test-input epsilon-lst generation-lst)
    ; Main testing function which will call the second level testing function and test all the generation x epsilon
    ; combinations
    (cond
      ((null? generation-lst) '())
      (else
        (run-test-epsilon test-input epsilon-lst (car generation-lst))
        (run-test test-input epsilon-lst (cdr generation-lst))))))

(display "==================== Training the perceptron for Passability ====================")
(newline)
(run-test training-input-or test-epsilon test-generation)

