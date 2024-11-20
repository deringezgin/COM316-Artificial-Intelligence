(define training-input-passability
  '(
     ((0.95 0.3) 1) ((0.98 0.4) 1) ((0.92 0.2) 1) ((0.97 0.1) 1) ((0.93 0.45) 1)
    ((0.96 0.35) 1) ((0.99 0.25) 1) ((0.94 0.15) 1) ((0.91 0.05) 1) ((0.97 0.48) 1)
    ((0.93 0.2) 1) ((0.94 0.1) 1) ((0.95 0.05) 1) ((0.96 0.3) 1) ((0.97 0.4) 1)
    ((0.98 0.45) 1) ((0.99 0.35) 1) ((0.92 0.1) 1) ((0.91 0.2) 1) ((0.93 0.3) 1)
    ((0.94 0.4) 1) ((0.95 0.25) 1) ((0.96 0.15) 1) ((0.97 0.05) 1) ((0.98 0.2) 1)
    ((0.85 0.4) 0) ((0.9 0.6) 0) ((0.88 0.55) 0) ((0.7 0.4) 0) ((0.95 0.55) 0)
    ((0.89 0.51) 0) ((0.86 0.65) 0) ((0.82 0.35) 0) ((0.9 0.52) 0) ((0.87 0.6) 0)
    ((0.9 0.5) 0) ((0.85 0.6) 0) ((0.8 0.7) 0) ((0.75 0.8) 0) ((0.7 0.9) 0)
    ((0.65 0.95) 0) ((0.6 1.0) 0) ((0.55 0.85) 0) ((0.5 0.75) 0) ((0.45 0.65) 0)
    ((0.88 0.6) 0) ((0.9 0.55) 0) ((0.89 0.52) 0) ((0.87 0.58) 0) ((0.86 0.6) 0)
     ))

(define test-epsilon '(10 100 1000))
(define test-generation '(1000 10000 100000 1000000))
(define num-generation 0)
(define epsilon 0)
(define correct-count 0)

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
    (set! correct-count 0)
    (test-perceptron training-input)
    (display "Correct Guess Ratio: ")
    (display (exact->inexact (/ correct-count (length training-input-passability))))
    (newline) (newline) (newline)
    ))

(define test-perceptron
  (lambda (training-input)
    ; After we trained the perceptron, this function can test that perceptron by taking the training inputs
    ; It will go through each input and pass it through the perceptron. Finally, it will print the perceptron output
    (cond
      ((null? training-input) (newline))
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
        (cond
          ((equal? (cadar training-input) (normal-output (perceptron (caar training-input))))
            (set! correct-count (+ 1 correct-count))))
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
(run-test training-input-passability test-epsilon test-generation)

