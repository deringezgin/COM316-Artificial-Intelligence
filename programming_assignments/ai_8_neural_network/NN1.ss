; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #8
; Due November 19 2024
; File that has the code with responses to the questions in the homework.

;(define threshold-weights '(((1.5 1 1))))
; When I run these set of weights, I get the following output
; Input: (0 0) | Output: 0.182 | Smaller than 0.5 --> 0
; Input: (0 1) | Output: 0.377 | Smaller than 0.5 --> 0
; Input: (1 0) | Output: 0.377 | Smaller than 0.5 --> 0
; Input: (1 1) | Output: 0.622 | Larger then 0.5 --> 1
; In this case, these weights belong to the AND logic function

;(define threshold-weights '(((15 10 10))))
; I multiplied the weights by 10 to make the trues (above .5) are not so close to the falses (below .5).
; As I multiplied them by 10, the magnitude of Trues and Falses also increased.
; When I run these set of weights, I get the following output
; Input: (0 0) | Output: 0.000000306 | Smaller than 0.5 --> 0
; Input: (0 1) | Output: 0.00669 | Smaller than 0.5 --> 0
; Input: (1 0) | Output: 0.00669 | Smaller than 0.5 --> 0
; Input: (1 1) | Output: 0.99331 | Larger then 0.5 --> 1
; In this case, these weights belong to the AND logic function

;(define threshold-weights '(((5 10 10))))
; I took the weights for OR logic function from NN0.ss and multiplied it by 10
; When I run these set of weights, I get the following output
; Input: (0 0) | Output: 0.00693 | Smaller than 0.5 --> 0
; Input: (0 1) | Output: 0.99331 | Larger than 0.5 --> 1
; Input: (1 0) | Output: 0.99331 | Larger than 0.5 --> 1
; Input: (1 1) | Output: 0.99999 | Larger then 0.5 --> 1
; In this case, these weights belong to the OR logic function

(define threshold-weights '(((30 50 -25) (30 -25 50)) ((20 50 50))))
; I took the weights for OR logic function from NN0.ss and multiplied it by 50
; When I run these set of weights, I get the following output
; Input: (0 0) | Output: 0.00000000206 | Smaller than 0.5 --> 0
; Input: (0 1) | Output: 0.99999 | Larger than 0.5 --> 1
; Input: (1 0) | Output: 0.99999 | Larger than 0.5 --> 1
; Input: (1 1) | Output: 0.00000000403 | Smaller than 0.5 --> 0
; In this case, these weights belong to the XOR logic function


(define NN
  (lambda (lst)
    (NN2 lst threshold-weights)))

(define NN2
  (lambda (lst tw)
    (display lst)
    (newline)
    (if (null? tw)
      lst
    ;else
      (let ((next-level (get-next-level lst (car tw))))
         (NN2 next-level (cdr tw))))))

(define get-next-level
  (lambda (lst twl)
     (if (null? twl)
       '()
     ;else
       (cons (get-node lst (car twl)) (get-next-level lst (cdr twl))))))

(define get-node
  (lambda (lst twn)
    (let ((threshold (car twn))
          (weights (cdr twn)))
      (g (+ (get-activations lst weights) (- threshold))))))

(define get-activations
  (lambda (lst w)
    (if (null? lst)
       0
    ;else
       (+ (* (car lst) (car w)) (get-activations (cdr lst) (cdr w))))))

; Modified g function to use sigmoid activation function rather than the step function
(define g
  (lambda (x)
    (/ 1 (+ 1 (exp (- x))))))

(define test-nn
  (lambda ()
    (display "Output for ")
    (NN '(0 0))
    (newline)
    (display "Output for ")
    (NN '(0 1))
    (newline)
    (display "Output for ")
    (NN '(1 0))
    (newline)
    (display "Output for ")
    (NN '(1 1))
    (newline)))

; Running the test function to  see the output for each input combination
(test-nn)