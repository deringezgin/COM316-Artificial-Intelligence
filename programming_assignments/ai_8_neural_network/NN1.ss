(define threshold-weights '(((1.5 1 1))))
; When I run these set of weights, I get the following output
; Input: (0 0) | Output: 0.182 | Smaller than 0.5 --> 0
; Input: (0 1) | Output: 0.377 | Smaller than 0.5 --> 0
; Input: (1 0) | Output: 0.377 | Smaller than 0.5 --> 0
; Input: (1 1) | Output: 0.622 | Larger then 0.5 --> 1
; In this case, these weights belong to the AND logic function

;(define threshold-weights '(((.6 1 -.5) (.6 -.5 1)) ((.4 1 1))))

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