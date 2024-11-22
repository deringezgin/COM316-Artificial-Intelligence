(define threshold-weights (list (- (random 10) 5) (- (random 10) 5) (- (random 10) 5))) 
(define epsilon 1)

(define perceptron
  (lambda (lst)
    (get-node lst threshold-weights)))

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

(define g
  (lambda (x)
    (/ 1 (+ 1 (exp (- x))))))

(define learn
  (lambda (input-target-lst)
    (let* ((input-lst (car input-target-lst))
           (target (cadr input-target-lst))
           (perceptron-answer (perceptron input-lst))
           (derivative (* perceptron-answer (- 1 perceptron-answer)))
           (error (- target perceptron-answer))
           (activations (cons -1 input-lst))
           (deltaWs (map (lambda (x) (* error epsilon derivative x)) activations)))
      (set! threshold-weights (map + deltaWs threshold-weights)))))

(define do-learning-random
  (lambda (lst count num-inputs)
    (set! threshold-weights (get-threshold-weights num-inputs)) 
    (do-learning2 lst lst count)))

(define do-learning
  (lambda (lst count)
    (do-learning2 lst lst count)))

(define do-learning2
  (lambda (lst0 lst count)
    (cond
      ((< count 0)
         threshold-weights)
      ((null? lst) (do-learning2 lst0 lst0 count)) 
      (else
         (learn (car lst))
         (do-learning2 lst0 (cdr lst) (- count 1))))))

(define get-threshold-weights 
  (lambda (num)
    (if (< num 0)
        '()
        (cons (- (random 11) 5) (get-threshold-weights (- num 1)))))) 

