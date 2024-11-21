;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GAcode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; G.B. Parker
; 4 Dec 2017
; Basic Genetic Algorithm code for finding a list of integers.
; Trains a randomly generated initial population.
; Modifying train and evaluation functions permits the
; use of the genetic operators to evolve solutions for other problems.
; Each individual is a single chromosome made up of a list of ints.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;***************************** Constants *****************************
(define gene-list-bits '(8 8 8 8 8 8 8 8))
(define inds-per-pop 64)
(define mutation-rate 200)  ; 1/200 chance of mutation at each gene
(load "GAcode-helper.ss")  ; user defined functions
; (load "GAfile")  ; file input/output - same as w-file

;****************************** train ********************************
; Trains an input population for the specified number of generations.  
; Displays the starting population and the population after training.

(define GAtrain
  (lambda (pop num-generations)
    (GAtrain2 pop num-generations num-generations)))

(define GAtrain2
  (lambda (pop num-generations total-num-generations)  
    (if (<= num-generations 0)
      pop
    ;else
      (let* ((fitnesses (GAevaluate-pop pop))
             (fit-list (GAget-sum-fits fitnesses 0)))
        (GAtrain2 (GAevolve pop fit-list) (- num-generations 1) 
                total-num-generations)))))


;***************************** make-pop ******************************
; Makes a population of individuals.  Input includes the number of
; individuals desired and the length of each.

(define make-pop
  (lambda ()
    (GAmake-pop inds-per-pop gene-list-bits)))

(define GAmake-pop
  (lambda (num-inds bits)
    (if (<= num-inds 0)
      '()
    ;else
      (cons (GAmake-ind bits) 
            (GAmake-pop (- num-inds 1) bits)))))


;***************************** make-ind ******************************
; Makes an individual of the desired length. Genes are randomly 0 or 1

(define GAmake-ind
  (lambda (bits)
    (if (null? bits)
      '()
    ;else
      (cons (random (expt 2 (car bits))) (GAmake-ind (cdr bits))))))


;***************************** evaluate-pop **************************
; Evaluates the fitness of the individuals in a population.  Returns a
; list of fitnesses corresponding to the list of inds in the pop.
; Define evaluate-ind in file "GAcode-helper"

(define GAevaluate-pop
  (lambda (pop)
    (if (null? pop)
      '()
    ;else
      (cons (GAevaluate-ind (car pop)) (GAevaluate-pop (cdr pop))))))


;*************************** get-sum-fits ****************************
; Puts the fitnesses in a list of cumulated fitnesses so that a random
; number between 0 and the max will select one of the individuals.

(define GAget-sum-fits
  (lambda (fit-list fit-sums)
    (if (null? fit-list)
      '()
    ;else
      (cons (exact->inexact (+ (car fit-list) fit-sums)) 
          (GAget-sum-fits (cdr fit-list) (+ (car fit-list) fit-sums))))))
  

;****************************** evolve *******************************
; The main control for running the GA operators

(define GAevolve
  (lambda (pop fit-list)
    (GAevolve2 pop fit-list (length pop))))

(define GAevolve2
  (lambda (pop fit-list count)
    (if (<= count 0)
      '()
    ;else
      (let ((ind1 (GAselect pop fit-list))
            (ind2 (GAselect pop fit-list)))
        (cons (GAmutate (GAcrossover ind1 ind2 gene-list-bits) 
                        gene-list-bits (* 2 mutation-rate))
              (GAevolve2 pop fit-list (- count 1)))))))
             

;****************************** select *******************************
; Stochastically selects an individual from the population. 
; (car (reverse fit-list)) is the sum of the fitnesses.

(define GAselect
  (lambda (pop fit-list)
    (let ((ind (GAselect2 pop fit-list (real-rand (car (reverse fit-list))))))
      (if (null? ind)
        (list-ref pop (random (length pop)))
      ;else
        ind))))

(define GAselect2
  (lambda (pop fit-list select-num)
    (cond
      ((null? fit-list)
        (display "Error in GAselect2  ")
        (display select-num)
        '())
      ((< select-num (car fit-list))
        (car pop))
      (else
        (GAselect2 (cdr pop) (cdr fit-list) select-num)))))


;***************************** crossover *****************************
; Takes in two individuals, randomly selects a crossover point, and
; calls crossover2.  The crossover point can be anywhere from 0 to one
; more than the length of the individuals.  Crossover2 returns an
; individual that is made up of ind1 genes until the crossover point
; and ind2 genes after that.  A crossover point of 0 will return ind2
; unchanged, one over the individual length will result in a return of
; ind1 unchanged.  This allows for some selection without crossover,
; although in most cases there will be a crossover.

(define GAcrossover
  (lambda (ind1 ind2 bits)
    (if (> (random 2) 0)
      (GAcrossover2 ind1 ind2 (random (+ (length ind1) 1)) 0)
    ;else
      (GAcrossgenes ind1 ind2 bits))))

(define GAcrossover2
  (lambda (ind1 ind2 cross-point count)
    (cond
      ((null? ind1)
        '())
      ((> cross-point count)
        (cons (car ind1) 
           (GAcrossover2 (cdr ind1) (cdr ind2) cross-point (+ count 1))))
      (else 
        ind2))))

(define GAcrossgenes
  (lambda (ind1 ind2 bits)
    (if (null? ind1)
      '()
    ;else
      (cons (GAcrossgenes2 (car ind1) (car ind2) 
                           (expt 2 (random (+ 1 (car bits)))))
            (GAcrossgenes (cdr ind1) (cdr ind2) (cdr bits))))))

(define GAcrossgenes2
  (lambda (gene1 gene2 cross-num)
    (+ (- gene1 (mod gene1 cross-num)) (mod gene2 cross-num))))

(define mod
  (lambda (num1 num2)
    (modulo num1 num2)))

  
;****************************** mutate *******************************
; Takes in an individual that has some random chance of having each
; gene flipped, which is designed to be a mutation.  The random chance
; is predefined as the mutation-rate, which is a constant.

(define GAmutate
  (lambda (ind bits rate)
    (cond
      ((null? ind)
        '())
      ((= (random rate) 0)
        (cons (GAmutate-gene (car ind) (random (car bits))) 
              (GAmutate (cdr ind) (cdr bits) rate)))
      ((= (random rate) 1)
        (cons (random (expt 2 (car bits))) (GAmutate (cdr ind) (cdr bits) rate)))
      (else
        (cons (car ind) (GAmutate (cdr ind) (cdr bits) rate))))))

(define GAmutate-gene
  (lambda (gene bit)
    (let ((e2b (expt 2 bit)))
      (if (= (quotient (mod gene (expt 2 (+ 1 bit))) e2b) 1)
        (- gene e2b)
      ;else
        (+ gene e2b)))))
 


(define real-rand
  (lambda (real)
    (exact->inexact (* real (/ (+ 1 (random 999999998)) 999999999)))))

  



