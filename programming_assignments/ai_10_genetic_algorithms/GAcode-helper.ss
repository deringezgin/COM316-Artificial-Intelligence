; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #10
; Due November 26 2024
; This file has the necessary modifications for the evaluation of an individual in our GA training process.

(define GAevaluate-ind
  (lambda (ind)
    ; Please change this line into part-2 / part-3 to try other modifications
    (part-1 ind)))

(define part-1
  (lambda (lst)
    ; Function that returns the sum of integeres in a list.
    ; We can also use this as a fitness function as in part-1 we are aiming to have as large numbers as possible.
    (cond
      ((null? lst) 0)
      (else (+ (car lst) (part-1 (cdr lst)))))))

(define part-2
  (lambda (lst)
    ; Function that will take an individual and return the count of numbers in that individual whom are divisible by 10
    ; This is used in the second part of the assignment
    (cond
      ((null? lst) 0)
      ((= (modulo (car lst) 10) 0) (+ 1 (part-2 (cdr lst))))
      (else (+ 0 (part-2 (cdr lst)))))))

; This is our target list where the GA tries to learn exactly these numbers
(define int-target-lst '(101 102 103 104 105 106 107 108 109 110))

(define part-3
  (lambda (lst)
    ; In the 3rd part, the program compares the individual with the target list
    ; This is used in the third part of the assignment
    (compare-lst lst int-target-lst 0)))

(define compare-lst
  (lambda (lst1 lst2 count)
    ; Helper function to compare two lists
    (cond
      ((or (null? lst1) (null? lst2)) count)
      ((= (car lst1) (car lst2)) (compare-lst (cdr lst1) (cdr lst2) (+ count 1)))
      (else (compare-lst (cdr lst1) (cdr lst2) count)))))
