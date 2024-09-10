(define lst1 '(1 2 3 4 5 6))

(define shuffle-lst
  (lambda (lst)
    (cond
      ((null? lst) '()) ; Base case: empty list returns an empty list
      (else
        (let ((picked-element (find-element lst (random (len lst)))))
          (cons picked-element (shuffle-lst (remove lst picked-element))))))))
; A function that recursively builds a shuffled list.

(define (remove lst item)
  (cond
    ((null? lst) '())
    ((equal? (car lst) item) (cdr lst)) ; Remove the first occurrence of the item
    (else (cons (car lst) (remove (cdr lst) item)))))
; Function to remove an element from a list

(define find-element
  (lambda (lst index)
    (cond
      ((= index 0) (car lst)) ; Return the element at the index
      (else (find-element (cdr lst) (- index 1))))))
; Function to find an element at a specific index in a list

(define (len lst)
  (cond
    ((null? lst) 0)
    (else (+ 1 (len (cdr lst))))))
; Function to calculate the length of a list

; Example usage:
(display (shuffle-lst lst1))
