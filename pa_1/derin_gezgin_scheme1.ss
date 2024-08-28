; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Scheme #1
; Due August 29 2024

; For my own usage
(define println
  (lambda (n)
    (display n)
    (newline)))


; ___________________________________________________________________________
; Function #1 is_big

(define is_big
  (lambda (x)
    (> x 1000)))

(println "Function #1 is_big (returns true if the input is bigger than 1000)")
(display "x = 300 --> ")
(println (is_big 300))
(display "x = 1200 --> ")
(println (is_big 1200))
(newline)


; ___________________________________________________________________________
; Function #3 sqr

(define sqr
  (lambda (n)
    (* n n)))

(println "Function #2 sqr")
(display "(sqr 2) --> ")
(println (sqr 2))
(display "(sqr 4) --> ")
(println (sqr 4))
(newline)


; ___________________________________________________________________________
; Function #3 double

(define double
  (lambda (x)
    (* 2 x)))

(println "Function #3 double")
(display "(double 2) --> ")
(println (double 2))
(display "(double 8) --> ")
(println (double 8))
(newline)


; ___________________________________________________________________________
; Function #4 dist

(define sqr (lambda (n) (* n n))) ; Defining again to avoid confusion

(define dist
  (lambda (x1 y1 x2 y2)
    (let ((dx (- x1 x2)) (dy (- y1 y2)))
      (sqrt (+ (sqr dx) (sqr dy))))))

; Function #5 dist_pt
(define dist_pt
  (lambda (p1 p2)
    (let ((dx (- (car p1) (car p2))) (dy (- (car (cdr p1)) (car (cdr p2)))))
      (sqrt (+ (sqr dx) (sqr dy))))))

(define pointa '(1 2))
(define pointb '(3 5))

(println "Function #4 dist & #5 dist_pt")
(println "Testing points --> (1, 2) and (3, 5)")
(display "Using dist --> ")
(println (dist 1 2 3 5))
(display "Using dist_pt --> ")
(println (dist_pt pointa pointb))
(display "Are the results the same? --> ")
(println (= (dist 1 2 3 5) (dist_pt pointa pointb)))
(newline)


; ___________________________________________________________________________
; Function #6 find_tf
; finds if x is in the list lst. It returns #t or #f

(define find_tf
  (lambda (x lst)
    (cond
      ((null? lst) #f)
      ((eq? (car lst) x) #t)
      (else (find_tf x (cdr lst))))))

(define l1 '(this is a test))
(define a1 'this)
(define a2 'nope)

(println "Function #6 find_tf")
(display "l1 --> ")
(println l1)
(display "a1 --> ")
(println a1)
(display "a2 --> ")
(println a2)
(display "Looking for a1 in l1 --> ")
(println (find_tf a1 l1))
(display "Looking for a2 in l1 --> ")
(println (find_tf a2 l1))
(newline)


; ___________________________________________________________________________
; Function #7 find_posit
; returns index of x is in the list lst.  it returns -1 if not in list

(define finder
  (lambda (x lst ind)
    (cond
      ((null? lst) -1)
      ((eq? x (car lst)) ind)
      (else (finder x (cdr lst) (+ ind 1))))))

(define find_posit
  (lambda (x lst)
    (finder x lst 0)))

(define a3 'a)

(println "Function #7 find_posit")
(display "a3 --> ")
(println a3)
(display "Index of a1 in l1 --> ")
(println (find_posit a1 l1))
(display "Index of a2 in l1 (doesn't exist) --> ")
(println (find_posit a2 l1))
(display "Index of a3 in l1 --> ")
(println (find_posit a3 l1))
(newline)


; ___________________________________________________________________________
; Function #8 count_all
; returns the number of elements in the list lst

(define count_all
  (lambda (lst)
    (cond
      ((null? lst) 0)
      (else (+ 1 (count_all (cdr lst)))))))

(println "Function #8 count_all")
(display "Number of the elements in l1 --> ")
(println (count_all l1))
(display "Testing an empty list --> ")
(println (count_all '()))
(newline)


; ___________________________________________________________________________
; Function #9 Count_twos
; returns the number of 2s in the list lst

(define count_twos
  (lambda (lst)
    (cond
      ((null? lst) 0)
      ((eq? (car lst) 2) (+ 1 (count_twos (cdr lst))))
      (else (count_twos (cdr lst))))))

(define l2 '(2 2 3))
(define l3 '(3 3 3))

(println "Function #9 count_twos")
(display "l2 --> ")
(println l2)
(display "l3 --> ")
(println l3)

(display "Number of 2s in l1 --> ")
(println (count_twos l1))
(display "Number of 2s in l2 --> ")
(println (count_twos l2))
(display "Number of 2s in l3 --> ")
(println (count_twos l3))
(display "Number of 2s in an empty list --> ")
(println (count_twos '()))


; ___________________________________________________________________________
