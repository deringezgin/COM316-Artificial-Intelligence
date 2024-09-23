; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #3
; Due September 24 2024
; File that has the complete code for the main file
; This file runs a test with the Real-Time A* and the Hill Climber on the same grid and saves the result in a global variable

(define num-col-row 30)
(define pause-num 100000)
(define size (floor (/ 700 num-col-row)))
(define obstacle-density 30)

(define copy-grid
  (lambda (original-grid)
    ; Main function I use to create a copy of the grid
    (let ((rows (vector-length original-grid)))  ; Getting the number of rows
      ; Calling the helper with the original grid, an empty grid, and the starting row index (0)
      (copy-helper original-grid (make-vector rows) 0))))

(define copy-helper
  (lambda (original-grid new-grid i)
    ; Helper function which will access to each single row and copy it using the next helper function
    (let ((rows (vector-length original-grid)))  ; Getting the length of the grid
      (cond
        ((< i rows)  ; While there are still more rows to copy
          ; Set the relevant raw in the grid to a copy of the origianl row using the copy-row
          (vector-set! new-grid i (copy-row (vector-ref original-grid i)))
          ; Calling the function recursively but incrementing the row count
          (copy-helper original-grid new-grid (+ i 1)))
        ; If we're done, return the new grid
        (else new-grid)))))

(define copy-row
  (lambda (row)
    ; Helper function to copy a complete row into a new one
    (let* ((cols (vector-length row)) (new-row (make-vector cols)))  ; Calculate the length of the row and create an empty one
      ; Call the next helper function with row, new row, index and number of columns
      (copy-element row new-row 0 cols))))

(define copy-element
  (lambda (row new-row j cols)
    ; Helper function to copy a row
    (cond
      ((>= j cols) new-row)  ; If all elements were copied, return the new row
      ; Otherwise update the new row and call the function again with the next index
      (else (vector-set! new-row j (vector-ref row j)) (copy-element row new-row (+ j 1) cols)))))

(load "grid-class.ss")
(load "grid-draw.ss")
(load "grid-make.ss")

; ==============================
; RUNNING THE Real-Time A*
; ==============================
(define grid0 (make-grid num-col-row))
(draw-obstacles grid0)
(define grid (convert-grid grid0))
(load "grid-new.ss")
(load "grid-RTA.ss")
(set-goal grid)
(set-start grid)
; Taking a copy of the grid, start, goal and the robot's starting position so that we can use them again.
(define grid-copy (copy-grid grid))
(define start-copy (list (car start) (cadr start)))
(define goal-copy (list (car goal) (cadr goal)))
(define robot-copy (list (car robot) (cadr robot)))
(draw-start)
(draw-goal)
(draw-robot)
(show canvas)
(search-rta grid 20000)
; (send top destroy)  ; Uncomment this line to close the window after the test-run

; ==============================
; RUNNING THE HILL CLIMBER
; ==============================
(load "grid-class.ss")
(load "grid-draw.ss")
(load "grid-make.ss")
(draw-obstacles grid0)
(define grid grid-copy)
(set! start start-copy)
(set! goal goal-copy)
(set! robot robot-copy)
(load "grid-new.ss")
(load "grid-HC.ss")
(draw-start)
(draw-goal)
(draw-robot)
(show canvas)
(search-hc grid 20000)
; (send top destroy)  ; Uncomment this line to close the window after the test-run

; ==============================
; SAVING THE RESULTS
; ==============================
(display "BOTH SEARCHES ARE DONE")
(newline)
(display "REAL-TIME A* FINAL SCORE ")
(display rta-count)
(newline)
(display "HILL-CLIMBER FINAL SCORE ")
(display hc-count)
(newline)
(set! total-rta (cons rta-count total-rta))
(set! total-hc (cons hc-count total-hc))
