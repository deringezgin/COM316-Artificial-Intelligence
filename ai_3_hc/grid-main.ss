; Derin Gezgin
; COM316: Artificial Intelligence | Fall 2024
; Programming Assignment #3
; Due September 24 2024
; File that has the complete code for the main file
; This file runs a test with the Real-Time A* and the Hill Climber on the same grid and saves the result in a global variable

(define copy-element
  (lambda (row new-row j cols)
    (cond
      ((>= j cols) new-row)
      (else (vector-set! new-row j (vector-ref row j)) (copy-element row new-row (+ j 1) cols)))))

(define copy-row
  (lambda (row)
    (let ((cols (vector-length row)))
      (let ((new-row (make-vector cols)))
        (copy-element row new-row 0 cols)))))

(define copy-helper
  (lambda (original-grid new-grid i)
    (let ((rows (vector-length original-grid)))
      (if (< i rows)
          (begin
            (vector-set! new-grid i (copy-row (vector-ref original-grid i)))
            (copy-helper original-grid new-grid (+ i 1)))
          new-grid))))

(define copy-grid
  (lambda (original-grid)
    (let ((rows (vector-length original-grid)))
      (copy-helper original-grid (make-vector rows) 0))))

(define num-col-row 100)
(define pause-num 10000)
(define size (floor (/ 700 num-col-row)))
(define obstacle-density 30)
(load "grid-class.ss")
(load "grid-draw.ss")
(load "grid-make.ss")

(define grid0 (make-grid num-col-row))

(draw-obstacles grid0)
(define grid (convert-grid grid0))
(load "grid-new.ss")
(load "grid-RTA.ss")
(set-goal grid)
(set-start grid)
(define grid-copy (copy-grid grid))
(define start-copy (list (car start) (cadr start)))
(define goal-copy (list (car goal) (cadr goal)))
(define robot-copy (list (car robot) (cadr robot)))
(draw-start)
(draw-goal)
(draw-robot)
(display "START COPY :")
(display start-copy)
(newline)
(show canvas)
(search-rta grid 20000)
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
; =============================
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
