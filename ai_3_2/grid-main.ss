(define num-col-row 20)
(define pause-num 100000)
(define size (floor (/ 700 num-col-row)))
(define obstacle-density 25)
(load "grid-class.ss")
(load "grid-draw.ss")
(load "grid-make.ss")
(load "grid-queue.ss")

(define grid0 (make-grid num-col-row))
(draw-obstacles grid0)
(define grid (convert-grid grid0))
(load "grid-new.ss")
(load "grid-RTA.ss")
(set-goal grid)
(set-start grid)
(define (copy-row row)
  (let ((cols (vector-length row)))
    (let ((new-row (make-vector cols)))
      (let loop ((j 0))
        (when (< j cols)
          (vector-set! new-row j (vector-ref row j))  ; Copy each element
          (loop (+ j 1)))  ; Recur for the next column
      new-row))))  ; Return the new row

(define (copy-grid original-grid)
  (let ((rows (vector-length original-grid)))
    (let loop ((i 0)
                (new-grid (make-vector rows)))  ; Create the new grid
      (if (< i rows)
          (begin
            (vector-set! new-grid i (copy-row (vector-ref original-grid i)))  ; Copy each row
            (loop (+ i 1) new-grid))  ; Recur for the next row
          new-grid))))  ; Return the new grid

(draw-start)
(draw-goal)
(draw-robot)
(define grid-copy (copy-grid grid))
(show canvas)
(search grid 20000)
(destroy canvas)
(load "grid-class.ss")
(load "grid-make.ss")
(define grid grid-copy)
(draw-start)
(draw-goal)
(draw-robot)
(show canvas)
(display grid)
(search grid 20000)
