
(define create-instance
  (lambda (search-function base-grid)
    (set-goal grid)
    (set-start grid)
    (draw-start)
    (draw-goal)
    (draw-robot)
    (show canvas)
    (search-function grid 20000)
    ))

