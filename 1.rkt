#lang racket

(define (solve lines offset)
  (for/fold ((result 0))
            ((line lines)
             (offset-line (drop lines offset)))
    (if (> offset-line line)
        (+ result 1)
        result)))

(let ((lines (map string->number (file->lines "1.txt"))))
  (display (solve lines 1))
  (display "\n")
  (display (solve lines 3))
  (display "\n"))
