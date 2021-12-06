#lang racket

(require math/matrix)

(define (counts-up-to n lst)
  (map (lambda (x) (length (filter (lambda (y) (equal? x y)) lst)))
       (inclusive-range 0 n)))

(define (solve ages iterations)
  (let ((transitions (matrix (;0 1 2 3 4 5 6 7 8
                              (0 0 0 0 0 0 1 0 1) ; 0
                              (1 0 0 0 0 0 0 0 0) ; 1
                              (0 1 0 0 0 0 0 0 0) ; 2
                              (0 0 1 0 0 0 0 0 0) ; 3
                              (0 0 0 1 0 0 0 0 0) ; 4
                              (0 0 0 0 1 0 0 0 0) ; 5
                              (0 0 0 0 0 1 0 0 0) ; 6
                              (0 0 0 0 0 0 1 0 0) ; 7
                              (0 0 0 0 0 0 0 1 0) ; 8
                              )))
        (initial-state (list->matrix 9 9 (append (counts-up-to 8 ages) (build-list (* 9 8) (lambda (_) 0))))))
    (foldl + 0 (matrix->list (matrix* initial-state (matrix-expt transitions iterations))))))

(let ((ages (map string->number (string-split (car (file->lines "6.txt")) ","))))
  (display (solve ages 80))
  (display "\n")
  (display (solve ages 256))
  (display "\n"))
