#lang racket

(define (walk-cost n)
  (/ (* n (+ n 1)) 2))

(let ((positions (map string->number (string-split (car (file->lines "7.txt")) ","))))
  (display (argmin values (map (lambda (x) (foldl + 0 (map (lambda (y) (abs (- y x))) positions))) (range 0 (argmax values positions)))))
  (display "\n")
  (display (argmin values (map (lambda (x) (foldl + 0 (map (lambda (y) (walk-cost (abs (- y x)))) positions))) (range 0 (argmax values positions)))))
  (display "\n"))
