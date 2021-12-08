#lang racket

(define numbers
  '(("abcefg" . 0)
    ("cf" . 1)
    ("acdeg" . 2)
    ("acdfg" . 3)
    ("bcdf" . 4)
    ("abdfg" . 5)
    ("abdefg" . 6)
    ("acf" . 7)
    ("abcdefg" . 8)
    ("abcdfg" . 9)))

(define number-lists
  (map (lambda (x) (cons (string->list (car x)) (cdr x)))
       numbers))

(define segment-permutations
  (foldl (lambda (in result) (cons (map cons (string->list "abcdefg") in) result))
         '()
         (permutations (string->list "abcdefg"))))

(define (remap-list lst m)
  (map (lambda (x) (cdr (assoc x m)))
       lst))

(define (find-outputs lines)
  (map (lambda (line)
         (let ((mapping (ormap (lambda (perm)
                                 (and (andmap (lambda (pattern) (assoc (sort (remap-list pattern perm) char<?) number-lists))
                                              (car line))
                                      perm))
                               segment-permutations)))
           (map (lambda (output) (cdr (assoc (sort (remap-list output mapping) char<?) number-lists)))
                (cdr line))))
       lines))

(let* ((raw-lines (file->lines "8.txt"))
       (lines (map (lambda (raw-line)
                     (let* ((split (string-split raw-line " | "))
                            (patterns (map string->list (string-split (car split))))
                            (outputs (map string->list (string-split (cadr split)))))
                       (cons patterns outputs)))
                   raw-lines))
       (real-outputs (find-outputs lines))
       (sum (foldl +
                   0
                   (map (lambda (outputs) (foldl (lambda (digit result) (+ (* result 10) digit))
                                                 0
                                                 outputs))
                        real-outputs))))
  (display (count (lambda (x) (member x '(1 4 7 8)))
                  (flatten real-outputs)))
  (display "\n")
  (display sum)
  (display "\n"))
