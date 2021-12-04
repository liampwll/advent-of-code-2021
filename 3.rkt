#lang racket

(define (most-common digits)
  (if (> 0 (foldl + 0 (map (lambda (x) (if (equal? x 1) 1 -1)) digits)))
      0
      1))

(define (least-common digits)
  (if (equal? (most-common digits) 1) 0 1))

(define (bin-list->number digits)
  (foldl (lambda (digit result) (+ (* result 2) digit)) 0 digits))

(define (solve lines)
  (* (bin-list->number (map most-common (apply map list lines)))
     (bin-list->number (map least-common (apply map list lines)))))

(define (solve2 lines)
  (let ((find-num (lambda (pred)
                    (let loop ((lines lines))
                      (if (null? (cdr lines))
                          (car lines)
                          (let ((keep-digit (pred (apply list (map car lines)))))
                            (cons keep-digit (loop (map cdr (filter (lambda (x) (equal? (car x) keep-digit)) lines))))))))))
    (* (bin-list->number (find-num most-common))
       (bin-list->number (find-num least-common)))))

(let ((lines (map (lambda (line) (map (lambda (digit) (if (equal? digit #\1) 1 0)) line))
                  (map string->list
                       (file->lines "3.txt")))))
  (display (solve lines))
  (display "\n")
  (display (solve2 lines))
  (display "\n"))
