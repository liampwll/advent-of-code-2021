#lang racket

(require graph)

(define next-label!
  (let* ((label-counter 0)
         (proc (lambda ()
                 (set! label-counter (+ 1 label-counter))
                 label-counter)))
    proc))

(define (solve1 points)
  (let ((graph (undirected-graph (append (append-map list->pairs points)
                                         (append-map list->pairs (apply map list points))))))
    (foldl (lambda (x result) (+ result (cdr x) 1))
           0
           (filter (lambda (x) (andmap (lambda (y) (< (cdr x) (cdr y)))
                                       (get-neighbors graph x)))
                   (get-vertices graph)))))

(define (solve2 points)
  (let* ((graph (undirected-graph (filter (lambda (x) (not (or (equal? (cdar x) 9)
                                                               (equal? (cdadr x) 9))))
                                          (append (append-map list->pairs points)
                                                  (append-map list->pairs (apply map list points))))))
         (components (cc graph)))
    (foldl *
           1
           (take (sort (map length components) >) 3))))

(define (list->pairs lst)
  (if (or (null? lst) (null? (cdr lst)))
      '()
      (cons (list (car lst) (cadr lst))
            (list->pairs (cdr lst)))))

(let ((points (map (lambda (line)
                     (map (lambda (char) (cons (next-label!)
                                               (- (char->integer char) (char->integer #\0))))
                          line))
                   (map string->list (file->lines "9.txt")))))
  (displayln (solve1 points))
  (displayln (solve2 points)))
