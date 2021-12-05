#lang racket

;; This is bad, don't do this.

(require math/array)
(require threading)

(define (every-nth-element n x)
  (let loop ((counter 1)
             (x x))
    (cond
      ((null? x) '())
      ((= counter n) (cons (car x) (loop 1 (cdr x))))
      (else (loop (+ counter 1) (cdr x))))))

(define (pairs x)
  (map list (every-nth-element 2 (cons #f x)) (every-nth-element 2 x)))


(let* ((file-parts (string-split (file->string "4.txt") "\n\n"))
       (draws (list->vector (map string->number (string-split (car file-parts) ","))))
       (bingos (~>
                (map (lambda (lines)
                       (string-split lines "\n"))
                     (cdr file-parts))
                (map (lambda (lines)
                       (map (lambda (line)
                              (map string->number (string-split line)))
                            lines))
                     _)
                (foldl (lambda (board result)
                         (list* board (apply map list board) result))
                       '()
                       _)
                (list*->array number?)
                (array-axis-expand 0
                                   (vector-length draws)
                                   (lambda (element index)
                                     (if (vector-member element (vector-take draws (+ index 1)))
                                         #f
                                         (* element (vector-ref draws index)))))
                (array-axis-reduce 3
                                   (lambda (dk proc)
                                     (foldl (lambda (jk result)
                                              (if (and (proc jk) result)
                                                  (+ (proc jk) result)
                                                  (or (proc jk) result)))
                                            #f
                                            (range 0 dk))))
                (array-axis-reduce 2
                                   (lambda (dk proc)
                                     (let ((res (foldl (lambda (jk result)
                                                         (if (false? (proc jk))
                                                             (cons #t (cdr result))
                                                             (if (cdr result)
                                                                 (cons (car result) (+ (proc jk) (cdr result)))
                                                                 (cons (car result) (proc jk)))))
                                                       '(#f . #f)
                                                       (range 0 dk))))
                                       (if (car res)
                                           (cdr res)
                                           #f))))
                (array-axis-reduce 0
                                   (lambda (dk proc)
                                     (foldl
                                      (lambda (jk result)
                                        (or result (if (proc jk)
                                                       (cons jk (proc jk))
                                                       #f))) #f (range 0 dk))))
                (array->list)
                (pairs)
                (map (lambda (x) (car (sort x (lambda (a b) (< (car a) (car b))))))
                     _)
                (sort (lambda (a b) (< (car a) (car b)))))))
  (display (car bingos))
  (display "\n")
  (display (last bingos))
  (display "\n"))
