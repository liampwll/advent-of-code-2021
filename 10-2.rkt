#lang racket

(require peg/peg
         math/statistics)

(define-peg root
  (and (name lines (+ line)) (! (any-char)))
  (median < (filter values lines)))

(define-peg line
  (and (name chunks (+ chunk)) "\n")
  (if chunks (last chunks) #f))

(define-peg chunk
  (and (name open open) (name inner-chunks (* chunk)) (? (name close close)))
  (cond
    ((and close (not (valid-pair? open close))) #f)
    ((ormap false? inner-chunks) #f)
    (close 0)
    (else (+ (* 5 (if (null? inner-chunks) 0 (last inner-chunks))) (score open)))))

(define-peg/bake open
  (or "(" "[" "{" "<"))

(define-peg/bake close
  (or ")" "]" "}" ">"))

(define valid-pairs
  '(("(" . ")")
    ("[" . "]")
    ("{" . "}")
    ("<" . ">")))

(define (valid-pair? open close)
  (equal? close (cdr (assoc open valid-pairs))))

(define (score open)
  (case open
    (("(") 1)
    (("[") 2)
    (("{") 3)
    (("<") 4)))

(let ((str (file->string "10.txt")))
  (displayln (peg root str)))
