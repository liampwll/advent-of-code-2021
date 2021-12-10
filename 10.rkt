#lang racket

(require peg/peg)

(define-peg root
  (and (name lines (+ line)) (! (any-char)))
  (foldl (lambda (x res) (+ res (or x 0))) 0 lines))

(define-peg line
  (and (name chunks (+ chunk)) "\n")
  (ormap values chunks))

(define-peg chunk
  (or complete-chunk
      incomplete-chunk))

(define-peg complete-chunk
  (and (name open open) (name inner-chunks (* complete-chunk)) (name close close))
  (or (ormap values (or inner-chunks '())) (score open close)))

(define-peg incomplete-chunk
  (and open (name inner-chunks (* (or complete-chunk incomplete-chunk))) (! close))
  (ormap values (or inner-chunks '())))

(define-peg/bake open
  (or "(" "[" "{" "<"))

(define-peg/bake close
  (or ")" "]" "}" ">"))

(define valid-pairs
  '(("(" . ")")
    ("[" . "]")
    ("{" . "}")
    ("<" . ">")))

(define (score open close)
  (if (equal? close (cdr (assoc open valid-pairs)))
      #f
      (case close
        ((")") 3)
        (("]") 57)
        (("}") 1197)
        ((">") 25137))))

(let ((str (file->string "10.txt")))
  (displayln (peg root str)))
