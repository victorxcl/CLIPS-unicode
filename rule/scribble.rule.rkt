#lang at-exp racket
(require racket/match)

(provide hpp cpp mm
         scribble-output; set-scribble-output!
         )

(define *SCRIBBLE_OUTPUT* "")

(define (scribble-output)
  (if (getenv "SCRIBBLE_OUTPUT")
      (getenv "SCRIBBLE_OUTPUT")
      *SCRIBBLE_OUTPUT*))
      
(define (set-scribble-output! kind)
  (case (getenv "SCRIBBLE_OUTPUT")
    [(#f) (set! *SCRIBBLE_OUTPUT* kind)]))
  
(define (hpp text)
  (case (scribble-output)
    [("HPP") @list[text]]
    [else ""]))

(define (cpp text)
  (case (scribble-output)
    [("CPP") @list[text]]
    [else ""]))
    
(define (mm text)
  (case (scribble-output)
    [("MM") @list[text]]
    [else ""]))
    
