#lang debug racket/base

(module+ reader
  (require syntax/strip-context racket/match racket/string)
  (provide (rename-out [rs read-syntax]))
  (define (rs name ip)
    (define lines
      (for*/list ([line (in-lines ip)]
                  [str (in-value (string-trim (string-trim line #px"#.*" #:left? #false)))]
                  #:when (non-empty-string? str))
        (match-define (list maybe-range tag) (string-split str ";"))
        (match-define (list left right)
          (map (λ (str) (string->number str 16))
               (match (string-split maybe-range "..")
                 [(list left right) (list left right)]
                 [(list left) (list left left)])))
        (list left right (string->symbol tag))))
    (strip-context
     (with-syntax ([((LVAL RVAL RES) ...) lines])
       #'(module _ racket/base
           (provide f)
           (define (f x)
             (cond [(<= LVAL x RVAL) 'RES] ...)))))))