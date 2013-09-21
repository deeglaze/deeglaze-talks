#lang at-exp slideshow
(provide (all-defined-out))
(require slideshow/pict
         slideshow/code
         unstable/gui/pict
         racket/gui/base)

(define use-color? (make-parameter #t))
(define (colorize* p c) (if (use-color?) (colorize p c) p))

(define-syntax-rule (with24 . body) (parameterize ([current-font-size 22]) . body))
(define (idtt txt) (colorize* (tt txt) id-color))
(define (abstt txt) (colorize* (tt txt) "red"))
(define (ftt txt) (colorize* (tt txt) "midnight blue"))
(define (ctt txt) (colorize* (tt txt) "maroon"))
(define (σtt txt) (colorize* txt "tomato"))
(define (Mtt txt) (colorize* txt "cadet blue"))
(define (Ξtt txt) (colorize* txt "firebrick"))
(define (ctxtt txt)  (colorize* txt "brown"))
(define (addr t) (colorize* t "medium forest green"))
(define (constants)
  (with24
   (values (σtt @tt{σ})
           (σtt @tt{σ′})
           (Mtt @tt{M})
           (Mtt @tt{M′})
           (Ξtt @tt{Ξ})
           (Ξtt @tt{Ξ′})
           (ctxtt @tt{ctx})
           (addr (tt "a")))))
