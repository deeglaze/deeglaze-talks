#lang at-exp slideshow
(provide (all-defined-out))
(require slideshow/pict
         slideshow/code
         unstable/gui/pict
         racket/gui/base)

(define-syntax-rule (with24 . body) (parameterize ([current-font-size 22]) . body))
(define (idtt txt) (colorize (tt txt) id-color))
(define (abstt txt) (colorize (tt txt) "red"))
(define (ftt txt) (colorize (tt txt) "midnight blue"))
(define (ctt txt) (colorize (tt txt) "maroon"))
(define (σtt txt) (colorize txt "tomato"))
(define (Mtt txt) (colorize txt "cadet blue"))
(define (Ξtt txt) (colorize txt "firebrick"))
(define (ctxtt txt)  (colorize txt "brown"))
(define (addr t) (colorize t "medium forest green"))
(define chσ (with24 (σtt @tt{σ})))
(define chσ′ (with24 (σtt @tt{σ′})))
(define M (with24 (Mtt @tt{M})))
(define M′ (with24 (Mtt @tt{M′})))
(define Ξ (with24 (Ξtt @tt{Ξ})))
(define Ξ′ (with24 (Ξtt @tt{Ξ′})))
(define ctx (with24 (ctxtt @tt{ctx})))
(define a (with24 (addr (tt "a"))))
