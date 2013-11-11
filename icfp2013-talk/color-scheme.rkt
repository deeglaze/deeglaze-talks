#lang at-exp slideshow
(provide (all-defined-out))
(require slideshow/pict
         slideshow/code
         unstable/gui/pict
         racket/gui/base)

(define use-color? (make-parameter #t))
(define (colorize* p c) (if (use-color?) (colorize p c) p))

(define σcol "tomato")
(define ccol "maroon")

(define-syntax-rule (with24 . body) (parameterize ([current-font-size 22]) . body))
(define (idtt txt) (colorize* (tt txt) id-color))
(define (abstt txt) (colorize* (tt txt) "red"))
(define (ftt txt) (colorize* (tt txt) "midnight blue"))
(define (ctt txt) (colorize* (tt txt) ccol))
(define (σtt txt) (colorize* (tt txt) σcol))
(define (Mtt txt) (colorize* (tt txt) "cadet blue"))
(define (Ξtt txt) (colorize* (tt txt) "firebrick"))
(define (ctxtt txt)  (colorize* (tt txt) "brown"))
(define (addr t) (colorize* t "medium forest green"))
(define (constants)
  (with24
   (values @σtt{σ}
           @σtt{σ′}
           @Mtt{M}
           @Mtt{M′}
           @Ξtt{Ξ}
           @Ξtt{Ξ′}
           @ctxtt{ctx}
           (addr (tt "a")))))
