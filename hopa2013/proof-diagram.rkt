#lang at-exp slideshow
(require unstable/gui/slideshow
         unstable/gui/ppict
         scheme/runtime-path
         slideshow/balloon slideshow/face
         (except-in "pict-helpers.rkt" addr)
         "color-scheme.rkt"
         (submod "semantics.rkt" slide-deck))

(define (concrete pict) (colorize pict "blue"))
(define (abstract pict) (colorize pict "red"))
(module* slide-deck #f
 (provide diagram)
 (define (diagram reveal?)
   
   (define diagram-corners
     (ppict-do (blank 400)
               #:go (coord 0 0 'lc)
               (tag-pict (big (concrete @t{CESK})) 'orig-concrete)
               #:go (coord 1 0 'lc)
               (tag-pict (big (abstract @t{CESK})) 'abstract-with-stack)
               #:go (coord 13/32 3/4 'rc)
               (show (tag-pict (big (concrete @t{CESKMΞ})) 'this-talk) reveal?)
               #:go (coord 25/32 3/4 'lc)
               (tag-pict (big (abstract @t{CFA2/PDCFA})) 'endpoint)))
   
   (define first-arrows
     (pin-arrow-label-line
      #:x-adjust 50
      (rotate (abstract (text "Pain" null 24)) (* -1/2 pi))
      10
      (pin-arrow-label-line
       (text "Bound store" null 22)
       10 diagram-corners
       (find-tag diagram-corners 'orig-concrete) rc-find
       (find-tag diagram-corners 'abstract-with-stack) lc-find)
      (find-tag diagram-corners 'abstract-with-stack)  cb-find
      (find-tag diagram-corners 'endpoint) ct-find))
   
   (slide
    (if reveal?
        (pin-arrow-label-line
         (text "Bound store" null 22)
         10
         (pin-arrow-label-line
          (inset (shadow (text "This Talk" null 30)
                         5 0 2
                         #:color "mediumblue"
                         #:shadow-color "midnightblue")
                 10)
          #:x-adjust -130
          10
          first-arrows
          (find-tag first-arrows 'orig-concrete) cb-find
          (find-tag first-arrows 'this-talk) ct-find)
         (find-tag first-arrows 'this-talk) rc-find
         (find-tag first-arrows 'endpoint) lc-find)
        first-arrows))))
(module* main #f
 (require (submod ".." slide-deck))
 (both diagram))