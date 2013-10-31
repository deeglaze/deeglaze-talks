#lang at-exp slideshow
(require unstable/gui/slideshow
         unstable/gui/ppict
         net/sendurl
         slideshow/code
         slideshow/flash
         racket/gui
         scheme/runtime-path
         slideshow/balloon slideshow/face
         "pict-helpers.rkt")

(define-runtime-path square-circle-path "square-peg-round-hole.jpg")

(define invisible (blank 0))

(module+ slide-deck
 (provide hammer)
(define (hammer hope?)
  (define base
    (pin-over
     (pin-over
      (pin-over (pin-over (bitmap square-circle-path)
                          210 280
                          (show
                           (tag-pict @t{Hope for extensibility} 'hope)
                           hope?))
                200 0
                (tag-pict (small (colorize @t{Grad student} "darkgray")) 'student))
      200 170 (tag-pict @t{PDS} 'pds))
     -160 40
     (tag-pict @t{Your language} 'lang)))
  (define arrow-points
    (pin-over
     (pin-over
      (pin-over
       (pin-over
        (pin-over
         (pin-over base 70 120 (tag-pict invisible 'lang-arrow))
         158 68 (tag-pict invisible 'student-arrow))
        111 194 (tag-pict invisible 'pds-arrow))
       29 246 (tag-pict invisible 'hope-arrow0))
      100 283 (tag-pict invisible 'hope-arrow1))
     190 232 (tag-pict invisible 'hope-arrow2)))
  (define with-arrows
    (pin-arrow-line
     10
     (pin-arrow-line 5
      (pin-arrow-line 10 arrow-points
                      (find-tag arrow-points 'lang)
                      cb-find
                      (find-tag arrow-points 'lang-arrow)
                      lt-find
                      #:line-width 4)
      (find-tag arrow-points 'student) lb-find
      (find-tag arrow-points 'student-arrow) rc-find
      #:color "gray")
     (find-tag arrow-points 'pds)
     lc-find
     (find-tag arrow-points 'pds-arrow)
     rc-find
     #:line-width 4))
  (define hope-arrows
    (pin-arrow-line
     6
     (pin-arrow-line
      6
      (pin-arrow-line 6
                      with-arrows
                      (find-tag with-arrows 'hope) lc-find
                      (find-tag with-arrows 'hope-arrow0) rc-find
                      #:line-width 3
                      #:style 'long-dash)
      (find-tag with-arrows 'hope) lc-find
      (find-tag with-arrows 'hope-arrow1) rc-find
      #:line-width 3
      #:style 'long-dash)
     (find-tag with-arrows 'hope) lc-find
     (find-tag with-arrows 'hope-arrow2) rc-find
     #:line-width 3
     #:style 'long-dash))
  ;; Transition funnier?
  (slide hope-arrows #;(if hope? hope-arrows with-arrows))))

(module+ main
 (require (submod ".." slide-deck))
;(hammer #f)
 (hammer #t)
)