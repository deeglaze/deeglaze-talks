#lang at-exp slideshow

#|
EXTERNAL DEPS:
Packages (removable with racket-poppler? = #f and require poppler-main instead of racket-poppler):
racket-poppler (raco pkg install racket-poppler)
slideshow-helpers (raco pkg install git://github.com/ianj/slideshow-helpers#master)

Libraries (removable with use-pdf? = #f):
libpoppler

Fonts (free on fontsquirrel.com):
Cantarell
Kaushan Script
Inconsolata
Telegrama

Papers (removable dependency with use-pdf? = #f):
Abstracting Control (locally as "abstracting-control.pdf")
 url: http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=AEA0F8AF807727EB1353B012FC8D7E41?doi=10.1.1.43.8753&rep=rep1&type=pdf
;; (already in repo) Abstracting Abstract Machines (locally as "../icfp2013/vanhorn2010abstract.pdf")
Soft Contract Verification (locally as "soft-contract-verification.pdf")
|#

(require (except-in unstable/gui/slideshow stage)
         (for-syntax syntax/parse)
         rsvg
         slideshow-helpers/picts
         slideshow-helpers/slide
         racket-poppler
         ;talk-utils/poppler-main ;; raco link'd to ../utils
         file/convertible
         unstable/gui/ppict
         net/sendurl
         slideshow/code
         slideshow/flash
         slideshow/play
         racket/gui/base
         scheme/runtime-path
         slideshow/balloon slideshow/face)
;; PARAMETERS
(set-page-numbers-visible! #f)
(define use-pdf? #t)
(define-for-syntax racket-poppler? #t)

(define SCREEN-WIDTH 1024)
(define SCREEN-HEIGHT 768)

(define (ct s) (text s "Cantarell" (current-font-size)))
(define (cbt s) (text s (cons 'bold "Cantarell") (current-font-size)))

(define (ic s) (text s "Inconsolata" (current-font-size)))
(define (iic s) (text s (cons 'italic "Inconsolata") (current-font-size)))

(define (kt s) (text s "Kaushan Script" (current-font-size)))
(define-syntax-rule (wkt . forms) (parameterize ([current-main-font "Kaushan Script"]) . forms))
(define (tg s) (text s "Telegrama" (current-font-size)))

(define bg-slide-assembler
  (lambda (title sep content)
    (inset content (- margin) (- margin) 0 0)))

(define-syntax-rule (scode e ...)
  (parameterize ([current-font-size 28])
    (code e ...)))

;; Inconsolata for code
;; Use thicker arrows
;; Use more curvy arrows
;; Inset frames. Borders rectangle/border
;; For AAM vs OAAM, keep them together, group with {
;; Group faster/smaller better. Don't cover with popups

(define-runtime-path grinder-small-path "../icfp2013/grinder_small.png")
(define logo-path (collection-file-path "prl-logo.png" "talk-utils"))
(define-runtime-path redex-path "plt-redex.jpeg")
(define-runtime-path regehr-path "regehr.png")

(define-runtime-path forbidden-path "forbidden.svg")

(define-runtime-path aam-path "../icfp2013/vanhorn2010abstract.pdf")
(define-runtime-path phil-path "soft-contract-verification.pdf")
(define-runtime-path delim-path "abstracting-control.pdf")

(define-runtime-path aam-png "../icfp2013/vanhorn2010abstract.png")
(define-runtime-path phil-png "soft-contract-verification.png")
(define-runtime-path delim-png "abstracting-control.png")

(define-syntax (render-pdf stx)
  (syntax-parse stx
    [(_ file α)
     (if racket-poppler?
         #'(scale (bitmap (page->bitmap (pdf-page (open-pdf file) 0) (* 4 α))) 0.25)
         #'(page->pict file scale))]))
(define-syntax-rule (error-bitmap e) (error 'todo "Render bitmap ~a" 'e))
(define (pdf-scale-or-bitmap pdf-path png-path [scale-factor 1.0])
  (cond [use-pdf?
         (define pict (render-pdf pdf-path scale-factor))
         ;; dump rendering to png for later use.

         (with-output-to-file png-path #:exists 'replace
           (λ () (write-bytes (convert (pict->bitmap pict) 'png-bytes))))
         pict]
        [else (bitmap png-path)]))

(define aam-pict (delay (pdf-scale-or-bitmap aam-path aam-png 0.7)))
(define phil-pict (delay (pdf-scale-or-bitmap phil-path phil-png 0.7)))
(define delim-pict (delay (pdf-scale-or-bitmap delim-path delim-png 0.7)))
(define delim-pict3 (delay (pdf-scale-or-bitmap delim-path delim-png 3)))

(define tyellow (make-object color% #xFF #xFF #x00 0.3))

(define (svg->pict path [α 1.0]) (svg-file->pict path α))

(module+ utils
  (provide bracket-left bracket-right fewer-col faster-col mkei)
  (define fewer-col "medium forest green")
  (define faster-col "slateblue")
  (define ((bracket superimpose) width height brack-height)
    (define brack (filled-rectangle width brack-height))
    (rb-superimpose
     (superimpose brack
                  (filled-rectangle (/ width 2) height))
     brack))
  (define (mkei n)
    (define n* (floor n))
    (if (inexact? n*)
        (inexact->exact n*)
        n*))
  (define bracket-right (bracket rt-superimpose))
  (define bracket-left (bracket lt-superimpose)))

(module+ slide-deck

  (provide title what-do-I-do regehr whiskey
           ;; main function
           run-talk
           )
  (require (submod ".." utils))

  (define (title)
    (parameterize ([current-slide-assembler bg-slide-assembler])
      (slide
       (cc-superimpose
        (bitmap logo-path)
        (vc-append
         (with-size 45 (bold (colorize @ct{Abstracting Abstract Control} '(15 15 74))))
         (blank 40)
         (with-size 24
           (ht-append gap-size
                      (vc-append
                       (colorize @cbt{J. Ian Johnson} "navy")
                       (small (tg "ianj@ccs.neu.edu"))
                       (blank-line)
                       (vc-append @ct{Northeastern University}
                                  @ct{Boston, MA, USA}))
                      (vc-append
                       @ct{David Van Horn}
                       (small (tg "dvanhorn@cs.umd.edu"))
                       (blank-line)
                       (vc-append @ct{University of Maryland}
                                  @ct{College Park, MD, USA})))))))))

  (define/staged what-do-I-do #:stages [sounds-abstract delim aam static not-really abstract]
    #:anim-at [sounds-abstract #:steps 10]
    #:anim-at [delim #:steps 10 #:skip-first] ;; slide from left "abstract control"
    #:anim-at [aam #:steps 10 #:skip-first] ;; slide from right "abstracting abstract machines"
    (define sounds (with-size 72 @kt{“Sounds abstract”}))
    (define (mk-title n)
      (pin-over-hcenter (blank 0) 0 0 sounds
                        #:y-translate (lerp (- (/ (pict-height sounds) 2))
                                            (- (/ SCREEN-HEIGHT 2))
                                            n)))
    
    (define delimp (shadow-frame (force delim-pict)))
    (define aamp (shadow-frame (force aam-pict)))
    (define (mk-delim n)
      (pin-over-vcenter
       (mk-title 1) 0 35 delimp
       #:x-translate (lerp (- (+ (pict-width delimp) (/ SCREEN-WIDTH 2)))
                           (- (pict-width delimp))
                           n)))
    (define (mk-aam n txt?)
      (define analysis (shadow-frame @ic{Static analysis}))
      (pin-over-vcenter
       (mk-delim 1)
       0 35 (cc-superimpose aamp
                       (show (cc-superimpose analysis
                                             (show (svg->pict forbidden-path 8)
                                                   (>= stage not-really))) txt?))
       #:x-translate (lerp (/ SCREEN-WIDTH 2) 5 n)))
    (cond
     [(= stage sounds-abstract)
      (λ (n) (mk-title (fast-start n)))]
     [(= stage delim)
      (λ (n) (mk-delim (fast-start n)))]
     [(= stage aam)
      (λ (n) (mk-aam (fast-start n) #f))]
     [else (cc-superimpose (mk-aam 1 (>= stage static))
                           (show
                            (shadow-frame
                             (hc-append gap-size
                                        (colorize @iic{Really} "firebrick")
                                        @ic{semantics of abstract execution}))
                            (>= stage abstract)))]))

  ;; Point at parts of John Regehr's tweet
  (define/staged regehr #:stages [base any-lang some-lang some-parts]
    (define bash (blank 0))
    (define start (blank 0))
    (define any
      (if (< stage some-parts)
          (show
           (shadow-frame
            (hc-append gap-size
                       (cc-superimpose
                        (show (colorize @it{any} "red") (= stage any-lang))
                        (show (colorize @it{some} "midnight blue") (= stage some-lang)))
                       (hc-append 0
                                  @t{dynamic language}
                                  (show @t{s} (= stage some-lang)))))
           (> stage base))
          (shadow-frame
           (hc-append gap-size
                      (colorize @it{some parts} "red") @t{of}
                      (colorize @it{some} "midnight blue")
                      @t{dynamic languages}))))
    (define p (bitmap regehr-path))
    (define pinned
      (pin-over
       (pin-over
        (pin-over
         p
         248 203
         bash)
        (if (< stage some-parts)
            -35
            -70)
        (+ (pict-height p) 30)
        any)
       200 (+ (pict-height p) 35)
       start))
    (if (< stage any-lang)
        pinned
        (pin-arrow-line 20 pinned start ct-find bash cb-find
                        #:start-angle (/ pi 2)
                        #:end-angle (/ pi 2)
                        #:line-width 4
                        #:color "red")))

  (define/staged whiskey #:stages [old-days 80s 90s 2000s now technical-debt λhard]
    #:title (wkt @titlet{Whiskey features})
    (cc-superimpose
     (table 5
            (list @ct{70s} 
                  (show @ct{80s} (>= stage 80s))
                  (show @ct{90s} (>= stage 90s))
                  (show @ct{2000s} (>= stage 2000s))
                  (show @ct{now} (>= stage now))

                  @kt{Things}
                  (show @ct{λ} (>= stage 80s))
                  (show @ct{Polymorphism} (>= stage 90s))
                  (show @tg{Eval} (>= stage 2000s))
                  (show @tg{Eval} (>= stage now))

                  (blank 0)
                  (blank 0)
                  (blank 0)
                  (show @ct{Reflection} (>= stage 2000s))
                  (show @ct{Reflection} (>= stage now))

                  (blank 0)
                  (blank 0)
                  (blank 0)
                  (show @ct{Prototypes} (>= stage 2000s))
                  (show @ct{Prototypes} (>= stage now))

                  (blank 0)
                  (blank 0)
                  (blank 0)
                  (blank 0)
                  (show @ct{Continuations?} (>= stage now)))
            cc-superimpose cc-superimpose gap-size gap-size)
     (show
      (vc-append (shadow-frame (colorize @ic{We have some technical debt} "midnight blue"))
                 (show (shadow-frame (colorize @ic{λ is still hard} "firebrick")) (>= stage λhard)))
      (>= stage technical-debt))))

  (define (rainbow-rect w h left-col right-col)
    (define (get-rgb c) (values (send c red) (send c green) (send c blue)))
    (define-values (lr lg lb) (get-rgb left-col))
    (define-values (rr rg rb) (get-rgb right-col))
    (define colorless (filled-rectangle 1 h))
    (define boxes
      (for/list ([i (in-range w)])
        (define n (/ i w))
        (colorize colorless (make-object color%
                                         (mkei (lerp lr rr n))
                                         (mkei (lerp lg rg n))
                                         (mkei (lerp lb rb n))))))
    (apply hc-append 0 boxes))

  (define/staged false-dichotomy #:stages [mk-spectrum point-to-static point-to-dynamic
                                                       plug-phil symbolic
                                                       its-all-symbolic which-symbols]   
    #:title (wkt (with-size 60 @t{False dichotomy}))
    #:anim-at [mk-spectrum #:steps 10]
    (define static (with-size 60 @ic{Static}))
    (define dynamic (with-size 60 @ic{Dynamic}))
    (define sw (pict-width static))
    (define sh (pict-height static))
    (define dw (pict-width dynamic))
    (define SW2 (/ SCREEN-WIDTH 2))
    (define starting-width 1)
    (define left-pad 15)
    (define lr-pad 5)
    (define right-pad 50)
    (define (spec n*)
      (define n (fast-end n*))
      (define static-placement (- (+ sw lr-pad)))
      (define static-left (lerp static-placement (+ (- SW2) left-pad) n))
      (define dynamic-left (lerp starting-width (- SW2 dw starting-width right-pad) n))
      (define static-right (+ static-left (- static-placement)))
      (define rectw (max 0 (- dynamic-left static-right)))
      (define to-blue (make-object color% 0 0 (mkei (lerp 0 255 n))))
      (define to-red (make-object color% (mkei (lerp 0 255 n)) 0 0))
      (define spectrum (rainbow-rect rectw sh to-blue to-red))
      (pin-over
       (pin-over spectrum static-placement 0
                 (colorize static to-blue))
       rectw 0 (colorize dynamic to-red)))
    (cond [(= stage mk-spectrum) spec]
          [else
           (define base (spec 1))
           (define no-runtime (with-size 40 @ic{No runtime context}))
           (define all-runtime (with-size 40 @ic{All runtime context}))
           (define-values (sx sy) (cb-find base (list static)))
           (define-values (dx dy) (cb-find base (list dynamic)))
           (define left-arrow
             (pin-arrow-line 15
                             (pin-over-hcenter base 0 (+ sy 50) no-runtime)
                             no-runtime ct-find static cb-find))
           (define right-arrow
             (pin-arrow-line 15
                             (pin-over-hcenter left-arrow (- dx 75) (+ dy 50) all-runtime)
                             all-runtime ct-find dynamic cb-find))
           (define (mk-phil)
             (define phil (shadow-frame (force phil-pict)))
             (pin-over right-arrow 0 (- (/ (pict-height phil) 2)) phil))
           (cond 
            [(= stage point-to-static) left-arrow]
            [(= stage point-to-dynamic) right-arrow]
            [(= stage plug-phil) (mk-phil)]
            [else
             (cc-superimpose
              (mk-phil)
              (show (shadow-frame (colorize @ic{“Symbolic execution”}
                                            "firebrick"))
                    (>= stage symbolic))
              (show (rotate (shadow-frame (hc-append gap-size (colorize @iic{All} "midnight blue")
                                                     @ic{computation is}
                                                     (colorize @ic{symbolic} "firebrick")))
                            (/ pi 6))
                    (>= stage its-all-symbolic))
              (show (rotate (shadow-frame
                             (hc-append gap-size @iic{Which} (colorize @ic{symbols?} "firebrick")))
                            (- (/ pi 6)))
                    (>= stage which-symbols)))])]))

  
  ;; TODO
  ;; We have a recipe for respecting the stack. 


  (define (run-talk [sections '(intro features spectrum)]
                    #:deltas? [deltas? #t]
                    #:main-font [main-font "LModern"])
    (parameterize ([current-main-font main-font])
      (define-syntax-rule (section name body ...) (when (memv 'name sections) body ...))
      (section intro
               (title)
               (run-stages what-do-I-do)
               (run-stages regehr))

      (section features
               (run-stages whiskey)
               ;;(run-stages disclaimer)
               )

      (section spectrum
               (run-stages false-dichotomy)))))

(module+ main
  (require (submod ".." slide-deck))

 (void (run-talk))
 '|That's all, folks!|)
