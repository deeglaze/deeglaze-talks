#lang at-exp slideshow

(require unstable/gui/slideshow
         rsvg
         slideshow-helpers/picts
         talk-utils/poppler-main ;; package
         file/convertible
         unstable/gui/ppict
         net/sendurl
         slideshow/code
         slideshow/flash
         slideshow/play
         racket/gui/base
         scheme/runtime-path
         slideshow/balloon slideshow/face
         
         (rename-in "pict-helpers.rkt" [addr paddr])
         "color-scheme.rkt"
         (submod "semantics.rkt" slide-deck))
(set-page-numbers-visible! #f)

(define SCREEN-HEIGHT 768)

;; Watch
;; Daniel Spiewak keynote at lambda jam
;; Tell Dave Thomas about optimization coaching.
;; -- little languages vs kitchen sink is poorly thought out - what about interop?
;; Contour model for seeing bindings during execution? (Burroughs)
;; Educating users in languages is important - people don't know how to learn Haskell. Student languages.
;; FP for CRUD only? Kata for FP? Yay plug for HtDP (how to design.. with types?)
;; Tell Dave Thomas about Justin's dependently typed J.

;; PHP Hack
;; Type checker keeps witness for why it thinks a variable has a given type.
;; "mixed" type -- do they have true union types?

;; Should be in sync with texpict/code (used by pict/code)
(define comma @tt{, })
(paren-color "brown")

(define bg-slide-assembler
  (lambda (title sep content)
    (inset content (- margin) (- margin) 0 0)))

(define-syntax-rule (scode e ...)
  (parameterize ([current-font-size 28])
    (code e ...)))

(current-main-font "LModern")
;; Inconsolata for code
;; Use thicker arrows
;; Use more curvy arrows
;; Inset frames. Borders rectangle/border
;; For AAM vs OAAM, keep them together, group with {
;; Group faster/smaller better. Don't cover with popups

(define-runtime-path grinder-path "grinder.png")
(define-runtime-path grinder-small-path "grinder_small.png")
(define-runtime-path grinderp-path "grinder-transparent_small.png")
(define-runtime-path meat-path "super_meat_boy_by_mrcbleck-d5tsgeo_small.png")
(define logo-path (collection-file-path "prl-logo.png" "talk-utils"))
(define-runtime-path redex-path "plt-redex.jpeg")
(define-runtime-path tweak-path "tweak.png")
(define-runtime-path map-path "map.png")
(define-runtime-path fenway-path "fenway-small.png")
(define-runtime-path grinder-tiny-path "grinder-tiny.png")
(define-runtime-path vet-path "vet.png")

(define-runtime-path aam-path "vanhorn2010abstract.pdf")
(define-runtime-path speedup-path "all-relative-time.pdf")
(define-runtime-path fanout-path "fanout.pdf")
(define-runtime-path lazy-path "lazy.pdf")
(define-runtime-path lazyc-path "lazyc.pdf")
(define-runtime-path graph0-path "introspective-base.pdf")
(define-runtime-path graph1-path "introspective-lazy.pdf")
(define-runtime-path graph2-path "introspective-lazyc.pdf")
(define-runtime-path comp-store-path "comp-store.pdf")

(define-runtime-path aam-png "vanhorn2010abstract.png")
(define-runtime-path speedup-png "all-relative-time.png")
(define-runtime-path fanout-png "fanout.png")
(define-runtime-path lazy-png "lazy.png")
(define-runtime-path lazyc-png "lazyc.png")
(define-runtime-path graph0-png "introspective-base.png")
(define-runtime-path graph1-png "introspective-lazy.png")
(define-runtime-path graph2-png "introspective-lazyc.png")
(define-runtime-path comp-store-png "comp-store.png")

(define-runtime-path curly-svg "curly.svg")

(define use-pdf? #t)
(define-syntax-rule (error-bitmap e) (error 'todo "Render bitmap ~a" 'e))
(define (pdf-scale-or-bitmap pdf-path png-path [scale-factor #f])
  (cond [use-pdf?
         (define pict
           (if scale-factor
             (scale (page->pict pdf-path) scale-factor)
             (page->pict pdf-path)))
         ;; dump rendering to png for later use.
         #;
         (with-output-to-file png-path #:exists 'replace
           (λ () (write-bytes (convert (pict->bitmap pict) 'png-bytes))))
         pict]
        [else (bitmap png-path)]))
(define aam-pict (delay (pdf-scale-or-bitmap aam-path aam-png 0.7)))
(define speedup-pict (delay (pdf-scale-or-bitmap speedup-path speedup-png)))
(define fanout-pict (delay (pdf-scale-or-bitmap fanout-path fanout-png 0.6)))
(define lazy-pict (delay (pdf-scale-or-bitmap lazy-path lazy-png 0.6)))
(define lazyc-pict (delay (pdf-scale-or-bitmap lazyc-path lazyc-png 0.6)))
(define graph0-pict (delay (pdf-scale-or-bitmap graph0-path graph0-png 0.15)))
(define graph1-pict (delay (pdf-scale-or-bitmap graph1-path graph1-png 0.15)))
(define graph2-pict (delay (pdf-scale-or-bitmap graph2-path graph2-png 0.15)))
(define comp-store-pict (delay (pdf-scale-or-bitmap comp-store-path comp-store-png 0.4642)))

(define (title)
 (parameterize ([current-slide-assembler bg-slide-assembler])
   (slide
    (cc-superimpose
     (bitmap logo-path)
     (vc-append
      (with-size 45 (bold
                      (vc-append gap-size (colorize @it{Optimizing} '(30 30 110))
                                 (colorize @t{Abstract Abstract Machines} '(15 15 74)))))
      (blank 40)
      (with-size 24
       (ht-append gap-size
        (vc-append
              (hc-append (colorize @bt{J. Ian Johnson} "navy") @t{,})
              (t "Nicholas Labich, David Van Horn")
              (small (tt "{ianj,labichn,dvanhorn}@ccs.neu.edu"))
              (blank-line)
              (vc-append @t{Northeastern University}
                         @t{Boston, MA, USA}))
        (vc-append
         @t{Matt Might}
         (ghost (t "David Van Horn"))
         (small (tt "matt@might.net"))
         (blank-line)
         (vc-append @t{University of Utah}
                    @t{Salt Lake City, UT, USA})))))))))

(define (greent txt) (colorize (t txt) grn))
(define (oranget txt) (colorize (t txt) "orange"))
(define (light-gray txt) (colorize (t txt) (list 200 200 200)))
(define tyellow (make-object color% #xFF #xFF #x00 0.3))

(define (intro stage)
 (slide (cc-superimpose
         (with-size 60 (para #:align 'center (colorize @it{Abstract} '(15 15 74)) @t{abstract machines?}))
         (show (shadow-frame (force aam-pict)) (>= stage 1))
         (show (shadow-frame (with-size 60 (with-font "DejaVu Serif" (colorize @t{x 1000} "midnightblue"))))
               (= stage 2)))))

(define (grinder stage)
  (λ (n)
     (define in-anchor (blank 0))
     (define out-anchor (blank 0))
     (define analysis (let ([p (inset (with-size 48 @t{Analysis}) 3)])
                        (pict-if (= stage 0)
                                 p
                                 (cc-superimpose (thick-filled-rounded-rectangle
                                                  (pict-width p)
                                                  (pict-height p)
                                                  0
                                                  #:border-color "black"
                                                  #:color "slategray")
                                                 (colorize p "white")))))
     (define program (show @t{Program} (= stage 1)))
     (define report (show @t{Report} (= stage 1)))
     (define amidx (/ (pict-width analysis) 2))
     (define anim-y (* n 500))
     (define analysis-box
       (pin-over-hcenter
        (pin-over-hcenter
         analysis amidx (+ (* 3 (pict-height report)) anim-y) report)
        amidx (* -2.5 (pict-height program)) program))
     (define analysis-arrows
       (if (= stage 1)
           (pin-arrow-line
            15
            (pin-arrow-line 15 analysis-box analysis cb-find report ct-find
                            #:line-width 3
                            #:under? #t)
            #:line-width 3
            program cb-find analysis ct-find)
           analysis-box))
     (define semantics (with-size 48 @t{Semantics}))
     (define anchored-grinder
       (pin-over
        (pin-over
         (pin-over-vcenter
          (pin-over-hcenter (bitmap grinder-path)
                            166 (* -2 (pict-height semantics)) semantics)
          400 127 analysis-arrows)
         355 127 out-anchor)
        166 11 in-anchor))
     (define overall
       (pin-arrow-line 15
                     (pin-arrow-line 15 anchored-grinder semantics cb-find
                                     in-anchor ct-find
                                     #:line-width 3)
                     out-anchor rc-find
                     analysis lc-find
                     #:line-width 3))
     (pin-over (ghost overall) -200 0 overall)))

(define (grinder-grinder meat?)
  (define in-anchor (blank 0))
  (define out-anchor (blank 0))
  (define out2-anchor (blank 0))
  (define elbow (blank 0))
  (define semantics (with-size 48 @t{Semantics}))
  (define grinder2 (bitmap grinderp-path))
  (define meat-boy (bitmap meat-path))
  (define in1x 135)
  (define in2x 117)
  (define out1y 105)
  (define out2x 262)
  (define grind2y 150)
  (define anchored-grinder
    (panorama
     (pin-over
      (pin-over
       (pin-over
        (pin-over-hcenter (vc-append (bitmap grinder-small-path) @t{AAM})
                          in1x (* -2 (pict-height semantics))
                          semantics)
        282 out1y out-anchor)
       in1x 5 in-anchor)
      300 grind2y (pin-over (vc-append grinder2 @t{OAAM}) 262 94 out2-anchor))))
  (define elbow-find
    (lambda (p pth)
      (define-values (x y) (ct-find p pth))
      (values (- x (/ (pict-width grinder2) 2) (- in2x)) (- y (- grind2y out1y)))))
  (define base
    (pin-over-vcenter
     (pin-arrow-line
      15
      (pin-line
       (pin-arrow-line 15 anchored-grinder semantics cb-find
                       in-anchor ct-find
                       #:line-width 3)
       out-anchor rc-find
       grinder2 elbow-find
       #:line-width 3)
      grinder2 elbow-find
      grinder2 (lambda (p pth) (define-values (x y) (lt-find p pth)) (values (+ x in2x) y)) #:line-width 3)
     out2-anchor (lambda (p pth) (define-values (x y) (lt-find p pth)) (values (+ x 50) (- y 25)))
     (show meat-boy meat?)))
  
  (panorama
   (pin-arrow-line
    15
    base
    out2-anchor rc-find
    meat-boy
    (lambda (p pth) (define-values (x y) (lc-find p pth)) (values x (+ y 25)))
    #:line-width 3 #:alpha (if meat? 1.0 0.0))))

(define ((bracket superimpose) width height brack-height)
  (define brack (filled-rectangle width brack-height))
  (rb-superimpose
   (superimpose brack
                (filled-rectangle (/ width 2) height))
   brack))
(define bracket-right (bracket rt-superimpose))
(define bracket-left (bracket lt-superimpose))

(define (analysis-qualities stage)
  (define-values (intro show-qualities aam oaam) (apply values (range 4)))
  (define-values (sound prec perf maint)
    (with-size 40
     (values @t{Soundness}
             (if (= stage aam) (colorize @it{Precision} "dimgray") @t{Precision})
             @t{Performance}
             @t{Maintainability})))

  (define aamcite (with-size 24 (colorize-if (>= stage oaam)
                                             (vc-append @t{[Van Horn and Might ICFP 2010]} @t{(AAM)})
                                             "gray")))
  (define oaamcite (with-size 24 (colorize (vc-append @t{[Johnson, et al. ICFP 2013]} @t{(OAAM)}) "navy")))
  (define base
    (vc-append gap-size
               (show sound (> stage intro))
               (show maint (> stage intro))
               (show prec (> stage intro))
               (if (= stage aam) (colorize perf "gray") perf)))
  (define cites
    (pin-over
      (pin-over base (* -0.8 (pict-width aamcite)) (- (pict-height aamcite)) (show aamcite (>= stage aam)))
      (- (pict-width base) 25) (+ (pict-height base) (pict-height oaamcite))
      (show oaamcite (>= stage oaam))))
  (define-values (x y) (lc-find cites prec))
  (slide #:title "An ideal static analysis has"
         (ht-append
          (show (colorize-if (>= stage oaam) (bracket-left 20 y 10) "gray") (>= stage aam))
          cites
          (show (bracket-right 20 (pict-height cites) 10)
                (>= stage oaam)))))

(define fewer-col "medium forest green")
(define faster-col "slateblue")
(define (talk-focus stage)
  (define-values (anim dec fast focus goal revisit) (apply values (range 6)))
  (define count (box 0))
  (define (numbers)
    (for/vector #:length 3 ([i (in-range 3)])
      (t (format "~a." (add1 i)))))
  (define (item-space last . s)
    (keyword-apply item '(#:bullet) (list (apply
                                           cc-superimpose
                                           (append (cons (ghost (scale bullet 2))
                                                         (for/list ([i (in-vector (numbers))]) (ghost i)))
                                                   (list last))))
                   s))
  (define (bullet-item . s) (apply item-space (scale bullet 2) s))
  (define in-talk (if (= stage anim)
                      (λ (n . s) (bullet-item s))
                      (λ (n . s) (apply item-space (vector-ref (numbers) n) s))))
  (define in-paper (if (>= stage focus)
                       (λ (p) (colorize (bullet-item p) "gray"))
                       bullet-item))
  (define (if-paper paper-only? p)
    (if (and (number? paper-only?) (> stage fast))
        (in-talk paper-only? p)
        (in-paper p)))
  (define (decrease paper-only? p)
    (colorize-if
     (or (and (>= stage dec) (number? paper-only?) (< stage revisit))
         (= stage dec)
         (= stage fast))
     (if-paper paper-only? p) fewer-col))
  (define (faster paper-only? p)
    (colorize-if
     (or (and (>= stage fast) (number? paper-only?) (< stage revisit))
         (= stage fast))
     (if-paper paper-only? p)
     faster-col))
  (with-size 44
    (define-values (i0 i1 i2 i3 i4 i5 i6)
      (values (decrease 'paper @t{Store-allocate values})
              (decrease 'paper @t{Frontier-based semantics})
              (decrease 0 @t{Lazy non-determinism})
              (decrease 1 @t{Abstract compilation})
              (faster 2 @t{Locally log-based store-deltas})
              (faster 'paper @t{Store-counting})
              (faster 'paper @t{Mutable frontier and store})))
    (define is (vector i0 i1 i2 i3 i4 i5 i6))
    (define off-screen (blank 0))
    (define placement*
      (parameterize ([current-gap-size (* 2 gap-size)])
        (apply vl-append (for/list ([i (in-vector is)]) (ghost i)))))
    (define bars (let-values ([(x0 y0) (lt-find placement* i0)]
                              [(x1 y1) (lb-find placement* i3)])
                   (define base
                     (ht-append
                      (show (colorize
                             (hc-append gap-size
                                        (rotate (with-size 28 @t{Decrease state space}) (/ pi 2))
                                        (bracket-left 20 (add1 (- y1 y0)) 10))
                             fewer-col)
                            (and (>= stage dec) (< stage goal)))
                      placement*))
                   (define-values (x2 y2) (rt-find base i4))
                   (define-values (x3 y3) (rb-find base i6))
                   (pin-over base x2 y2
                             (show (colorize
                                    (hc-append gap-size
                                     (bracket-right 20 (add1 (- y3 y2)) 10)
                                     (rotate (with-size 28 @t{Explore faster}) (/ pi -2))) faster-col)
                                   (and (>= stage fast) (< stage goal))))))
    (define-values (x y) (lt-find bars i0))
    (define placement (pin-over bars x SCREEN-HEIGHT off-screen))
    (define slider (slide-and-compose placement is off-screen))
    (λ (n) (cc-superimpose (slider n)
                           (show (with-size 48 (shadow-frame @t{Goal: Directly implementable math}))
                                 (= stage goal))))))
(define focus-title (with-size 50 @t{OAAM outline}))

(define bench-overview
  (delay
   (define AAM-point (blank 0))
   (define OAAM-point (blank 0))
   (define AAM-text (with-size 48 @t{AAM}))
   (define OAAM-text (with-size 48 @t{OAAM}))
   (define-values (AAM-x AAM-y) (values 40 245))
   (define-values (OAAM-x0 OAAM-y0) (values 742 0))
   (define-values (OAAM-x1 OAAM-y1) (values 750 175))
   (define speedup
     (pin-over
      (pin-over
       (pin-over
        (pin-over
         (pin-over
          (force speedup-pict) OAAM-x0 OAAM-y0 (ellipse 15 OAAM-y1))
         OAAM-x1 OAAM-y1 OAAM-point)
        AAM-x AAM-y AAM-point)
       (- AAM-x (pict-width AAM-text)) (+ AAM-y (* 2 (pict-height AAM-text))) AAM-text)
      (- OAAM-x1 (pict-width OAAM-text)) (+ AAM-y (* 2 (pict-height AAM-text))) OAAM-text))
   (vc-append
    (pin-arrow-line 15
                    (pin-arrow-line 15 speedup AAM-text ct-find AAM-point cb-find)
                    OAAM-text ct-find OAAM-point cb-find
                    #:end-angle (/ pi 2))
    (blank 180)
    (colorize @t{No change in evaluated precision} "slategray"))))

(define (drama stage)
  (define hipster "Aliqua put a bird on it pour-over DIY cupidatat Truffaut American Apparel paleo et sed Pariatur leggings photo booth beard Craft beer sed brunch eiusmod readymade Anim put a bird on it odio Cosby sweater Cupidatat raw denim placeat selvage bicycle rights gentrify Brooklyn dolor roof party Excepteur laborum fingerstache dreamcatcher artisan")
  (define words (string-split hipster))
  (define junk (for/vector ([w (in-list words)])
                 (typeset-code (datum->syntax #f (string->symbol w)))))
  (define (random-junk . meh) (vector-ref junk (random (vector-length junk))))
  ;; can't use #,@ in code, so hack in indentation properties.
  (define prog
    #`(match e
        [(var x) {(continue v σ κ) : v ∈ (get σ (get ρ x))}]
        [(app e₀ e₁) {(eval e₀ ρ σ (cons (ar e₁ ρ) κ))}]
        [(lam x e*) {(continue (closure e ρ) σ κ)}]
        #,@(if (= stage 0)
               #'()
               (let*-values
                   ([(indent) 8]
                    [(cases position)
                     ;; 160 is how many characters the above form is without the last )
                     (for/fold ([acc '()] [position (+ 160 indent)]) ([i 100])
                       (define pattern
                         #`(#,@(build-list (add1 (random 10)) random-junk)))
                       (define rhs
                         #`{#,@(build-list (add1 (random 30)) random-junk)})
                       ;; 3 for [ ] and space between lhs, rhs
                       (define span (+ 3 (syntax-span pattern) (syntax-span rhs)))
                       (values
                        (cons
                         (syntax-property
                          (datum->syntax
                           #f
                           (list pattern rhs)
                           (list 'code
                                 (+ i 4)
                                 indent
                                 position
                                 span))
                          'paren-shape #\[)
                         acc)
                        ;; +1 since column 0 is a thing.
                        (+ position span (add1 indent))))])
                 cases))))
  (typeset-code prog))

(define fake-match (with-size 3 (drama 1)))
(define (matches show?)
  (slide (cc-superimpose
          fake-match
          (show (shadow-frame (with-size 42 @t{Compile away dispatch overhead})) show?))))

(define aam->oaam
  (let ()
    (define-values (graph slazy scomp sdelt hlazy hcomp hdelt) (apply values (range 7)))
    (define (hilight b t text-col)
      (define t* (colorize-if b t "white"))
      (pict-if b
               (filled-rounded-rectangle-frame t* #:color "slategray" #:scale 1.02)
               (colorize t* text-col)))
   (define (pin-many-ellipses-over base xyxys show?)
     (for/fold ([p base]) ([xyxy (in-list xyxys)])
       (match-define (list x0 y0 x1 y1) xyxy)
       (define ew (add1 (- x1 x0)))
       (define eh (add1 (- y1 y0)))
       (pin-over p x0 y0 (show
                          (annulus ew eh 3 #:color "medium forest green" #:border-color "black") show?))))
   (define-values (lazy comp delt)
     (with-size 60
       (values (item #:bullet @t{1.} @t{Lazy non-determinism})
               (item #:bullet @t{2.} @t{Abstract compilation})
               (item #:bullet @t{3.} @t{Store deltas}))))
   (λ (stage)
      (define (surround-stuff base)
        (define-values (x y) (rt-find base (find-tag base 'lazy)))
        (define text
          (para (show (hilight (= stage hlazy) lazy fewer-col) (>= stage slazy))
                (show (hilight (= stage hcomp) comp fewer-col) (>= stage scomp))
                (show (hilight (= stage hdelt) delt faster-col) (>= stage sdelt))))
        (define y* (+ y (/ (pict-height base) 2) (/ (pict-height text) -2)))
        (panorama
         (pin-over
          (pin-many-ellipses-over
           base
           '((38 195 70 213)
             (38 292 66 310)
             (68 292 104 310)
             (83 389 122 404)
             (37 421 63 438)
             (38 443 67 458)
             (82 476 122 491)
             (38 553 69 567))
           (or (= stage slazy) (= stage hlazy)))
          (- x (* 2.5 2 gap-size) 40) y* text)))
      (λ (n)
         (cond [(= stage sdelt)
                (define starting (graph-show 0))
                (define template (surround-stuff (ghost starting)))
                (define-values (x y) (lt-find template starting))
                (define nf (fast-start n))
                (define dy
                  (if (>= nf 1/2)
                      ;; halfway through, jump to top of the screen and slide back to original position.
                      ;; dy ∈ [-height, y], so (2nf - 1)*y + (1 - (2nf - 1))*-height
                      (+ (* y (sub1 (* nf 2)))
                         (* (pict-height starting) (- (* 2 nf) 2)))
                      ;; First half, the picture shoots off the screen, dy ∈ [y, SCREEN-HEIGHT]
                      (+ (* SCREEN-HEIGHT 2 nf) (* y (- 1 (* 2 nf))))))
                (pin-over template x dy starting)]
               [else
                (surround-stuff (graph-show 0))])))))

(define aam-title (with-size 42 @t{Finite reduction relation = graph}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ((proof-discussion stage) n)
  (define base
    (vc-append gap-size
               @t{The proof is interesting (a simple use of WEBs).}
               @t{It is a complete abstraction,}
               @t{but not if the store is global.}
               (show
                @t{Roughly, stores don't line up} (> stage 0))
               (show (pin-over
                      (pin-over (blank 300)
                                (* n 125) 0 (rectangle 100 200))
                      (- 300 (* n 175)) 0 (force comp-store-pict))
                     (> stage 0))))
  (cond
   [(> stage 0)
    (define origin (blank 0))
    (define y-axis (blank 0))
    (define x-axis (blank 0))
    (define bottom-distance 200)
    (define origin-x 0)
    (define origin-y (- SCREEN-HEIGHT bottom-distance))
    (define y-length 120)
    (define x-length 150)
    (define x-axis-x (+ origin-x x-length))
    (define y-axis-y (- origin-y y-length))
    ;; Draw a legend for addresses × abstraction
    (define anchors
      (pin-over
       (pin-over
        (pin-over base origin-x origin-y origin)
        origin-x y-axis-y y-axis)
       x-axis-x origin-y x-axis))
    (define ytext (colorize (rotate (with-size 24 @t{Addresses}) (/ pi 2)) "slateblue"))
    (define xtext (colorize (with-size 24 @t{Abstraction}) "slateblue"))
    (pin-over
     (pin-over
      (pin-arrow-line
       10
       (pin-arrow-line 10 anchors origin cc-find x-axis cc-find
                       #:line-width 3 #:color "slategray")
       origin cc-find y-axis cc-find
       #:line-width 3 #:color "slategray")
      (- origin-x (pict-width ytext) 5)
      ;; halfway between origin and y-axis
      (+ y-axis-y (/ y-length 2) (* -1/2 (pict-height ytext)))
      ytext)
     (+ origin-x (/ x-length 2) (* -1/2 (pict-width xtext)))
     (+ origin-y 5)
     xtext)]
   [else base]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Store deltas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (dinky)
  (code (code:line (define (apply-discount x lst)
                     (map (λ (y) (- y x)) lst)))))
(define (formi i) (string->symbol (format "form~a" i)))
(define (a-program)
  (parameterize ([port-count-lines-enabled #t])
   (with-input-from-file "proctime.rkt"
     (λ ()
        (read-line)
        (define all-forms-rev
          (for/fold ([acc '()])
              ([form (in-port read-syntax)]
               [i (in-naturals)])
            (define stuff
              (list 'code
                    (syntax-line form)
                    (syntax-column form)
                    (syntax-position form)
                    (syntax-span form)))
            (cons (datum->syntax #f
                                 (list #'code:line form)
                                 stuff) acc)))
        (define len (length all-forms-rev))
        (values
         (apply vl-append
          (for/fold ([acc '()]) ([form (in-list all-forms-rev)]
                                 [i (in-naturals)])
            (define (itag p) (tag-pict (typeset-code p) (formi i)))
            (cond
             [(= i (quotient len 2))
              (list* (itag form) (tag-pict (dinky) 'helper) acc)]
             [else (list* (itag form) acc)])))
         len)))))

(define context
  (let ()
    (define-values (scene recall #;point sstore schange shighlight scascade) (apply values (range 6)))
    (define-values (p* len) (with-size 3 (a-program)))
    (define p
      (pin-under-all p* 'helper
                     (filled-flash-frame
                      (ghost (with-size 3 (dinky)))
                      #:color (make-object color% #xFF #xFF #x00 0.8))))
    (λ (stage)
       (define (bump base path [size 10])
         (define-values (x y) (lt-find base path))
         (define bw (pict-width base))
         (define h (pict-height (car path)))
         (pin-under base 0 y
                    (pin-over
                     (frame (colorize (filled-rectangle (+ size bw) h) (if (= stage shighlight)
                                                                           "red"
                                                                           "white")))
                     1 0
                     (colorize (filled-rectangle (- bw 2) (add1 h)) "white"))))
       (cond [(= stage scene) p]
             [(= stage recall)
              (define ballooned (wrap-balloon (dinky) 'n 5 -120 "white" 32))
              (pin-balloon ballooned p (find-tag p 'helper) cb-find)]
#;
             [(= stage point) ;; point at helper
              (define helper-path (find-tag p 'helper))
              (define-values (x y) (rt-find p helper-path))
              (define impact @t{Small impact on σ})
              (pin-arrow-line
               15
               (pin-over p (+ x (/ (pict-width impact) 2))
                         (- y (* 2 (pict-height impact)))
                         impact)
               impact lb-find (car helper-path) rt-find)]
             [(= stage sstore) ;; show "store"
              (pin-under p 0 0 (rectangle (pict-width p) (pict-height p)))]
             [(member stage (list schange shighlight)) ;; show small change to store
              (pin-over
               (pin-under (bump p (find-tag p 'helper))
                          0 0 (rectangle (pict-width p) (pict-height p)))
               500 600 (show (with-size 18 (colorize @t{Keep pushing} "red")) (= stage shighlight)))]
             [(= stage scascade) ;; show cascaded changes
              (for/fold ([p* p]) ([i (in-range (- len 2))])
                (pin-under (bump p* (find-tag p (formi i)) (random 150))
                           0 0 (rectangle (pict-width p) (pict-height p))))]))))

(define (what-is-CESK stage)
  (define-values (C E S K transform) (apply values (range 5)))
  (define-syntax-rule (ctx body ...) (with-size 72 (parameterize ([use-color? #t]) body ...)))
  (ctx
   (define Cp @idtt{e})
     (define Ep @idtt{ρ})
     (define Sp @σtt{σ})
     (define Kp @ctt{κ})
     (define size 30)
     (define Cpoint (show (vc-append @t{Control} (arrow size (/ pi -2))) (= stage C)))
     (define Epoint (show (vc-append (arrow size (/ pi 2)) @t{Environment}) (= stage E)))
     (define Spoint (show (vc-append @t{Store} (arrow size (/ pi -2))) (= stage S)))
     (define (Kpoint n)
       (show (vc-append (arrow size (/ pi 2))
                        (hc-append 2
                                   (cc-superimpose
                                    (ghost @t{C})
                                    (ghost @t{K})
                                    (if (<= n 1/2)
                                        (fade-pict (chop-at 0 1 (* 2 n)) @t{K} @t{C})
                                        (fade-pict (chop-at 0 1 (sub1 (* 2 n))) @t{C} @t{K})))
                                   @t{ontinuation}))
             (= stage K)))
     (define ς (ntuple Cp Ep Sp Kp))
     (define (state n)
       (pin-over-hcenter
        (pin-over-hcenter
         (pin-over-hcenter
          (pin-over-hcenter ς Cp ct-find Cpoint #:y-translate (- (pict-height Cpoint)))
          Ep cb-find Epoint)
         Sp ct-find Spoint #:y-translate (- (pict-height Spoint)))
        Kp cb-find (Kpoint n)))
     (define (with-plt p)
       (pin-under p 450 200 (scale (bitmap redex-path) 0.2)))
     (cond
      [(= stage transform)
       (λ (n)
          (ctx
           (fade-pict n (state 0)
                      (call (compile @idtt{e})
                            (apply hc-append (add-between (list Ep Sp Kp) comma))))))]
      [(= stage K) (λ (n) (ctx (with-plt (state n))))]
      [else (with-plt (state 0))])))

(define (fanout stage)
  (vc-append gap-size
             (code (f x y))
             (force fanout-pict)
             (show (vc-append (arrow 20 (/ pi -2))
                              (force lazy-pict))
                   (>= stage 1))
             (show (vc-append (arrow 20 (/ pi -2))
                              (force lazyc-pict))
                   (>= stage 2))))

(define (graph-show stage)
  (hc-append (* 2.5 gap-size)
             (force graph0-pict)
             (show (arrow 20 0) (>= stage 1))
             (show (tag-pict (force graph1-pict) 'lazy) (>= stage 1))
             (show (arrow 20 0) (>= stage 2))
             (show (tag-pict (force graph2-pict) 'abs-comp) (>= stage 2))))

(define (run-talk [sections '(intro slow motivation recap
                              outline lazy abscomp abscomp-proof deltas
                              evaluation wrapup)])
  (define-syntax-rule (section name body ...) (when (memv 'name sections) body ...))
  (section intro
   (title)

   ;; What is AAM?
   (for ([i 3]) (intro i))

   ;; What does AAM promise?
   (slide ((grinder 0) 0)))

  ;; Where does this promise break down?
  (section slow
           (play (grinder 1) #:steps 100)
           (slide ((grinder 1) 1)))

  (section motivation
   ;; Three key points
   (define (key-points overlay?)
     (slide #:title (with-size 50 @t{Goal: Directly implementable math})
      (cc-superimpose
       (vc-append gap-size
                  (with-size 60
                    (shadow-frame (colorize
                                   (vc-append (* 2 gap-size)
                                              (hc-append @t{Competitive performance,}
                                                         (colorize (superscript @t{*})
                                                                   "firebrick"))
                                              @t{minimal effort,}
                                              @t{simple proofs})
                                   "medium forest green")))
                  (blank 50)
                  (para #:align 'right (colorize @t{same ballpark, nosebleed seats} "firebrick")))
       (show (shadow-frame (with-size 60 (vl-append
                                          gap-size
                                          (colorize @t{Slogan:} "firebrick")
                                          (vc-append gap-size
                                                     @t{Engineering tricks}
                                                     @t{as semantics refactorings}))))
             overlay?))))
   (both key-points)
   ;; What do we get out of OAAM?
   (for ([i 4]) (analysis-qualities i))
   ;; What's in the paper, and how is it partitioned?
   (play (talk-focus 0) #:steps 60 #:title focus-title)
   (for ([i (in-range 1 4)]) (slide #:title focus-title ((talk-focus i) 1))))

  (section recap
    (slide (with-size 60 @t{First: recap of AAM})))

  (parameterize ([pushdown? #f]
                 [use-color? #f])
    (section recap
     (for ([i 3]) (slide (what-is-CESK i) #:title "Start with CESK machine"))
     (play (what-is-CESK 3) #:title "Start with CESK machine" #:steps 25 #:skip-first? #t)
     (slide ((what-is-CESK 3) 1) #:title "Start with CESK machine")
     (define psCESK (CESK-table sCESK))
     (define psCESK* (CESK-table sCESK*))
     (define psaCESK (CESK-table saCESK))
     (play (λ (n) (fade-pict n psCESK psCESK*)) #:steps 40)
     (play (λ (n) (fade-pict n psCESK* psaCESK)) #:steps 40)
     (slide psaCESK))

    ;; AAM produces graphs
    (section outline
     (define (state-to-edge stage)
       (let ()
        (with-size 72
          (define left (ntuple @idtt{x} @idtt{ρ} @σtt{σ} @ctt{κ}))
          (define right (ntuple @idtt{v} @idtt{ρ′} @σtt{σ} @ctt{κ}))
          (define rc (colorize (disk (max (pict-width left) (pict-height left))) "red"))
          (define gc (colorize (disk (max (pict-width right) (pict-height right))) "green"))
          (λ (n)
             (define cleft (cc-superimpose (if (= stage 1) rc (fade-pict n (ghost rc) rc))
                                           (if (= stage 0) left (fade-pict n left (ghost left)))))
             (define cright (cc-superimpose (if (= stage 1) gc (fade-pict n (ghost gc) gc))
                                            (if (= stage 0) right (fade-pict n right (ghost right)))))
             (define p (with-size 72
                         (hc-append (* 2 gap-size) cleft (if (= stage 0)
                                                             (fade-pict n @t{ ↦ } (ghost @t{ ↦ }))
                                                             (ghost @t{ ↦ }))
                                    cright)))
             (pin-arrow-line 30 p cleft rc-find cright lc-find #:line-width 3 #:alpha (if (= stage 0) n 1))))))
     (play (state-to-edge 0))
     (play (state-to-edge 1))
     (slide ((state-to-edge 1) 1))
     (for ([i 3]) (slide ((aam->oaam i) 0) #:title aam-title))
     (play (aam->oaam 3) #:title aam-title #:skip-first? #t)
     (slide ((aam->oaam 3) 1) #:title aam-title))

    (section lazy
      (slide ((aam->oaam 4) 0) #:title (ghost aam-title)) ;; Highlight lazy
      (slide (CESK-table lazy)) ;; The problem
      (slide (CESK-table lazy-fix)) ;; The solution (hand-wavy)

      ;; Visualize the problem
      (for ([i 2]) (slide (fanout i)))
      ;; Example program state space
      (slide (graph-show 1))
      )

    (section abscomp

     (slide ((aam->oaam 5) 0) #:title (ghost aam-title)) ;; highlight comp
     (slide (CESK-table abscomp)) ;; The problem

     (slide (with-size 26 (drama 0))) ;; Dispatch hell
     (both matches) ;; Dramatic dispatch

     (define (abscomp-template stage)
       (define step
         (with-size 72
          (parameterize ([use-color? #t])
            (vc-append gap-size
                       (hc-append (ntuple @idtt{e} @idtt{ρ} @σtt{σ} @ctt{κ}) @t{ ↦ } @t{RHS})
                       (arrow 30 (/ pi -2))
                       (hc-append (compile @idtt{e}) @t{ = λ} @idtt{ρ} comma @σtt{σ} comma @ctt{κ} @t{. RHS})))))
       (if (= stage 0)
           (λ (n) (vc-append 40 ((what-is-CESK 4) n) (ghost step)))
           (vc-append 40 ((what-is-CESK 4) 1) (show step (= stage 2)))))

     (play (abscomp-template 0))
     (slide (abscomp-template 1))
     (slide (abscomp-template 2))

     (for ([i (in-range 1 3)]) (slide (fanout i)))
     (slide (graph-show 2)))
#;
    (section abscomp-proof
     (slide ((proof-discussion 0) 0))
     (play (proof-discussion 1) #:steps 15)
     (slide ((proof-discussion 1) 1)))
    )

 (section deltas
     (slide ((aam->oaam 6) 0) #:title (ghost aam-title)) ;; highlight deltas

     (define-syntax-rule (with-sub p s) (hb-append 0 p (with-size (quotient (current-font-size) 2) s)))

     (define (wide-expl stage)
       (define-values (orig hilight global deltas final) (apply values (range 5)))
       (define store-per-state
         (let ()
          (define (zoom-state show-non-store?)
            (define (sns p) (show p show-non-store?))
            (hc-append
             (vc-append
              (* 2 gap-size)
              (vl-append
               (* 2 gap-size)
               (hc-append (sns (tt "{"))
                           (hc-append (sns (pcolor (t "〈")))
                                      (sns (with-sub @idtt{e} @idtt{0}))
                                      (sns comma)
                                      (sns (with-sub @idtt{ρ} @idtt{0}))
                                      (sns comma)
                                      (tag-pict (with-sub @σtt{σ} @σtt{0}) 'store-start)
                                      (sns comma)
                                      (sns (with-sub @ctt{κ} @ctt{0}))
                                      (sns (pcolor (t "〉"))))
                           (sns comma))
               (hc-append (blank 50) (rotate @t{...} (/ pi 2)))
               (hc-append (ghost (tt "{"))
                           (hc-append (sns (pcolor (t "〈")))
                                      (sns (with-sub @idtt{e} @idtt{n}))
                                      (sns comma)
                                      (sns (with-sub @idtt{ρ} @idtt{n}))
                                      (sns comma)
                                      (tag-pict (with-sub @σtt{σ} @σtt{n}) 'store-end)
                                      (sns comma)
                                      (sns (with-sub @ctt{κ} @ctt{n}))
                                      (sns (pcolor (t "〉"))))
                           (sns (tt "}"))))
              (show (with-sub @σtt{σ} @σtt{global}) (>= stage global)))

             @tt{ ↦ }

             (vc-append (* 2 gap-size)
              (vl-append (* 2 gap-size)
                         (hc-append (sns (tt "{"))
                                    (hc-append (sns (pcolor (t "〈")))
                                               (sns (with-sub @idtt{e′} @idtt{0})) (sns comma)
                                               (sns (with-sub @idtt{ρ′} @idtt{0})) (sns comma)
                                               (tag-pict (with-sub @σtt{σ′} @σtt{0}) 'nstore-start) (sns comma)
                                               (sns (with-sub @ctt{κ′} @ctt{0}))
                                               (sns (pcolor (t "〉"))))
                                    (sns comma))
                         (hc-append (blank 50) (rotate @t{...} (/ pi 2)))
                         (hc-append (ghost (tt "{"))
                                    (hc-append (sns (pcolor (t "〈")))
                                               (sns (with-sub @idtt{e′} @idtt{m})) (sns comma)
                                               (sns (with-sub @idtt{ρ′} @idtt{m})) (sns comma)
                                               (tag-pict (with-sub @σtt{σ′} @σtt{m}) 'nstore-end) (sns comma)
                                               (sns (with-sub @ctt{κ′} @ctt{m}))
                                               (sns (pcolor (t "〉"))))
                                    (sns (tt "}"))))
              (show (if (= stage final)
                        (hc-append @it{replay} (t " changes"))
                        (t "All small changes")) (>= stage deltas)))))
          (define left (zoom-state #t))
          (define-values (x0 y0) (lt-find left (find-tag left 'store-start)))
          (define-values (x1 y1) (rb-find left (find-tag left 'store-end)))
          (define-values (x2 y2) (lt-find left (find-tag left 'nstore-start)))
          (define-values (x3 y3) (rb-find left (find-tag left 'nstore-end)))
          (lt-superimpose
           (pin-over
            (pin-over left (- x0 34) (- y0 30)
                      (show (shadow-frame (cc-superimpose
                                           (blank (add1 (abs (- x1 x0))) (add1 (abs (- y1 y0))))
                                           (with-size 42 (t "⨆"))))
                            (>= stage hilight)))
            (- x2 34) (- y2 30)
            (show (shadow-frame (cc-superimpose
                                 (blank (add1 (abs (- x3 x2))) (add1 (abs (- y3 y2))))
                                 (with-size 42 (t "⨆"))))
                  (>= stage hilight)))
           (show (zoom-state #f) (>= stage hilight)))))
       
       (define widened
         (let ([start (hc-append (tt "(") @σtt{σ} comma (tt "{"))]
               [end (hc-append (tt "(") @σtt{σ′} comma (tt "{"))])
          (hc-append
           (vl-append (* 2 gap-size)
                      (hc-append start (ntuple (with-sub @idtt{e} @idtt{0})
                                               (with-sub @idtt{ρ} @idtt{0})
                                               (with-sub @ctt{κ} @ctt{0})) comma)
                      (hc-append (ghost start) (blank 50) (rotate @t{...} (/ pi 2)))
                      (hc-append (ghost start) (ntuple (with-sub @idtt{e} @idtt{n})
                                         (with-sub @idtt{ρ} @idtt{n})
                                         (with-sub @ctt{κ} @ctt{n}))
                                 (tt "})")))
           @tt{ ↦ }
           (vl-append (* 2 gap-size)
                      (hc-append end (ntuple @idtt{e′₀} @idtt{ρ′₀} @ctt{κ′₀}) comma)
                      (hc-append (ghost end) (blank 50) (rotate @t{...} (/ pi 2)))
                      (hc-append (ghost end) (ntuple (with-sub @idtt{e′} @idtt{m})
                                            (with-sub @idtt{ρ′} @idtt{m})
                                            (with-sub @ctt{κ′} @ctt{m})) (tt "})"))))))

       (slide #:title "Store widening / Global store"
              (hc-append @t{↦ implemented as } @tt{step : State → ℘(State)})
              @t{Lift and iterate to fixed point in ℘(State) → ℘(State)}
              store-per-state
              #;
              (pict-cond [(or (= stage sps) (= stage hilight))
                          store-per-state]
                         [(= stage final) widened]
                         [else (blank 0)])))
     (for ([i 5]) (wide-expl i))

#;
    (slide (dinky))
#;
    (for ([i 6]) (slide (context i)))
#;
    (slide #:title "The semantic trick"
           (vc-append (hc-append @σtt{σ′} @tt{ = } @σtt{σ} @tt{ ⊔ [a ↦ v]})
                      (arrow 25 (/ pi -2))
                      @tt{σΔ′ = (a, v):σΔ})
           'next
           (blank 100)
           (vc-append (para #:align 'center (hc-append 0 @σtt{σ} (subscript @t{next}))
                            @t{ = ⨆ of all }
                            (hc-append 0 @σtt{σ′} @t{s}))
                      (arrow 25 (/ pi -2))
                      (para #:align 'center (hc-append 0 @σtt{σ} (subscript @t{next}))
                            @tt{ = }
                            @it{replay} @tt{ σΔ } @t{on} @σtt{σ}))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Evaluation section
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (section evaluation
           (slide (with-size 72 @t{What does all this buy us?}))
           (slide (with-size 72 (colorize @t{100000% improvement} "firebrick")))
           (slide #:title "Factor speed-up over naive vs. paper section"
                  (force bench-overview))

           ;; Tweak and OAAM
           (slide (pin-over
                   (pin-over
                    (pin-over (blank 1024 768)
                              -20 -20 (cc-superimpose (filled-rectangle 1024 768) (bitmap fenway-path)))
                    380 390
                    (vc-append (bitmap tweak-path)
                               (colorize (filled-rounded-rectangle-frame @t{Hand-optimized}) "firebrick")))
                   160 125
                   (vc-append (colorize (filled-rounded-rectangle-frame (with-size 40 @t{OAAM})) "darkgreen")
                              (filled-rounded-rectangle-frame (bitmap grinder-tiny-path)))))

           ;; AAM not in same ballpark
           (define (aam-ballpark stadium?)
             (slide
              (pin-over
               (pin-over
                (blank 1024 768) -20 -20
                (cc-superimpose (filled-rectangle 1024 768) (bitmap map-path))) 90 560
                (show (vc-append (filled-rounded-rectangle-frame @t{AAM}) (bitmap vet-path)) stadium?))))
           
           (both aam-ballpark)

           #;#;
           (define pointer (hc-append 10
                                      (with-size 20 @t{Hand-optimized})
                                      (arrow 15 0)
                                      (filled-ellipse 4 4)))
           (pin-over graph
                            (- (pict-width graph) (pict-width pointer))
                            -45
                            pointer))


  (section wrapup
           (define (evolution b) (slide #:title "The evolution of AAM to OAAM" (grinder-grinder b)))
           (both evolution)
   
   (slide #:title (with-size 50 @t{Before we part})
          (with-size 42
            (hc-append @t{Simple, systematic implementation}
                       (bitmap grinder-tiny-path)))
          'next
          (blank 30)
          (with-size 42 (hc-append (scale (bitmap meat-path) 0.3)
                                   (colorize @t{1000x speedup} "midnight blue")))
          'next
          (blank 30)
          (with-size 42
            (vc-append gap-size
                       (colorize @t{"A simple matter of engineering?"} "firebrick")
                       (colorize @t{Build your tricks into the semantics} "medium forest green")))
          'next
          (blank 70)
          (with-size 72 (colorize @t{Thank you} "dark slate blue")))))

(module+ main
 (define dummy (run-talk #;
                (append
                 
                 '(intro                ;; Good
                   slow                 ;; Good
                   motivation)          ;; Okay
                 
                 '(recap
                   outline
                   lazy
                   )
                 
                 '(abscomp
                   deltas
                   evaluation
                   wrapup))))
  '|That's all, folks!|
)