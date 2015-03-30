#lang at-exp slideshow

(require (except-in unstable/gui/slideshow stage)
         rsvg
         slideshow-helpers/picts
         slideshow-helpers/slide
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
(define js-path (collection-file-path "logo_JavaScript.png" "talk-utils"))
(define dalvik-path (collection-file-path "dalvik-logo.jpg" "talk-utils"))
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

(provide fanout-pict lazy-pict speedup-pict aam-pict)

(define tyellow (make-object color% #xFF #xFF #x00 0.3))

(module+ utils
  (provide bracket-left bracket-right fewer-col faster-col)
  (define fewer-col "medium forest green")
  (define faster-col "slateblue")
  (define ((bracket superimpose) width height brack-height)
    (define brack (filled-rectangle width brack-height))
    (rb-superimpose
     (superimpose brack
                  (filled-rectangle (/ width 2) height))
     brack))
  (define bracket-right (bracket rt-superimpose))
  (define bracket-left (bracket lt-superimpose)))

(module+ slide-deck
  (provide title intro grinder fanout grinder-grinder what-is-CESK abscomp-template
           analysis-qualities talk-focus key-points state-to-edge
           ;; untranslated
           aam->oaam matches drama bench-overview
           graph-show same-ballpark
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

  (define/staged intro #:stages [base aam speedup]
    (cc-superimpose
     (with-size 60 (para #:align 'center (colorize @it{Abstract} '(15 15 74)) @t{abstract machines?}))
     (show (shadow-frame (force aam-pict)) (>= stage aam))
     (show (shadow-frame (with-size 60 (with-font "DejaVu Serif" (colorize @t{x 1000} "midnightblue"))))
           (= stage speedup))))

  (define/staged (grinder precision?) #:stages [static big-semantics output faster-output]
    #:anim-at [output #:steps 100]
    #:anim-at [faster-output #:steps 5 #:skip-first]
    (define (position-box n)
      (define in-anchor (blank 0))
      (define out-anchor (blank 0))
      (define analysis
        (let ([p (inset (with-size 48 @t{Analysis}) 3)])
          (pict-if (< stage output)
                   p
                   (cc-superimpose (thick-filled-rounded-rectangle
                                    (pict-width p)
                                    (pict-height p)
                                    0
                                    #:border-color "black"
                                    #:color "slategray")
                                   (colorize p "white")))))
      (define program (show @t{Program} (>= stage output)))
      (define report (show (if (or (> stage output) (= n 1))
                               @t{Maybe}
                               @t{Report}) (>= stage output)))
      (define good-report (show @t{Yes} (and (= stage faster-output) (= n 1))))
      (define (slow-y n) (+ (* 3 (pict-height report)) (* n 300)))
      (define fast-y (+ (pict-height analysis) (* n 75)))
      (define fast-offset 40)
      (define amidx (/ (pict-width analysis) 2))
      (define analysis-box
        (pin-over-hcenter
         (pin-over-hcenter
          (pin-over-hcenter
           analysis amidx (if (= stage output) (slow-y n) (slow-y 1)) report)
          (+ amidx fast-offset) fast-y good-report)
         amidx (* -2.5 (pict-height program)) program))
      (define analysis-arrows
        (cond
         [(>= stage output)
          (define base
            (pin-arrow-line
             15
             (pin-arrow-line 15 analysis-box analysis cb-find report ct-find
                             #:line-width 3
                             #:under? #t)
             #:line-width 3
             program cb-find analysis ct-find))
          (if (= stage faster-output)
              (pin-arrow-line 15 base analysis (λ (p path)
                                                  (define-values (x y) (cb-find p path))
                                                  (values (+ x fast-offset) y))
                              good-report ct-find #:line-width 3 #:under? #t)
              base)]
         [else analysis-box]))
      (define semantics (with-size 48 @t{Semantics}))
      (define eg (hc-append 10
                            (show (scale (bitmap js-path) 0.1) (= stage big-semantics))
                            semantics
                            (show (scale (bitmap dalvik-path) 0.4) (= stage big-semantics))))
      (define anchored-grinder
        (pin-over
         (pin-over
          (pin-over-vcenter
           (pin-over-hcenter (bitmap grinder-path)
                             183 (* -1.3 (pict-height eg)) eg)
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
      (pin-over (ghost overall) -200 0 overall))
    (if (or (= stage output) (= stage faster-output))
        position-box
        (position-box 0)))

  (define/staged fanout #:stages [base lazy lazyc]
    (vc-append gap-size
               (code (f x y))
               (force fanout-pict)
               (show (vc-append (arrow 20 (/ pi -2))
                                (force lazy-pict))
                     (>= stage lazy))
               (show (vc-append (arrow 20 (/ pi -2))
                                (force lazyc-pict))
                     (>= stage lazyc))))

  (define/staged grinder-grinder #:stages [base meat] #:title "The evolution of AAM to OAAM"
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
       (show meat-boy (= stage meat))))
  
    (panorama
     (pin-arrow-line
      15
      base
      out2-anchor rc-find
      meat-boy
      (lambda (p pth) (define-values (x y) (lc-find p pth)) (values x (+ y 25)))
      #:line-width 3 #:alpha (if (= stage meat) 1.0 0.0))))

  (define-syntax-rule (ctx body ...) (with-size 72 (parameterize ([use-color? #t]) body ...)))
  (define (CESK) (ctx (values @idtt{e} @idtt{ρ} @σtt{σ} @ctt{κ})))
  (define/staged what-is-CESK #:stages [C E S K]
    #:title "Start with CESK machine"
    #:anim-at [K #:steps 25 #:skip-first]    
    (ctx
     (define-values (Cp Ep Sp Kp) (CESK))
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
      [(= stage K) (λ (n) (ctx (with-plt (state n))))]
      [else (with-plt (state 0))])))

  (define/staged abscomp-template #:stages [do-fade show-step] #:anim-at [do-fade]
    (define step
      (ctx
       (vc-append gap-size
                  (hc-append (ntuple @idtt{e} @idtt{ρ} @σtt{σ} @ctt{κ}) @t{ ↦ } @t{RHS})
                  (arrow 30 (/ pi -2))
                  (hc-append (compile @idtt{e}) @t{ = λ} @idtt{ρ} comma @σtt{σ} comma @ctt{κ} @t{. RHS}))))
    (define-values (Cp Ep Sp Kp) (CESK))
    (define RHS (ctx (call (compile @idtt{e})
                           (apply hc-append (add-between (list Ep Sp Kp) comma)))))
    (define (fader n)
      (ctx
       (vc-append 40 (fade-pict n (ntuple Cp Ep Sp Kp) RHS) (show step (= stage show-step)))))
    (if (= stage do-fade)
        fader
        (fader 1)))

  (define icfp-focus
    (hash 'store-alloc 'paper
          'frontier 'paper
          'lazy 0
          'abscomp 1
          'deltas 2
          'σcounting 'paper
          'mutable 'paper))
  (define defense-focus
    (hash 'store-alloc 'paper
          'frontier 'paper
          'lazy 0
          'abscomp 'paper
          'deltas 'paper
          'σcounting 'paper
          'mutable 'paper))
  (define/staged (talk-focus defense?) #:stages [anim dec fast focus #;goal #;revisit]
    #:title (with-size 50 @t{OAAM outline})
    #:anim-at [anim #:steps 60]
    (define goal 4) ;; killed stage
    (define count (box 0))
    (define (numbers)
      (if defense?
          (vector (t "*"))
          (for/vector #:length 3 ([i (in-range 3)])
                      (t (format "~a." (add1 i))))))
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
       (or (and (>= stage dec) (number? paper-only?)
                (< stage (add1 goal) #;revisit
                   ))
           (= stage dec)
           (= stage fast))
       (if-paper paper-only? p)
       fewer-col))
    (define (faster paper-only? p)
      (colorize-if
       (or (and (>= stage fast) (number? paper-only?) (< stage (add1 goal) #;revisit
                                                         ))
           (= stage fast))
       (if-paper paper-only? p)
       faster-col))
    (define f (if defense? defense-focus icfp-focus))
    (with-size 44
      (define-values (i0 i1 i2 i3 i4 i5 i6)
        (values (decrease (hash-ref f 'store-alloc) @t{Store-allocate values})
                (decrease (hash-ref f 'frontier) @t{Frontier-based semantics})
                (decrease (hash-ref f 'lazy) @t{Lazy non-determinism})
                (decrease (hash-ref f 'abscomp) @t{Abstract compilation})
                (faster (hash-ref f 'deltas) @t{Locally log-based store-deltas})
                (faster (hash-ref f 'σcounting) @t{Store-counting})
                (faster (hash-ref f 'mutable) @t{Mutable frontier and store})))
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
      (define (final n)
        (cc-superimpose (slider n)
                        (show (with-size 48 (shadow-frame @t{Goal: Directly implementable math}))
                              (= stage goal))))
      (if (= stage anim)
          final
          (final 1))))

  (define/staged analysis-qualities #:stages [intro show-qualities aam oaam]
    #:title "An ideal static analysis has"
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
    (ht-append
     (show (colorize-if (>= stage oaam) (bracket-left 20 y 10) "gray") (>= stage aam))
     cites
     (show (bracket-right 20 (pict-height cites) 10)
           (>= stage oaam))))

  ;; Three key points
  (define/staged key-points #:stages [base slogan]
    #:title (with-size 50 @t{Goal: Directly implementable math})
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
           (= stage slogan))))

  (define/staged state-to-edge #:stages [fade-circ fade-solid]
    #:anim-at [fade-circ #:skip-last]
    #:anim-at [fade-solid]
    (with-size 72
      (define left (ntuple @idtt{x} @idtt{ρ} @σtt{σ} @ctt{κ}))
      (define right (ntuple @idtt{v} @idtt{ρ′} @σtt{σ} @ctt{κ}))
      (define rc (colorize (disk (max (pict-width left) (pict-height left))) "red"))
      (define gc (colorize (disk (max (pict-width right) (pict-height right))) "green"))
      (λ (n)
         (define (do-circ circ top)
           (cc-superimpose (if (= stage fade-solid) circ (fade-pict n (ghost circ) circ))
                           (if (= stage fade-circ) top (fade-pict n top (ghost top)))))
         (define cleft (do-circ rc left))
         (define cright (do-circ gc right))
         (define p (with-size 72
                     (hc-append (* 2 gap-size) cleft
                                (if (= stage fade-circ)
                                    (fade-pict n @t{ ↦ } (ghost @t{ ↦ }))
                                    (ghost @t{ ↦ }))
                                cright)))
         (pin-arrow-line 30 p cleft rc-find cright lc-find #:line-width 3
                         #:alpha (cond [(= stage fade-circ) n]
                                       [else 1])))))

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
  
  (define frr (with-size 42 @t{Finite reduction relation = graph}))
  (define/staged (aam->oaam deltas?) #:stages [[graph #:title frr]
                                               [slazy #:title frr]
                                               [scomp #:title frr]
                                               [sdelt #:title frr]
                                               [hlazy #:title (ghost frr)]
                                               [hcomp #:title (ghost frr)]
                                               [hdelt #:title (ghost frr)]]
    #:anim-at [sdelt #:skip-first]
    #:group intro (append (list graph slazy scomp) (if deltas? (list sdelt) '()))
    (define-values (lazy comp delt)
      (with-size 60
        (values (item #:bullet @t{1.} @t{Lazy non-determinism})
                (item #:bullet @t{2.} @t{Abstract compilation})
                (show (item #:bullet @t{3.} @t{Store deltas}) deltas?))))
    (define (surround-stuff base)
      (define-values (x y) (rt-find base (find-tag base 'lazy)))
      (define text
        (para (show (hilight (= stage hlazy) lazy fewer-col) (>= stage slazy))
              (show (hilight (= stage hcomp) comp (if deltas?
                                                      fewer-col
                                                      ;; proposal talk: hilight that abscomp removes red nodes
                                                      "firebrick")) (>= stage scomp))
              (show (hilight (= stage hdelt) delt faster-col) (and (>= stage sdelt) deltas?))))
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
    (cond [(= stage sdelt)                
           (define starting (graph-show 0))
           (define template (surround-stuff (ghost starting)))
           (define-values (x y) (lt-find template starting))
           (λ (n)
              (define nf (fast-start n))
              (define dy
                (if (>= nf 1/2)
                    ;; halfway through, jump to top of the screen and slide back to original position.
                    ;; dy ∈ [-height, y], so (2nf - 1)*y + (1 - (2nf - 1))*-height
                    (+ (* y (sub1 (* nf 2)))
                       (* (pict-height starting) (- (* 2 nf) 2)))
                    ;; First half, the picture shoots off the screen, dy ∈ [y, SCREEN-HEIGHT]
                    (+ (* SCREEN-HEIGHT 2 nf) (* y (- 1 (* 2 nf))))))
              (pin-over template x dy starting))]
          [else
           (surround-stuff (graph-show 0))]))
  ;; untranslated to new slide library

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

  (define (same-ballpark)
    (slide (pin-over
            (pin-over
             (pin-over (blank 1024 768)
                       -20 -20 (cc-superimpose (filled-rectangle 1024 768) (bitmap fenway-path)))
             380 390
             (vc-append (bitmap tweak-path)
                        (colorize (filled-rounded-rectangle-frame @t{Hand-optimized}) "firebrick")))
            160 125
            (vc-append (colorize (filled-rounded-rectangle-frame (with-size 40 @t{OAAM})) "darkgreen")
                       (filled-rounded-rectangle-frame (bitmap grinder-tiny-path))))))

  (define fake-match (with-size 3 (drama 1)))
  (define (matches show?)
    (slide (cc-superimpose
            fake-match
            (show (shadow-frame (with-size 42 @t{Compile away dispatch overhead})) show?))))

  (define (graph-show stage)
    (hc-append (* 2.5 gap-size)
               (force graph0-pict)
               (show (arrow 20 0) (>= stage 1))
               (show (tag-pict (force graph1-pict) 'lazy) (>= stage 1))
               (show (arrow 20 0) (>= stage 2))
               (show (tag-pict (force graph2-pict) 'abs-comp) (>= stage 2))))

  (define (run-talk [sections '(intro slow motivation recap
                                      outline lazy abscomp deltas
                                      evaluation wrapup)]
                    #:deltas? [deltas? #t]
                    #:main-font [main-font "LModern"])
    (parameterize ([current-main-font main-font])
      (define-syntax-rule (section name body ...) (when (memv 'name sections) body ...))
      (section intro
               (title)

               ;; What is AAM?
               (run-stages intro)

               ;; What does AAM promise?
               (run-stages (grinder #f) #:stage 0))

      ;; Where does this promise break down?
      (section slow
               (run-stages (grinder #f) #:stage 2))

      (section motivation
  
               (run-stages key-points)
               ;; What do we get out of OAAM?
               (run-stages analysis-qualities)
               ;; What's in the paper, and how is it partitioned?
               (run-stages (talk-focus #f)))

      (section recap
               (slide (with-size 60 @t{First: recap of AAM})))

      (parameterize ([pushdown? #f]
                     [use-color? #f])
        (section recap
                 (run-stages what-is-CESK)
                 (define psCESK (CESK-table sCESK))
                 (define psCESK* (CESK-table sCESK*))
                 (define psaCESK (CESK-table saCESK))
                 (play (λ (n) (fade-pict n psCESK psCESK*)) #:steps 40)
                 (play (λ (n) (fade-pict n psCESK* psaCESK)) #:steps 40)
                 (slide psaCESK))

        ;; AAM produces graphs
        (section outline
                
                 (run-stages state-to-edge)
                 (run-stages (aam->oaam deltas?) #:group 'intro))

        (section lazy
                 (run-stages (aam->oaam deltas?) #:stage 'hlazy) ;; Highlight lazy
                 (slide (CESK-table lazy))    ;; The problem
                 (slide (CESK-table lazy-fix)) ;; The solution (hand-wavy)

                 ;; Visualize the problem
                 (run-stages fanout #:stage '(0 1))
                 ;; Example program state space
                 (slide (graph-show 1))
                 )

        (section abscomp

                 (run-stages (aam->oaam deltas?) #:stage 'hcomp) ;; highlight comp
                 (slide (CESK-table abscomp)) ;; The problem

                 (slide (with-size 26 (drama 0))) ;; Dispatch hell
                 (both matches)                   ;; Dramatic dispatch

                 (run-stages abscomp-template)

                 (run-stages fanout #:stage '(1 2))
                 (slide (graph-show 2))))

      (section deltas
               (run-stages (aam->oaam deltas?) #:stage 'hdelt) ;; highlight deltas

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
                        store-per-state))
               (for ([i 5]) (wide-expl i))

               )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Evaluation section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (section evaluation
               (slide (with-size 72 @t{What does all this buy us?}))
               (slide (with-size 72 (colorize @t{100000% improvement} "firebrick")))
               (slide #:title "Factor speed-up over naive vs. paper section"
                      (force bench-overview))

               ;; Tweak and OAAM
               (same-ballpark)

               ;; AAM not in same ballpark
               (define (aam-ballpark stadium?)
                 (slide
                  (pin-over
                   (pin-over
                    (blank 1024 768) -20 -20
                    (cc-superimpose (filled-rectangle 1024 768) (bitmap map-path))) 90 560
                   (show (vc-append (filled-rounded-rectangle-frame @t{AAM}) (bitmap vet-path)) stadium?))))
           
               (both aam-ballpark)

               )


      (section wrapup
               (run-stages grinder-grinder)
   
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
                      (with-size 72 (colorize @t{Thank you} "dark slate blue")))))))

(module+ main
  (require (submod ".." slide-deck))

 (void (run-talk))
 '|That's all, folks!|
)
