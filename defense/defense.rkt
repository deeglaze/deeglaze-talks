#lang at-exp slideshow

#|

To play:

At command line:
racket defense.rkt

In Dr. Racket:
Click run or press F5



Notes for revisions:
* Intro is too general. Could start on "wonderful!"
* "already wasted" joke falls flat. Just make that "language."
* The overview of AAM is boring, too high level, and focuses too much on the concrete.
*  - focus on allocation, non-determinism, and ↦ leading to a finite graph
* Lead out from AAM is too abrupt - need to show what it accomplished before we can say what it didn't accomplish.

* The discussion of relevance needs more pictures to show encapsulated state and to nail home substitutivity.

* Need to add evaluation of the pushdown stuff - refer to Dimitrios' work since it's the same thing before stack inspection.
*  With GC, point to JFP numbers.

* When showing the code, emphasize that it's elegant but SLOW. The transition needs just a little more 
work.

* OAAM part needs an overview of the techniques I used, and why I'm presenting just lazy non-determinism.

The OAAM evaluation should use the ICFP "ballpark" framing, discuss informal comparison to other Scheme analysis. No "yada".

After a modest overview of the numbers, THEN elaborate that the "simple transform"s build up to tedium.

The intro to the language is rough
  The story motivating it is attempting to verify temporal higher-order contracts. Failed. For a year.
  Then I found the real problem. Equality. Is this wrapped value the same as that wrapped value?
  I need pushdown so that contract wrappers don't return to a point in time outside the contract monitor's state.
  It all gets pulled into the language.
  Allocation is entirely mitigated to ensure we can soundly finitize the state space.
  Discuss externals. Give an example. Say String + ⊤ (point to prefix/suffix/trie domains)

  Example rules, example metafunction.
  Evaluation: can express thocons, but the language needs pushdown, GC, weak boxes, sparseness
  Downsides: external allocation functions must be user provided, but I have a prototype that
             helps synthesize them based on types.

Related work needs trimming, better formatting, and small summaries of relation.







So you intentionally scrolled down to see the code.

Welcome to the code for my defense talk. If you are not J. Ian Johnson (me),
there really should be no reason for you to look at this.
For slides that are conceptually exportable to other talks, the

slide-deck

submodule provides the functionality to render them.
If there's a cool animation that isn't a giant hack,
I've exported it to my slideshow-helpers package.
If you want the giant hack, well, read on.


|#
(require (except-in unstable/gui/slideshow big)
         rsvg
         file/convertible
         unstable/gui/ppict
         slideshow/code
         slideshow/flash
         slideshow/play
         slideshow-helpers/picts
         slideshow-helpers/slide
         racket/gui/base
         scheme/runtime-path
         slideshow/balloon slideshow/face
         racket-poppler/render-tex
         racket-poppler
         (submod proposal/presentation pict-utils)
         (except-in (submod dls2014/talkb slide-deck) title)
         (only-in icfp2013-talk/icfp2013 fanout-pict lazy-pict speedup-pict aam-pict)
         foracc
         "wtable.rkt"
         (rename-in
          (only-in (submod icfp2013-talk/icfp2013 slide-deck)
                   intro
                   grinder
                   grinder-grinder
                   same-ballpark
                   talk-focus)
          [intro aam-intro]))
(latex-path "/usr/bin/pdflatex")


(define SCREEN-WIDTH 1024)
(define SCREEN-HEIGHT 768)

(define bg-slide-assembler
  (lambda (title sep content)
    (inset content (- margin) (- margin) 0 0)))

(define-syntax-rule (scode e ...)
  (parameterize ([current-font-size 28])
    (code e ...)))

(current-main-font "Inconsolata")

(define-syntax-rule (wct . forms) (parameterize ([current-main-font "Cantarell"]) . forms))
(define (ct s) (wct (t s)))
(define (cbt s) (wct (bt s)))
(define (cit s) (wct (it s)))

;(define (thanks s)  (text (string-downcase s) "Respective" (current-font-size)))
(define (thanks s) (text s "Respective" (current-font-size)))

(define-syntax-rule (wit . forms) (parameterize ([current-main-font "Inconsolata"]) . forms))
(define (ic s) (wit (t s)))
(define (ibc s) (wit (bt s)))
(define (iic s) (wit (it s)))

(define-syntax-rule (wkt . forms) (parameterize ([current-main-font "Kaushan Script"]) . forms))
(define (kt s) (wkt (t s)))
(define (kit s) (wkt (it s)))

(define use-pdf? #t)
(define-syntax-rule (error-bitmap e) (error 'todov "Render bitmap ~a" 'e))
(define-runtime-path neu-path "neu-logo-bg.png")
(define-runtime-path one-ring-path "one-ring.png")
(define-runtime-path redex-path "../icfp2013/plt-redex.jpeg")

(define-runtime-path addresses-path "addresses.pdf")
(define-runtime-path gamma-path "gamma.pdf")
(define-runtime-path gamma1-path "gamma1.pdf")
(define-runtime-path gammaω-path "gammaomega.pdf")
(define-runtime-path contexts-path "stack-graph.pdf")
(define-runtime-path impl-path "impl.pdf")

(define-runtime-path dino-path "dino-drums.png")

(define-runtime-path iswim-path "iswim.png")
(define-runtime-path abscomp-path "abscomp.png")
(define-runtime-path compiled-path "compiled-step.png")
(define-runtime-path replay-path "replay.png")
(define-runtime-path timestamp-path "timestamp.png")

(define-runtime-path samuel-path "samuel-johnson.jpg")

(define-runtime-path pd-precision-path "pd-precision.png")
(define-runtime-path pd-perf-path "pd-perf.png")
(define-runtime-path gc-pdcfa-path "gc-pdcfa-graphs.png")

(define-runtime-path bird-path "bird2.png")


(define redex-pict (scale (bitmap redex-path) 0.2))

(define c-neu (make-object color% #xad 0 0))
(define (title)
 (parameterize ([current-slide-assembler bg-slide-assembler])
   (slide
    (cc-superimpose
     (bitmap neu-path)
     (vc-append
      (with-size 45 (bold
                     (vc-append gap-size
                                (colorize @ic{Automating} '(45 45 156))
                                (colorize @ic{Abstract Interpretation} '(30 30 110))
                                (colorize @ic{of Abstract Machines} '(15 15 74)))))
      (blank 80)
      (colorize @ic{Thesis Defense of} c-neu)
      (colorize @cbt{Dionna Glaze} c-neu)
      (blank 1 80)
      (with-size 32 (colorize @ic{2015, March 30} c-neu))
      (blank-line))))))

(define (pad-string n str s->pict)
  (define mono-space (ghost (s->pict "0")))
  (apply hc-append 0 (append
                      (make-list (max 0 (- n (string-length str))) mono-space)
                      (list (s->pict str))) ))

(define (trbox #:color color #:text text #:show [show? #t] pict)
  (rt-superimpose
   (cc-superimpose
    (show (colorize (filled-rectangle (pict-width pict) (pict-height pict)) color)
          show?)
    pict)
   (show text show?)))

(module+ slide-deck
  (provide wonderful terrible
           thesis-slide talk-outline
           semantics aam? concrete->abstract
           big-states finitize aam-code
           pd-diagram pd-memo-diagram fib-analogy fib-insights
           substitutional-relevance 
           pd-results
           wins-of-aam aam-drawbacks
           finite-structure when-lookup
           stack-push relevance relevance-useful
           aaml-motivation thocon-problem thocon-example
           lang-intro example-rule
           my-language oneness-intro oneness-problem oneness-solution)

  (define/staged wonderful #:stages [understand
                                     harden
                                     improve]
    #:name 'wonderful
    (define light (make-object color% 210 210 210))
    (define (Sh p c) (shadow p 5 5 #:color c #:shadow-color light))
    (define U
      (vc-append
      (with-size 40 (Sh @ic{Understand code} "forest green"))
      (show
       (with-size 32
         (shadow-frame
          (table 1
                 (list @ic{Value flow}
                       @ic{Control flow}
                       @ic{Type inference})
                 lt-superimpose
                 lt-superimpose
                 gap-size gap-size)))
       (>= stage understand))))
    (define H
      (show
      (vc-append
       (with-size 40 (Sh @ic{Harden code} c-neu))
       (show
        (with-size 32
          (shadow-frame
           (table 1
                  (list @ic{Crash-freedom}
                        @ic{Contract verification}
                        @ic{Information flow}
                        @ic{Data race detection})
                  lt-superimpose
                  lt-superimpose
                  gap-size gap-size)))
        (>= stage harden)))
      (>= stage harden)))
    (define I
      (show
       (vc-append
        (with-size 40 (Sh @ic{Improve code} "midnight blue"))
        (show
         (with-size 32
           (shadow-frame
            (table 1
                   (list @ic{no runtime enforcement}
                         @ic{optimization})
                   lt-superimpose
                   lt-superimpose
                   gap-size gap-size)))
         (>= stage improve)))
       (>= stage improve)))
    (ct-superimpose
     (pin-over
      (pin-over
       (pin-over
        (blank SCREEN-WIDTH SCREEN-HEIGHT)
        30 100
        U)
       500 150 H)
      150 450 I)
     (with-size 50 (ic "Static analysis is wonderful!"))))

  (define (clamp-apply stage v fn n)
    (cond
     [(< stage v) 0]
     [(= stage v) (fn n)]
     [(> stage v) 1]))

  ;; Say the ways it's terrible, then slide the "too hard" off the screen
  ;; to make way for the solutions.
  (define/staged (terrible conclusion?) #:stages [right fast precise understand easy
                                            remove-too-hard move-criteria fade-criteria
                                            crit-right
                                            crit-fast
                                            crit-precise
                                            crit-understand
                                            crit-easy]
    #:name 'terrible
    #:title
    (if conclusion?
        (with-size 50 (ic "Conclusion"))
        (λ (stage)
           (with-size 50
             (panorama
              (pict-cond [(< stage fade-criteria)
                          (hc-append (ic "Static analysis is ") (ic "terrible!"))]
                         [else
                          (hc-append (ic "Static analysis is ")
                                     (colorize (ic "doable!") c-neu))])))))
    #:anim-at [remove-too-hard #:skip-first #:steps 10 #:skip-last]
    #:anim-at [move-criteria #:skip-first #:steps 10 #:skip-last]
    #:anim-at [fade-criteria #:steps 10]
    (with-size 60
      (define too-hard @ic{It's too hard to})
      (define too-easy (tag-pict @ic{It's too easy to} 'te-waste))
      (define th-tags '(th-|make right| th-|make fast| th-|make precise| th-understand))
      (define the-tags (append th-tags (list 'te-waste)))
      (define tags '(|make right| |make fast| |make precise| understand |waste time|))
      (define stages (list right fast precise understand easy))
      (define lefts
        (append
         (for/list ([tag th-tags]) (tag-pict too-hard tag))
         (list too-easy)))
      (define middles (for/list ([tag tags]) (tag-pict (ic (symbol->string tag)) tag)))
      (define rights (list @ic{systematic/automatic}
                           @ic{systematize folklore}
                           @ic{memoize = pushdown}
                           @ic{interpreters}
                           @ic{embedded language}))
      (define colon @ic{:})
      (define (place n)
        (define n1 (clamp-apply stage remove-too-hard fast-end n))
        (define n2 (clamp-apply stage move-criteria fast-start n))
        (define n3 (clamp-apply stage fade-criteria fast-start n))

        (define p
          (apply vl-append 50
                 (for/list ([ls lefts]
                            [ms middles]
                            [rs rights]
                            [st stages]
                            [ltag the-tags]
                            [rtag tags])
                   (define ms:
                     (hc-append
                      (if (eq? rtag '|waste time|)
                          (let ([overline
                                 (filled-rectangle (pict-width (with-size 60 @ic{waste})) 3)])
                            (lt-superimpose ms (fade-in n3 overline)))
                          ms)
                      (fade-in n3 colon)))
                   (define starting-placement (lt-superimpose (blank 930 61)
                                                              (hc-append gap-size ls ms:)))
                   (define lpath (find-tag starting-placement ltag))
                   (define rpath (find-tag starting-placement rtag))
                   (define-values (ls-x ls-y)
                     (lt-find starting-placement lpath))
                   (define-values (ms-x ms-y)
                     (lt-find starting-placement rpath))
                   (define middle-right
                     (show (panorama (pin-over (ghost starting-placement)
                                               (lerp ms-x ls-x n2) ms-y
                                               (hc-append ms:
                                        ;(blank 20 1)
                                                          (if (>= stage (+ st crit-right))
                                                              rs
                                                              (blank 0)))))
                           (>= stage st)))
                   (if (< stage move-criteria)
                       (pin-over
                        middle-right
                        (lerp ls-x (- (- (pict-width ls))
                                 ;; magic number to make it go offscreen
                                 80)
                              n1)
                        ls-y
                        (show ls (>= stage st)))
                       middle-right))))
        (if (= stage remove-too-hard)
            p
            (panorama p)))

      (if (or (= stage remove-too-hard)
              (= stage move-criteria)
              (= stage fade-criteria))
          place
          (place 0))))

  (define thesis-pict
    (parameterize ([current-font-size 30])
      (in-sawasdee
       (vc-append
        gap-size
        (hc-append (tag-pict (hc-append (tag-pict (t "Precise ") 'precise)
                                        (t "and ") (tag-pict (t "performant") 'perf)) 'measure)
                   (t " analyses for higher-order languages"))
        (hc-append (t "can be ") (tag-pict
                                  (hc-append (tag-pict (t "systematically") 'systematic)
                                             (t " and ")
                                             (tag-pict (t "algorithmically") 'algorithmic)
                                             (tag-pict (t " constructed") 'constructed))
                                  'whole-construction))
        (hc-append (t " from their ") (tag-pict (t "semantics.") 'semantics))))))

  (define/staged thesis-slide #:stages [statement built measure semantics
                                                  performant/systematic
                                                  precise/performant/systematic
                                                  precise/automatic]
    #:name 'thesis
    (with-size 30
      (in-sawasdee
       (define bg
         (tagged-shadow-frame
          (hilight-tag
           (hilight-tag
            (hilight-tag
             (hilight-tag
              (hilight-tag
               (hilight-tag
                (hilight-tag
                 (hilight-tag
                  thesis-pict lt-find 'whole-construction #:show (<= built stage measure))
                 lt-find 'measure #:show (= stage measure))
                lt-find 'semantics #:show (= stage semantics))
               lt-find 'systematic #:show (or (= stage performant/systematic)
                                              (= stage precise/performant/systematic)))
              lt-find 'constructed #:show (or (member stage (list performant/systematic
                                                                  precise/performant/systematic
                                                                  precise/automatic))))
             lt-find 'algorithmic #:show (= stage precise/automatic))
            lt-find 'precise #:show (member stage (list precise/automatic
                                                 precise/performant/systematic)))
           lt-find 'perf #:show (member stage (list performant/systematic
                                                   precise/performant/systematic)))
          (inset (big (bt "Thesis:")) 10)))
       (define build-text (big (t "I built and proved correct")))
       (define build-arrow
         (pin-arrow-line 15
                         (pin-over bg -30 300 build-text)
                         build-text ct-find
                         (first (find-tag bg 'whole-construction))
                         (λ (p f) (define-values (x y) (cb-find p f))
                            (values (- x 200) y))
                         #:start-angle (* 1/3 pi)
                         #:end-angle (* 1/2 pi)))
       (match stage
         [(== built) build-arrow]
         [(== measure)
          (define measure-text (big (t "I evaluated")))
          (pin-arrow-line 15
                          (pin-over build-arrow 100 -200 measure-text)
                          measure-text cb-find
                          (first (find-tag bg 'measure)) ct-find
                          #:start-angle (* -1/3 pi)
                          #:end-angle (* -1/2 pi))]
         [else bg]))))

  (define ((focus-on unfocused focused current-stage) pict stage)
    (if (or (and (list? stage) (member current-stage stage))
            (and (number? stage) (= stage current-stage)))
        (colorize pict focused)
        (colorize pict unfocused)))

  (define/staged talk-outline #:stages [aam pushdown oaam language related]
    #:name 'roadmap
    #:title (with-size 60 @kt{Roadmap})
    (define F (focus-on '(150 150 150) '(0 0 0) stage))
    (with-size 40
     (vl-append gap-size
                (vl-append 2
                 (F (kt "Systematic:") `(,aam ,oaam ,pushdown))
                 (vl-append gap-size
                  (F (hc-append (blank 50) (ic "Background (AAM)")) aam)
                  (F (hc-append (blank 50) (ic "Precise [DLS 2014]")) pushdown)
                  (F (hc-append (blank 50) (ic "Performant [ICFP 2013]")) oaam))
                 (F (kt "Automatic:") language)
                 (F (hc-append (blank 50) (ic "Language for AAM")) language))
                (F (ic "Related work") related)
                (F (ic "Conclusion") related))))

  (define/staged semantics #:stages [what-is def intensional abstract-machines
                                             efficient high-level abstracted]
    #:name 'semantics
    #:anim-at [intensional #:skip-first #:steps 10]
    (with-size 50
      (define int @iic{Intensional})
      (define Meaning @ic{Meaning of programs})
      (define meaning @ic{meaning of programs})
      (define pre (panorama (hc-append gap-size (ghost int)
                                       (cc-superimpose Meaning (ghost meaning)))))
      (define post (panorama (hc-append gap-size int
                                        (cc-superimpose (ghost Meaning) meaning))))
      (define base
        (ct-superimpose
         (pin-over (blank SCREEN-WIDTH SCREEN-HEIGHT)
                  -20 -20
         (pin-over (bitmap bird-path)
                   314 100
                   (with-size 60 (colorize (ic "?") "white"))))
         (colorize
          (with-size 50 (hc-append gap-size @ic{What} @iic{is} @ic{semantics?}))
          "white")))
      (define (build n)
        (cc-superimpose
         base
         (colorize
          (with-size 50
            (vl-append
             gap-size
             (panorama (fade-pict n pre post))
             (blank 50)
             (hc-append
              (blank 300)
              (shadow
               (vl-append
                gap-size
                (show @ic{Abstract machines} (>= stage abstract-machines))
                (show (hc-append (blank 30) @ic{Reasonably efficient}) (>= stage efficient))
                (show (hc-append (blank 30) @ic{Approachable}) (>= stage high-level))
                (show (hc-append (blank 30) @ic{Easily abstracted}) (>= stage abstracted))
                (blank 80))
               10 5 #:shadow-color (make-object color% #x48 #x46 #x44)))))
          "white")))
      (cond
       [(= stage what-is)
        base]
       [(= stage intensional)
        build]
       [else
        (define built (build (clamp-apply stage intensional values 0)))
        (pin-over
         built
         (- (pict-width built) (pict-width redex-pict) 19)
         (- (pict-height built) (pict-height redex-pict) 20)
         (show redex-pict (>= stage abstract-machines)))])))

  (define/staged aam? #:stages [what paper]
    (cc-superimpose
     (with-size 60 (vc-append gap-size @iic{Abstract}
                              @ic{abstract machines?}))
     (show (shadow-frame (force aam-pict)) (= stage paper))))

  (define (n->byte i)
    (when (or (< i 0) (> i 255))
      (error 'n->byte "Out of range: ~a" i))
    (if (exact? i)
        (truncate i)
        (inexact->exact (truncate i))))


  (define clock (* 2/12 pi))
  (define πfourth (/ pi 4))
  (define π8 (/ pi 8))
  (define 2π (* 2 pi))
  (define finders
    (list rc-find rt-find ct-find lt-find lc-find lb-find cb-find rb-find))
  (define (within-range point anchor ±range)
    (and (< point (+ anchor ±range))
         (<= (- anchor ±range) point)))
  (define (angle->finder θ [adj 0])
    (define ϑ
      ;; should have a closed form, but whatever.
      (let norm ([θ (+ θ adj)])
        (cond [(< θ 0) (norm (+ θ 2π))]
              [(< θ 2π) θ]
              [else (norm (- θ 2π))])))
    (or
     (for/or ([finder (in-list finders)]
              [i (in-naturals)])
       (and (within-range ϑ (* i πfourth) π8)
            finder))
     rc-find))
  ;; The location of the out finder is 2 finders clockwise from the cardinal position
  (define (angle->find-out θ) (angle->finder (- θ (/ pi 2))))
  ;; The location of the in finder is 2 finders counterclockwise from the cardinal position
  (define (angle->find-in θ) (angle->finder (+ θ (/ pi 2))))

  (define states-to-step 10)
  (define tneg-col "cadetblue")
  (define tpos-col "tomato")
  (define fpos-col "olive")
  (define/staged concrete->abstract #:stages [reduction infinite crush
                                                        true-positive
                                                        false-positive
                                                        true-negative]
    #:title (λ (stage)
               (with-size 50
                (cond
                 [(<= stage infinite)
                  (kt "Reachability is undecidable")]
                 [(>= stage crush)
                  (kt "Abstraction: representation vs. denotation")])))
    #:anim-at [infinite #:skip-first #:steps states-to-step #:delay 0.5]
    #:anim-at [crush #:skip-first]

    (define (conc n)
      (pin-over-hcenter @ct{↦} 0 -5 (fade-in n (ct (string (integer->char #x302))))))
    (define-values (cw ch) (values 300 300))
    (define-values (aw ah) (values 100 100))
    (define-values (asw ash) (values 600 600))
    (define conc-space (thick-ellipse cw ch 3))
    (define state-radius (- (/ cw 2) 20))
    (define conc-space-unfocused
      (pin-over-center
       (thick-ellipse cw ch 3 #:color '(180 180 180)
                      #:fill-color '(120 120 120)
                      #:border-style 'long-dash)
       (* (- state-radius 30) (cos (/ pi 4)))
       (* (- state-radius 30) (sin (/ pi 4)))
       (show
        (colorize (filled-ellipse 25 25) tpos-col)
        (>= stage true-positive))))
    (define-values (cx cy)
      (values (/ (pict-width conc-space) 2)
              (/ (pict-height conc-space) 2)))
    (define (steps n)
      (define states (n->byte (* n states-to-step)))
      ;; start at clockface 12, count around to 9, then add ∞
      (for/acc ([last #:drop #f]
                [lastθ #:drop #f]
                [pict (blank cw ch)])
               ([s (in-range states)])
         (define θ (- (/ pi 2) (* s clock)))
         (define state-pict
           (with-size 20 (ct (cond
                              [(= s (- states-to-step 1)) "∞"]
                              [(= s (- states-to-step 2)) "…"]
                              [else
                               (format "s~a" s)]))))
         (define placement
           (pin-over-center
            pict
            (+ cx (* state-radius (cos θ)))
            ;; y is reversed. Positive on circle is negative on screen.
            (+ cy (* state-radius (- (sin θ))))
            state-pict))
         (define pict*
           (if last
               (pin-arrow-line 8 placement
                               last (angle->find-out lastθ)
                               state-pict (angle->find-in θ)
                               #:line-width 2)
               placement))
         (values state-pict θ pict*)))
    (define base (ghost (thick-ellipse asw ash 3)))
    (define abs-end-color '(210 210 210))
    (define (build n*)
      (define nc (clamp-apply stage crush fast-start n*))
      (cc-superimpose
       base ;; need this so the cc doesn't jitter in the animation.
       ;; abstract space grows
       (pin-over-center
        (pin-over-center
         (thick-ellipse (lerp cw asw nc) (lerp ch ash nc) 3
                        #:fill-color (and (>= stage crush)
                                          abs-end-color))
         (+ (/ asw 2) (* (- (/ asw 2) 70) (cos 0)))
         (+ (/ ash 2) (* (- (/ asw 2) 70) (- (sin 0))))
         (show (cc-superimpose (colorize (filled-ellipse 25 25) fpos-col)
                               (colorize (with-size 20 (ct "!")) "white"))
               (>= stage false-positive)))
        (+ (/ asw 2) (* (+ (/ asw 2) 70) (cos (/ pi -4))))
        (+ (/ ash 2) (* (+ (/ asw 2) 70) (- (sin (/ pi -4)))))
        (show (colorize (filled-ellipse 25 25) tneg-col) (>= stage true-negative)))
       ;; concrete space becomes unfocused
       (fade-pict nc conc-space conc-space-unfocused)
       ;; abstract relation shrinks
       (show
        (thick-ellipse (lerp cw aw nc) (lerp ch ah nc) 3 #:color "black"
                       #:fill-color (map n->byte (lerp* '(255 255 255) abs-end-color nc))
                       #:border-style 'dot)
        (>= stage crush))
       ;; show some states stepping
       (fade-out nc (steps (clamp-apply stage infinite values n*)))
       ;; relation gets a hat
       (conc nc)))
    (define dark (make-object color% 180 180 180))
    (define light (make-object color% 210 210 210))
    (if (or (= stage crush)
            (= stage infinite))
        build
        (pin-over
         (pin-over
          (pin-over
           (build 0)
           60 100
           (show (inset (shadow @ct{True positive} 5 5
                                #:color tpos-col
                                #:shadow-color dark)
                        5)
                 (>= stage true-positive)))
          330 100
          (show (inset (shadow @ct{False positive} 5 5
                               #:color fpos-col
                               #:shadow-color dark)
                       5) (>= stage false-positive)))
         550 450
         (show (shadow @ct{True negative} 5 5
                       #:color tneg-col
                       #:shadow-color light) (>= stage true-negative))))
    )

  (define/staged big-states #:stages [big prob1 prob2 prob3 solution expl1 expl2]
    #:name 'big-states
    #:title (with-size 50 (ic "Crushing the state space"))
    (with-size 50
     (vl-append gap-size
                (ct "〈expr heap cont〉")
                (show (ic "Problem 1: ∞ addresses") (>= stage prob1))
                (show (ic "Problem 2: ∞ structure") (>= stage prob2))
                (show (ic "Problem 3: ∞ values (e.g. ℕ)") (>= stage prob3))
                (blank 30)
                (show (ic "Solution: finitize (implicit) allocation") (>= stage solution))
                (show (ic "Bounded allocation bounds state space") (>= stage expl1))
                (show (ic "↦ defines a finite graph") (>= stage expl2)))))

  (define/staged finitize #:stages [meaning reuse alive before now both either nondet]
    #:name 'finitize
    (with-size 48
      (pin-over
       (vl-append
        gap-size
        @ic{What does it mean to finitize allocation?}
        (show @ic{Some allocations will reuse addresses.} (>= stage reuse))
        (show @ic{Old usage not dead!} (>= stage alive))
        (hc-append 0 (show @ct{[α(a₁) ↦ v₁]} (>= stage before)) (show @ct{⊔[α(a₂) ↦ v₂]} (>= stage now)))
        (show @ct{[â₁ ↦ {v₁, v₂}]} (>= stage both))
        (show (hc-append gap-size @ic{Lookup} @ct{â₁} @ic{means} @iic{either} @ct{v₁} @iic{or} @ct{v₂})
              (>= stage either))
        (show (hc-append gap-size @ct{〈â σ κ〉 ↦ 〈v σ κ〉} @ic{where} @ct{v ∈ σ(â)})
              (>= stage nondet)))
       550 125
       (show (pdf->pict addresses-path 0.7)
             (>= stage reuse)))))

  (define/staged aam-code #:stages [entirety general specific]
    (ct-superimpose
     (show (with-size 50 @kt{That's it.}) (= stage entirety))
     (cc-superimpose
      (pin-over
       (pin-over (blank SCREEN-WIDTH SCREEN-HEIGHT)
                 280 -20
                 (show (rt-superimpose
                        (colorize (filled-rectangle (- SCREEN-WIDTH 300) SCREEN-HEIGHT)
                                  '(210 210 210))
                        (inset (colorize @kt{Language specific} "white") 10))
                       (= stage specific)))
       -20 -20
       (show
        (rt-superimpose
         (colorize (filled-rectangle 300 SCREEN-HEIGHT) '(160 160 160))
         (inset (colorize @kt{General} "white") 10))
        (>= stage general)))
      (pin-over-hcenter
       (blank 0)
       -100 -310
       (hc-append
        50
        (scale
         (code
          (code:comment "all transparent")
          (struct state (point σ κ))
          (struct mt ())
          (struct kaddr (a))
          code:blank
          (define F (mutable-set))
          (define R (mutable-set))
          (define Seen (mutable-set))
          code:blank
          (define (add-state! s)
            (unless (set-member? Seen s)
              (set-add! F s)
              (set-add! Seen s)))
          code:blank
          (define (add-reduction! s0 s1)
            (set-add! R (cons s0 s1))
            (add-state! s1))
          code:blank
          (define (analyze e)
            (set-clear! F)
            (set-clear! R)
            (set-clear! Seen)
            (define κa (gensym))
            (define ς₀
              (state (cons e (hash))
                     (hash κa (set (mt)))
                     κa))
            (set-add! F ς₀)
            (do () ((set-empty? F))
              (define ς (set-first F))
              (set-remove! F ς)
              (step ς))
            '|the final system|
            R))
         0.4)
        (scale
         (code
          (code:comment "continuation frames")
          (struct ar (e ρ))
          (struct fn (v))
          code:blank
          (code:comment "Syntax")
          (struct ref (x))
          (struct app (ℓ e0 e1))
          (struct lam (x e))
          code:blank
          (code:comment "Semantics")
          (define (step s)
            (match s
              [(state (cons (ref x) ρ) σ κ)
               (define a (hash-ref ρ x))
               (for ([v (in-set (hash-ref σ a))])
                 (add-reduction! s (state v σ κ)))]
              [(state (cons (app ℓ e0 e1) ρ) σ κ)
               (define κa (alloc s))
               (add-reduction! s
                               (state (cons e0 ρ) (hash-store σ κa κ)
                                      (cons (ar e1 ρ) κa)))]
              [(state v σ (cons (ar e ρ) κa))
               (add-reduction! s
                               (state (cons e ρ) σ (cons (fn v) κa)))]
              [(state v σ (cons (fn (cons (lam x e) ρ)) κa))
               (define a (alloc s))
               (define ρ* (hash-set ρ x a))
               (define σ* (hash-add σ a v))
               (for ([κ (in-set (hash-ref σ κa))])
                 (add-reduction! s (state (cons e ρ*) σ* κ)))]
              [_ (void)])))
         0.4))))))

  (define (pdf->pict file [α 1])
    (bitmap (scale (page->pict (pdf-page (open-pdf file) 0)) α)))

  (define/staged finite-structure #:stages [implicit unbounded problematic explicit]
    #:name 'finite-structure
    #:title (with-size 50 (ic "Implicit allocation"))
    (define cons (with-size 50 (hc-append @ct{(cons frame kont)} (blank 100 20))))
    (with-size 50
      (vl-append gap-size
                 (pin-over cons (+ 10 (pict-width cons)) 0
                           (show (stack-push 0) (>= stage unbounded)))
                 (blank 30)
                 (show (hc-append gap-size @ct{kont} @ic{is problematic.})
                       (>= stage problematic))

                 (blank 50)
                 (show (vl-append @ct{let a = new location()}
                                  @ct{store! a kont}
                                  @ct{(cons frame a)})
                       (>= stage explicit)))))

  (define/staged wins-of-aam #:stages [full-aam almost oversimplification slow]
    #:title (with-size 50 @kt{AAM is enough to...})
    (vc-append
     (vl-append gap-size
                @ic{Define higher-order monotone framework [Kam,Ullman 77]}
                @ic{Reconstruct Olin's dissertation [Shivers 91]}
                @ic{Justify lightweight closure conversion [Steckler,Wand 97]})
     (blank 50)
     (show @ic{Almost.} (>= stage almost))
     (blank 50)
     (show @ic{Oversimplification.} (>= stage oversimplification))
     (blank 50)
     (show @ic{Math as code is slow.} (>= stage slow))))

   
  (define/staged aam-drawbacks #:stages [finite lossy pda special solution]
    #:title (with-size 50 @kt{Drawbacks to AAM})
    (define memo @ic{Memoization})
    (vc-append
     gap-size
     (with-size 40
       (vl-append
        gap-size
        @ic{Finite ⇒ NFA}
        (show @ic{Push/pop ↔ call/return approximated in NFA} (>= stage lossy))
        (show (hc-append (pict-if #:combine cc-superimpose (< stage solution) @ic{?}
                                  (refocus (filled-flash-frame memo #:color "yellow") memo))
                         @ic{ ⇒ PDA})
              (>= stage pda))))
     (show
      (with-size 72
        (vc-append
         gap-size
         @kt{Generally, finitize structure.}
         @kt{But, the stack is special.}))
      (>= stage special))))
  
  ;; mode:
  ;; 0: just lines
  ;; 1: lines and shapes
  ;; 2: 1 + circle call trace
  ;; 3: 1 + circle whole call and add dotted skip line.
  (define (call-diagram call-style mode left? state?)
    (define (mk) (colorize (filled-ellipse 25 25) "darkgreen"))
    (define lκ (colorize (ct "κ") "darkgreen"))
    (define rκ (colorize (ct "κ") "cadetblue"))
    (define hspace (if state? 0 20))
    (define-values (cx cy)
      (if state?
          (values 300 550)
          (values 40 425)))
    (define-values (rx ry)
      (if state?
          (values 220 120)
          (values 30 80)))
    (define-values (cs-find ce-find rs-find rp-find)
      (if state?
          (values lc-find
                  (if (>= mode 1) rc-find cb-find)
                  (if (>= mode 2) rc-find cb-find)
                  (if (>= mode 2) lc-find ct-find))
          (values (if (>= mode 1) lc-find cc-find)
                  (if (>= mode 1) rc-find cb-find)
                  rt-find
                  lb-find)))
    (define call-site
      (if state?
          (pin-over
           (hc-append (ct "〈") (if (= mode 0)
                                    (ct "call heap₀")
                                    (mk))
                      (ct " ") lκ (ct "〉"))
           -25 -100
           (show (hc-append (ct "Ξ⊔[")
                            (mk)
                            (ct " ↦ {")
                            lκ
                            (if (>= mode 3) (hc-append (ct ", ") rκ) (blank 0))
                            (ct "}]")) (>= mode 1)))
          (show (mk) (>= mode 1))))
    (define call-site2
      (hc-append (ct "〈") (if (= mode 0)
                               (ct "call heap₀")
                               (mk))
                 (ct " ") rκ (ct "〉")))
    (define call-entry
      (show (if state?
                (hc-append (ct "〈") (ct "body heap₁ ") (ct "(memo ") (mk) (ct ")") (ct "〉"))
                (mk)) (>= mode 1)))
    (define return-site (if state?
                            (show (hc-append (ct "〈v heap₂ (memo ") (mk) (ct ")〉"))
                                  (>= mode 2))
                            (blank 0)))
    (define return-point (if state?
                             (pin-over
                              (show (hc-append (ct "〈v heap₂ ") lκ (ct "〉")) (>= mode 2))
                              -25 100
                              (show (hc-append (ct "M⊔[") (mk) (ct "↦ {〈v,heap₂〉}]")) (>= mode 2)))
                             (blank 0)))
    (define return-point2 (show (hc-append (ct "〈v heap₂ ") rκ (ct "〉")) (>= mode 3)))
    (define return-ongoing (blank 0))
    (define return-ongoing2 (blank 0))
    (define call-incoming (blank 0))
    (define call-incoming2 (blank 0))
    (define outer-call
      (pin-arrow-line
       15
       (vc-append return-site (blank 1 200) call-entry)
       call-entry (if (and state? (>= mode 1)) ct-find cb-find) #:under? #t
       return-site cb-find))
    (define call-return
      (pin-arrow-line
       15
       (pin-arrow-line
        15
        (vc-append return-ongoing
                   (blank 100)
                   return-point
                   (blank (+ 100 (pict-height outer-call)))
                   call-site
                   (blank 100)
                   call-incoming)
        call-incoming ct-find
        call-site (if state? cb-find (if (>= mode 1) cb-find cc-find))
        #:line-width 8 #:style call-style)
       return-point ct-find
       return-ongoing cb-find
       #:line-width 8 #:style call-style))
    (define call-return2
      (pin-arrow-line
       15
       (pin-arrow-line
        15
        (pin-arrow-line
         15
         (vc-append return-ongoing2
                    (blank 100)
                    return-point2
                    (blank (+ 100 (pict-height outer-call)))
                    call-site2
                    (blank 100)
                    call-incoming2)
         call-incoming2 ct-find
         call-site2 cb-find
         #:line-width 8 #:style 'long-dash)
        return-point2 ct-find
        return-ongoing2 cb-find
        #:line-width 8 #:style 'long-dash)
       call-site2 ct-find
       return-point2 cb-find
       #:line-width 2 #:style 'dot))
    (define out
      (panorama
       (pin-over
        (pin-over
         (pin-arrow-line
          15
          (pin-arrow-line
           15
           (hc-append
            (if state?
                outer-call
                (pin-under-center outer-call
                                  (/ (pict-width outer-call) 2)
                                  (/ (pict-height outer-call) 2)
                                  (show (ellipse (* 1.6 (pict-width outer-call))
                                                 (* 1.6 (pict-height outer-call)))
                                        (= mode 2))))
            (blank hspace)
            call-return)
           call-site cs-find
           call-entry ce-find)
          return-site rs-find
          return-point rp-find)
         cx cy (ic "Call"))
        rx ry (ic "Return"))))
    (define (polyfill base point-images)
      (define points
        (for/list ([img (in-list point-images)])
          (define-values (x y) (cc-find base img))
          (list x y)))
      (filled-polygon points #:color '(210 210 210) #:fill-style 'crossdiag-hatch))
    (if state?
        (pin-arrow-line
         15
         (hc-append out (show call-return2 (= mode 3)))
         return-site rs-find
         return-point2 lc-find
         #:style (if (= mode 3) 'solid 'transparent)
         #:hide-arrowhead? (not (= mode 3)))
        (if (= mode 3)
            (lt-superimpose
              (pin-over
               (polyfill (ghost out)
                         (list call-site call-entry return-site return-point))
               50 250
               @ic{Skip})
              out)
            out)))

  (define/staged pd-diagram #:stages [first-call package second-call short-circuit]
    (hc-append (call-diagram 'solid (min stage second-call) #t #f)
               (blank 50)
               (show (with-size 50 @ct{⇒}) (>= stage second-call))
               (blank 50)
               (show (call-diagram 'long-dash
                                   (if (= stage second-call) 1 stage)
                                   #f #f)
                     (>= stage second-call))))

  (define/staged pd-memo-diagram #:stages [package in-Ξ return-M second-call]
    (call-diagram 'solid (min stage second-call) #t #t))
  
  (define fibm
    (code (define memo (make-hash))
          (define (fibm n)
            (cond
             [(hash-has-key? memo n)
              (hash-ref memo n)]
             [else
              (code:comment "compute (fib n)")
              (define fn
                (+ (fibm (- n 1)) (fibm (- n 2))))
              (hash-set! memo n fn)
              fn]))))

  (define/staged fib-analogy #:stages [non-memo memo]
    (vc-append 50
               (code (define (fib n)
                       (if (<= n 1)
                           1
                           (+ (fib (- n 1)) (fib (- n 2))))))
               (show fibm
                     (>= stage memo))))

  (define (fib-insights)
    (with-size 60
     (slide @kt{Insight 1: callers don't matter}
            'next
            (hc-append @kt{Insight 2: only } (code n) @kt{ is relevant})
            'next
            @kt{Insight 3: store relevant if stateful})))

  (define (substitutional-relevance)
    (slide #:title (with-size 50 (kt "Relevance"))
           (with-size 50 (hc-append @ic{Context } @ct{ctx} @ic{ extendable to a state}))
           @ct{∀ K. combine(〈ctx (memo ctx)〉, K) ↦* combine(〈v (memo ctx)〉, K)}
           'next
           (with-size 50 @ct{∀ E. E[e] ↦* E[v]})))

  (define/staged pd-results #:stages [graphs numbers]
    (if (= stage graphs)
        (bitmap gc-pdcfa-path)
        (vc-append (bitmap pd-precision-path)
                   (blank 50)
                   (bitmap pd-perf-path))))
  
  (define/staged when-lookup #:stages [example-rule example-expr fanout me lazy]
    #:title (with-size 50 @ic{Nondeterminism in AAM is too eager:})
     (vc-append gap-size                
                (hc-append gap-size @ct{〈x ρ σ κ〉 ↦ 〈v σ κ〉} @ic{where} @ct{v ∈ σ(ρ(x))})
      (show (code (f x y)) (>= stage example-expr))
      (show (force fanout-pict) (>= stage fanout))
      (show (hc-append gap-size @ic{Look up} @iic{by need:}) (>= stage me))
      (show (hc-append gap-size @ct{〈x ρ σ κ〉 ↦ 〈(Delay ρ(x)) σ κ〉})
            (>= stage me))
      (show (vc-append (arrow 20 (/ pi -2))
                       (force lazy-pict))
            (>= stage lazy))))


  (define/staged aaml-motivation #:stages [why problem]
    #:title (with-size 50 @ic{A language to ease the pain})
    (with-size 60
     (vc-append gap-size
                @kt{Not because I could.}
                @kt{Because I needed to.}
                (blank 50)
                (show (with-size 50 @ic{Problem: temporal higher-order contracts})
                      (= stage problem)))))

  (define/staged thocon-problem #:stages [what temporal not-how identity]
    #:title (with-size 50 @kt{What are they?})
    (with-size 50
      (vl-append gap-size
                 (vl-append gap-size @ic{Higher-order contracts}
                            (hc-append (blank 50) (ct "C ∈ Contract ::= C → C | flat(e)")))
                 (show (hc-append gap-size @iic{Who} @ic{is called} @iic{when,}
                                  @ic{with} @iic{whom?})
                       (>= stage temporal))
                 (show (hc-append gap-size @iic{How} @ic{is not the hard part.})
                       (>= stage not-how))
                 (show (hc-append gap-size @iic{Identity} @ic{is the hard part.})
                       (>= stage identity)))))

  (define/staged thocon-example #:stages [for-example identity implementation]
    #:title (with-size 50 @kt{For example})
    (with-size 50
     (vl-append gap-size
                @ic{No smuggling:}
                (with-size 40 @ct{¬ (call(f,g) (¬ret(f,_))* ret(f,_) (¬call(g,_))* call(g,_))})
                (show
                 (hc-append gap-size @iic{The} @ct{g} @ic{passed to} @iic{the} @ct{f} 
                            @ic{for} @iic{the} @ct{call})
                 (>= stage identity))
                (show (ht-append
                       gap-size
                       @ic{Implementation:}
                       (vl-append gap-size 
                                  @ic{pointer equality of}
                                  (hc-append gap-size @ct{g} @ic{contract wrapper})))
                      (>= stage implementation)))))

   (define/staged lang-intro #:stages [built-in sound finite concrete functions]
     #:name 'lang-intro
     #:title (with-size 50 @kt{Language features})
     (with-size 50
       (vl-append gap-size
                  (hc-append gap-size
                             @iic{Pattern}
                             @ct{↦}
                             @iic{Expression}
                             @iic{[Side-conditions]})
                  (blank 30)
                  (show @ic{Equality is built-in}
                        (>= stage built-in))
                  (show @ic{Any allocation strategy is sound}
                        (>= stage sound))
                  (show @ic{Finite allocation ⇒ computable}
                        (>= stage finite))
                  (show @ic{Fresh allocation ⇒ complete}
                        (>= stage concrete))
                  (show @ic{First-order metafunctions} (>= stage functions)))))

   (define-syntax left-nest
     (syntax-rules ()
       [(_ fn base) base]
       [(_ fn base [args ...] rest ...)
        (left-nest fn (fn base args ...) rest ...)]))

   (define/staged example-rule #:stages [rule pattern side-condition expr
                                              built-in
                                              metafunction
                                              nonlinear]
     #:title (λ (stage)
                (with-size 50
                 (cond
                  [(= stage pattern) @kt{Pattern}]
                  [(= stage side-condition) @kt{Side-condition}]
                  [(= stage expr) @kt{Expression}]
                  [(= stage built-in) @kt{Store built-in}]
                  [(= stage metafunction) @kt{Metafunctions use ordered rules}]
                  [(= stage nonlinear) @kt{Non-linear patterns}]
                  [else (ghost @kt{Pattern})])))
     (define rule-pict
       (with-size 40
        (vc-append
         (ht-append
          gap-size
          (tag-pict @ct{ev〈(x := Name) ρ κ〉} 'pat1)
          (vl-append
           gap-size
           (hc-append gap-size @ct{↦} (tag-pict @ct{co〈κ v〉} 'expr1))
           (tag-pict
            (hc-append
             (ct "[where ")
             (tag-pict (ct " v ") 'pat2) 
             (tag-pict (hc-append (ct "(")
                                  (tag-pict (ct "lookup ") 'lookup)
                                  (tag-pict (ct "(elookup ρ x)") 'metafunction)
                                  (ct ")")) 'expr2)
             (ct "]"))
            'sc))))))

     (vc-append
      (blank 60)
      (hilight-tag
       (hilight-tag
        (hilight-tag
         (hilight-tag
          (hilight-tag
           (hilight-tag
            (hilight-tag rule-pict lt-find 'pat1 #:show (= stage pattern))
            lt-find 'pat2 #:show (= stage pattern))
           lt-find 'expr1 #:show (= stage expr))
          lt-find 'expr2 #:show (= stage expr))
         lt-find 'sc #:show (= stage side-condition))
        lt-find 'lookup #:show (= stage built-in))
       lt-find 'metafunction #:show (= stage metafunction))
      (blank 60)      
      (show
       (with-size 40
        (vl-append
         gap-size
         (hilight-tag
          (hilight-tag
           (hc-append (ct "elookup〈econs〈")
                      (tag-pict (ct "x") 'patx1)
                      (ct " v _〉 ")
                      (tag-pict (ct "x") 'patx2)
                      (ct "〉 ↦ v"))
           lt-find 'patx1 #:show (= stage nonlinear))
          lt-find 'patx2 #:show (= stage nonlinear))
         @ct{elookup〈econs〈_ _ ρ〉 x〉 ↦ (elookup ρ y)}))
       (>= stage metafunction))
      ))
   
#|
 (show
       (scale
        (latex->pict
         (string-join
          (list
           "\\begin{align*}"
           "p \\in \\mathit{Pattern} &::= x \\texttt{:=} p \\mid n\\langle p \\ldots\\rangle"
           "\\mid \\texttt{Address} \\mid \\texttt{(External}\\ E\\texttt{)} \\mid \\texttt{\\_} \\\\"
           "&x \\equiv x \\texttt{:=} \\texttt{\\_}"
           "\\end{align*}")
          "\n"))
        3)
       (>= stage pat-grammar))
|#

   (define/staged my-language
     #:stages [grammar variants externals
                       allocators store-interaction mode
                       delay resolve deref]
     #:name 'lang-in-a-slide
     #:title @ic{My language in a slide}
     (cc-superimpose
      (bitmap
       (scale
        (left-nest
         hilight-tag
         (left-nest
          pin-under
          (latex->pict
           (string-join
            (list
             "\\begin{align*}"
             

             "e \\in \\mathit{Expr} &::= x \\mid"
             "\\texttt{(}n\\ \\mathit{tag}\\ e \\ldots\\texttt{)} \\mid"
             "\\texttt{(let (} \\mathit{bu} \\ldots\\texttt{)}\\ e\\texttt{)} \\mid \\texttt{(}f\\ e \\ldots\\texttt{)} \\\\"

             "&\\phantom{::=}\\mid \\texttt{(alloc}\\ \\mathit{tag}\\texttt{)} \\mid \\texttt{(lookup}\\ e\\ \\mathit{mode}\\texttt{)} \\\\"
             "\\mathit{bu} \\in \\mathit{BU} &::= \\texttt{[where}\\ p\\ e\\texttt{]} \\mid \\texttt{[update}\\ e_a\\ e_v\\texttt{]} \\\\"
             "\\phantom{::=}\\\\"
             "\\mathit{rule} \\in \\mathit{Rule} &::= \\texttt{[-->}\\ p\\ e\\ \\mathit{bu} \\ldots\\texttt{]} \\\\"
             "\\phantom{::=}\\\\"
             "\\mathit{MF} &::= \\mathbf{User}(\\overline{\\mathit{rule}}) \\mid \\mathbf{ExtMF}(\\mathit{emf}) \\\\"
             "\\mathit{emf} \\in & \\mathit{State} \\times \\widehat{\\mathit{Term}}^* \\to \\mathit{EvRes}[\\widehat{\\mathit{Term}}] \\text{ in meta-meta-language}\\\\"
             "x,n,f  &\\mathit{Names} \\\\"
             "\\mathit{tag} \\in &\\mathit{Tag} \\text{ some set}"
             "\\phantom{::=}\\\\"
             "E \\in &\\mathit{External\\text-descriptor} ::= \\langle\\mathit{name}, \\sqcup, \\equiv \\rangle"
             "\\end{align*}")
            "\n"))
          [125 12 (tag-pict (blank 42 13) 'pvariant)]
          [110 27 (tag-pict (blank 57 13) 'evariant)]
          [100 43 (tag-pict (blank 53 13) 'alloc)]
          [55 167 (tag-pict (blank 82 13) 'tagset)]
          [162 43 (tag-pict (blank 76 13) 'lookup)]
          [210 43 (tag-pict (blank 22 13) 'mode)]
          [157 58 (tag-pict (blank 68 13) 'update)]
          [221 12 (tag-pict (blank 63 13) 'pext)]
          [152 120 (tag-pict (blank 63 13) 'emf)]
          [50 136 (tag-pict (blank 267 18) 'emftype)]
          [58 182 (tag-pict (blank 190 13) 'extdesc)])
         [lt-find 'pvariant #:show (= stage variants)]
         [lt-find 'evariant #:show (or (= stage variants)
                                       (= stage allocators))]
         [lt-find 'alloc #:show (or (= stage allocators)
                                    (= stage store-interaction))]
         [lt-find 'tagset #:show (= stage allocators)]
         [lt-find 'lookup #:show (= stage store-interaction)]
         [lt-find 'update #:show (= stage store-interaction)]
         [lt-find 'mode #:show (= stage mode)]
         [lt-find 'pext #:show (= stage externals)]
         [lt-find 'emf #:show (= stage externals)]
         [lt-find 'emftype #:show (= stage externals)]
         [lt-find 'extdesc #:show (= stage externals)])
        3))
      (vc-append
       gap-size
       (show (hc-append gap-size (shadow-frame @ic{Delay})
                        (force lazy-pict))
             (>= stage delay))
       (show (hc-append gap-size (shadow-frame @ic{Resolve})
                        (force fanout-pict))
             (>= stage resolve))
       (show (ht-append gap-size
                        (shadow-frame @ic{Deref})
                        (vc-append gap-size
                         (force lazy-pict)
                         (inset (shadow @ic{Different behavior for fresh addresses} 10 5) 10)))
             (>= stage deref)))))

   (define (stack-push stage)
     (define arrow-start (blank 0))
     (define arrow-end (blank 0))
     (define stack-base
       (apply vl-append 10
              (for/list ([i 6])
                (colorize (filled-rectangle 100 40) `(10 10 ,(- 220 (* i 30)))))))
     (define base-circ
       (show (scale (ellipse (pict-width stack-base) (pict-height stack-base)) 1.5)
             (>= stage 2)))
     (define-values (bx by) (mk-center 0 0 stack-base base-circ))
     (define circ-stack-base
       (pin-under stack-base bx by
                  base-circ))
     (define ellipses-and-anchors
       (vl-append
        50
        arrow-start
        (vc-append
         2
         arrow-end
         circ-stack-base
         (blank 5) ;; vdots
         (filled-rectangle 4 4)
         (blank 5)
         (filled-rectangle 4 4)
         (blank 5)
         (filled-rectangle 4 4))))
     (define pushed
       (colorize (filled-rectangle 100 40) `(10 10 250)))
     (define circ-push
       (show (scale (ellipse (pict-width pushed) (pict-height pushed)) 1.5)
             (= stage 1)))
     (define-values (px py) (mk-center 0 0 pushed circ-push))
     (define stack-graph
       (show
        (vc-append @ct{Ξ}
                   (pdf->pict contexts-path 0.9))
        (= stage 3)))
     (define stack
       (pin-over-vcenter
        (panorama
         (hc-append
          (pin-over
           ellipses-and-anchors
           -102 -20
           (pin-under pushed px py circ-push))
          (blank 200 400)))
        300 200 stack-graph))
     (pin-arrow-line 10 stack arrow-start lt-find arrow-end lt-find
                     #:line-width 2
                     #:start-angle 0 #:end-angle (/ pi -2) #:start-pull 0.5 #:end-pull .2))

   (define/staged relevance #:stages [powerful top whole graph]
     #:name 'relevance
     #:title (with-size 70 @kt{Relevance is powerful})
     (show (stack-push stage) (>= stage top)))

   (define/staged relevance-useful #:stages [useful trick meaning]
     #:name 'relevance-useful
     #:title (with-size 70 @kt{Relevance is useful})
     (with-size 50
       (cc-superimpose
        (vc-append
         50
         (vl-append gap-size
                    @ic{Garbage collection}
                    @ic{Access control security}
                    @ic{Continuation marks})
         (vl-append gap-size
                    (show @ic{Each stack property ∈ Context} (>= stage trick))
                    (show @ic{States delineated by stack property} (>= stage meaning)))))))

   (define/staged oneness-intro #:stages [uses relation]
     #:title (with-size 70 @kt{The importance of “oneness”})
     #:name 'oneness
     (with-size 50
       (vl-append
        30
        (vl-append (hc-append @ic{Observable allocation})
                   (blank 15)
                   (hc-append (blank 50) @tt{ref ()} @ic{, } @tt{(gensym)} @ic{, etc.})
                   (show @ic{Oneness and sameness are related} (>= stage relation))))))

   (define/staged oneness-problem #:stages [setup question of-course gg-dude
                                                  abs-setup
                                                  follow-up
                                                  abs concretization ctx ctx2]
     (with-size 50
       (if (<= stage gg-dude)
           (vc-append
            50
            (vl-append
             gap-size
             (show (hc-append gap-size
                              @ic{Concrete: both}
                              (colorize @ct{f} "darkgreen")
                              @ic{and}
                              (colorize @ct{g} "darkgreen")
                              @ic{allocated at}
                              (colorize @ct{a} "firebrick"))
                   (>= stage setup))
             (show (hc-append gap-size @ic{Question: are}
                              (colorize @ct{f} "darkgreen")
                              @ic{and}
                              (colorize @ct{g} "darkgreen")
                              @ic{equal?})
                   (>= stage question)))
            (show @kt{Of course!} (>= stage of-course))
            (show @kt{Good job.} (>= stage gg-dude)))
           (vl-append
            gap-size
            (show
             (hc-append gap-size
                        @ic{Abstract: both}
                        (colorize @ct{f} "darkgreen")
                        @ic{and}
                        (colorize @ct{g} "darkgreen")
                        @ic{allocated at}
                        (colorize @ct{â} "firebrick"))
             (>= stage abs-setup))
            (show
             (hc-append gap-size @ic{Question: are}
                        (colorize @ct{f} "darkgreen")
                        @ic{and}
                        (colorize @ct{g} "darkgreen")
                        @ic{equal?}
                        (pict-cond
                         [(or (= stage abs) (= stage concretization)) @ic{Well...}]
                         [(= stage ctx) (colorize @ic{Yes!} "firebrick")]
                         [(= stage ctx2) (colorize @ic{Maybe?} "olive")]))
             (>= stage follow-up))
            (hc-append (show (pdf->pict addresses-path) (>= stage abs))
                       (pict-cond #:combine lt-superimpose
                                  [(= stage concretization) (pdf->pict gamma-path)]
                                  [(= stage ctx) (pdf->pict gamma1-path)]
                                  [(= stage ctx2) (pdf->pict gammaω-path)]))))))

   (define (oneness-solution)
     (with-size 50
       (slide
        #:title (with-size 50 @kt{Solution})
        (vc-append 50
                   (hc-append gap-size @kt{Count allocations} @ic{[Might 2006]})
                   (hc-append @kt{Mediate all allocation}))))))

(module+ sections
  (require (submod ".." slide-deck))
  (provide intro-to-thesis semantics-to-aam AAM OAAM example finite pushdown small-pdcfa
           prefix
           shifting-gears AAML related redex-great)

  (define (intro-to-thesis)
    (title)
    (run-stages wonderful)
    (run-stages (terrible #f))
    (run-stages thesis-slide #:stage '(statement built measure)))

  (define (semantics-to-aam)
    (run-stages semantics)
    (run-stages aam?)
    (run-stages (grinder #t) #:stage '(static big-semantics (output . 0)))
    )

  (define (example)
    ;; We've got some time. Let's evaluate Ω.
    ;; AAM in 3 slides.
    ;; Step 1: identify allocation points.
    (run-stages concrete->abstract)
    (run-stages big-states))

  (define (finite)
    ;; I use slightly different language than orthodox, so
    ;; this isn't entirely beating a dead horse.
    ;; Step 2: finitize allocation
    ;; Step 3: join and nondeterminism
    (run-stages finitize)
    (run-stages finite-structure)
    (run-stages aam-code)
    (run-stages wins-of-aam)
    (run-stages aam-drawbacks))

  (define (AAM)
    (example)
    (finite))

  (define/staged small-pdcfa #:stages [entirety general specific]
    (ct-superimpose
     (show (with-size 50 @kt{That's it.}) (= stage entirety))
     (cc-superimpose
      (pin-over
       (pin-over (blank SCREEN-WIDTH SCREEN-HEIGHT)
                 280 -20
                 (show (rt-superimpose
                        (colorize (filled-rectangle (- SCREEN-WIDTH 300) SCREEN-HEIGHT)
                                  '(210 210 210))
                        (inset (colorize @kt{Language specific} "white") 10))
                       (= stage specific)))
       -20 -20
       (show
        (rt-superimpose
         (colorize (filled-rectangle 300 SCREEN-HEIGHT) '(160 160 160))
         (inset (colorize @kt{General} "white") 10))
        (>= stage general)))
      (pin-over-hcenter
       (blank 0)
       -80 -310
       (hc-append
        50
        (scale
         (code
          (code:comment "all transparent")
          (struct state (point σ κ))
          (struct mt ())
          (struct rt (ctx))
          code:blank
          (define F (mutable-set))
          (define R (mutable-set))
          (define Seen (mutable-set))
          (define M (make-hash))
          (define Ξ (make-hash))
          code:blank
          (define (add-state! s)
            (unless (set-member? Seen s)
              (set-add! F s)
              (set-add! Seen s)))
          code:blank
          (define (add-reduction! s0 s1)
            (set-add! R (cons s0 s1))
            (add-state! s1))
          code:blank
          (define (analyze e)
            (set-clear! F)
            (set-clear! R)
            (set-clear! Seen)
            (hash-clear! M)
            (hash-clear! Ξ)
            (define ς₀
              (state (cons e (hash))
                     (hash) (mt)))
            (set-add! F ς₀)
            (do () ((set-empty? F))
              (define ς (set-first F))
              (set-remove! F ς)
              (step ς))
            '|the final system|
            (list R M Ξ)))
         0.4)
        (scale
         (code
          (code:comment "continuation frames")
          (struct ar (e ρ))
          (struct fn (v))
          code:blank
          (code:comment "Syntax")
          (struct ref (x))
          (struct app (e0 e1))
          (struct lam (x e))
          code:blank
          (code:comment "Semantics")
          (define (step s)
            (match s
              [(state (cons (ref x) ρ) σ κ)
               (for ([v (in-set (hash-ref σ (hash-ref ρ x)))])
                 (add-reduction! s (state v σ κ)))]
              code:blank
              [(state (cons (app e0 e1) ρ) σ κ)
               (add-reduction! s (state (cons e0 ρ) σ (cons (ar e1 ρ) κ)))]
              code:blank
              [(state v σ (cons (ar e ρ) κ))
               (add-reduction! s (state (cons e ρ) σ (cons (fn v) κ)))]
              code:blank
              [(state v σ (cons (fn (cons (lam x e) ρ)) κ))
               (define a (alloc s))
               (define ρ* (hash-set ρ x a))
               (define σ* (hash-add σ a v))
               (define ctx (list e ρ* σ*))
               (hash-add! Ξ ctx κ)
               (match (hash-ref M ctx #f)
                 [#f (add-reduction! s (state (cons e ρ*) σ* (rt ctx)))]
                 [results (for ([r (in-set results)])
                            (match-define (cons v* σ**) r)
                            (add-state! (state v* σ** κ)))])]
              code:blank
              [(state v σ (rt ctx))
               (hash-add! M ctx (cons v σ))
               (for ([κ (in-set (hash-ref Ξ ctx))])
                 (add-reduction! s (state v σ κ)))])))
         0.4))))))

  (define (pushdown)
    (run-stages talk-outline #:stage 'pushdown)
    (run-stages why-not-aam)
    (run-stages fib-analogy)
    (run-stages fib-insights)
    (run-stages pd-diagram)
    (run-stages fix-aam)
    (run-stages substitutional-relevance)
    (run-stages fix-zoom)
    (run-stages pd-memo-diagram)
    (run-stages relevance)
    (run-stages relevance-useful)
    (run-stages small-pdcfa)
    (run-stages pd-results)
    (run-stages thesis-slide #:stage 'precise/performant/systematic))


  (define (OAAM)
    (run-stages talk-outline #:stage 'oaam)
    (run-stages (talk-focus #t))
    (run-stages when-lookup)
    (slide #:title (with-size 50 @kt{Evaluation})
           (vc-append (force speedup-pict)
                      (blank 100)
                      @t{No observed precision change}))
    (same-ballpark)
    (run-stages thesis-slide #:stage 'performant/systematic)
    (slide #:title (with-size 50 @kt{Drawbacks to OAAM})
           (with-size 50 @t{By-hand})
           'next
           (with-size 50 @t{Several steps to apply})
           'next
           (with-size 50 @t{Output less readable than input})))

  (define (shifting-gears)
    (slide
     (pin-under
      (vr-append gap-size
                 (with-size 60
                   (vl-append gap-size
                              (kt "“What we hope ever to do with ease,")
                              (kt "we must first learn to do with diligence.”")))
                 (with-font "Cinzel" (vr-append (t "~Samuel Johnson")
                                                (t "(no relation)"))))
      -35 -250
      (scale (bitmap samuel-path) 0.5)))
    (run-stages talk-outline #:stage 'language))

  (define (identity-in-the-abstract)
    (run-stages oneness-intro)
    (run-stages oneness-problem)
    (run-stages oneness-solution))

  (define/staged prefix #:stages [type equiv join]
    #:name 'prefix-domain 
    (define epict
      (inset
       (vl-append
        gap-size
        (ct "≡(Single s, Single s) = Yes")
        (ct "≡(_ s, _ s') | s ⊑ s' = Maybe")
        (ct "≡(_ s, _ s') | s' ⊑ s = Maybe")
        (ct "≡(⊤,_) = ≡(_,⊤) = Maybe")
        (ct "≡(_,_) = No"))
       10))
    (define jpict
      (inset
       (vl-append
        gap-size
        (ct "⊔(Single s, Single s) = Single s")
        (ct "⊔(_ s, _ s') | s ⊑ s' = Prefix s")
        (ct "⊔(_ s, _ s') | s' ⊑ s = Prefix s'")
        (ct "⊔(_,_) = ⊤"))
       10))
    (define y 400)
    (ct-superimpose
     (pin-under
      (pin-over
       (blank SCREEN-WIDTH SCREEN-HEIGHT)
       -20 y
       (show
        (colorize (filled-rectangle 512 (- SCREEN-HEIGHT y))
                  '(210 210 210))
        (>= stage equiv)))
      (- 512 20) y
      (show
       (colorize (filled-rectangle 512 (- SCREEN-HEIGHT y))
                 '(180 180 180))
       (>= stage join)))
     (vc-append (with-size 50 (kt "Example: Prefix domain"))
                (blank 70)
                (vl-append
                 gap-size
                 (ct "s ∈ PString ::= ⊤ | Single string | Prefix string")
                 (ct "γ(⊤) = String")
                 (ct "γ(Single string) = {string}")
                 (ct "γ(Prefix string) = {string}·String")
                 (ht-append
                  gap-size
                  (show epict (>= stage equiv))
                  (show jpict (>= stage join)))))))
  
  (define (AAML)
    (run-stages aaml-motivation)
    (run-stages thocon-problem)
    (run-stages thocon-example)
    (identity-in-the-abstract)
    (with-size 60
      (slide @kt{I tried by hand.}
             'next
             @kt{I failed.}
             'next
             @kt{I wrote a language to help.}))
    (run-stages lang-intro)
    (run-stages example-rule)
    (slide #:title (with-size 50 (kt "Extensibility with externals"))
           (with-size 50 @ic{Closures are nice, but what about...})
           'next
           (with-size 50 @ic{Strings? Integers? Floats? Vectors?})
           'next
           (with-size 40 (hc-append gap-size @iic{External values:} @ct{⊔, ≡, ⊑}))
           'next
           (hc-append @iic{External metafunctions, e.g., }
                      @ct{append} @ic{, } @ct{+} @ic{, } @ct{vector-ref})
           )
    (run-stages prefix)
    (with-size 60
      (slide
       @ic{Write your language as an AM}
       'next
       @ic{Give finite allocation functions.}
       'next
       @ic{Presto! Computable approximation.}))
    (run-stages thesis-slide #:stage 'precise/automatic)
    (with-size 50
     (slide #:title (with-size 50 @kt{Drawbacks of AAML})
            @ic{Current implementation slow (no OAAM)}
            'next
            @ic{Small library of external domains}
            'next
            @ic{Allocation functions are tedious.}
            'next
            @ic{But! Prototype type system can help.})))

    (define (redex-great)
      (slide (pin-over
              (pin-over (blank SCREEN-WIDTH SCREEN-HEIGHT)
                        -20 -20
                        (bitmap dino-path))
              30
              30 (inset (shadow (scale (bitmap redex-path) 0.3) 10 3) 10)
              )))


  (define (related)
    (run-stages talk-outline #:stage 'related)
    (slide #:title (with-size 60 @ic{Related Work})
           (with-size 40
             (vl-append
              gap-size
              (with-size 50 @ct{Four categories:})
              (ht-append
               gap-size
               (blank 40)
               (vl-append 10
                          @ct{Analysis implementation:}
                          (hc-append (blank 50)
                                     @ct{kCFA}
                                     (with-size 25 @ct{[Shivers 88]}))
                          @ct{Pushdown analysis:}
                          (hc-append (blank 50)
                                     @ct{CFA2}
                                     (with-size 25 @ct{[Vardoulakis,Shivers 2011]}))
                          @ct{Semantic frameworks:}
                          (hc-append (blank 50)
                                     @ct{PLT Redex}
                                     (with-size 25 @ct{[Felleisen et. al. 2009]}))
                          @ct{Analysis synthesis:}
                          (hc-append (blank 50)
                                     @ct{Matching Logic}
                                     (with-size 25 @ct{[Rosu,Stephanescu 2012]})))))))

    #|
    Specifically
    @t{Olin's kCFA}
    @t{CFA2 and PDCFA}
    @t{PLT Redex}
    @t{Matching logic (K framework)}
    |#

    ))

(module+ main
  (require (submod ".." slide-deck)
           (submod proposal/presentation slide-deck)
           (submod ".." sections))
  (set-page-numbers-visible! #t)
  (intro-to-thesis)
  (run-stages talk-outline)
  (run-stages thesis-slide #:stage 'semantics)
  (semantics-to-aam)

  (AAM)

  ;; Finitizing the stack is actually not necessary.
  (pushdown)

  (OAAM)

  (shifting-gears)

  (AAML)

  (related)
  (run-stages (terrible #t) #:stage 'crit-easy)
  (run-stages parting)

  ;; Below the fold, for private session only

  (slide (with-size 40 (ic "Proposal vs. Dissertation"))
         (ht-append gap-size
                    (vl-append (ibc "Proposal")
                               (ic "Sparseness")
                               (ic "Pushdown shift/reset")
                               (ic "Pushdown stack inspection")
                               (ic "Verify Temporal HO-contracts"))

                    (vl-append (ibc "Dissertation")
                               (ic "Cut")
                               (ic "Proved/unevaluated")
                               (ic "Proved/~evaluated")
                               (ic "Just Semantics / language"))))

  (redex-great)

  (slide #:title (with-size 60 @ic{Ext. Related Work (Implementation)})
         #:name 'rw-impl
         @t{Constraint-based analysis [Heintze 93] [Steckler,Wand 96]}
         @t{Abstract compilation [Boucher,Feeley 96]}
         @t{Structural AI (Astrée) [Blanchet el. al. 2002]})

  (slide #:title (with-size 60 @ic{Ext. Related Work (Pushdown)})
         #:name 'rw-pushdown
         @t{Neil Jones' pushdown analysis [Jones 1981]}
         @t{WPDS++ [Reps,Lal,Kidd 2007]}
         @t{HORS [Knapik et. al. 2002]})
  
  (slide #:title (with-size 60 @ic{Ext. Related Work (Semantics)})
         #:name 'rw-semantics
         @t{K framework [Serbanuta et. al. 2012]}
         @t{Term reduction systems [Klop 92]})
  
  (slide #:title (with-size 60 @ic{Ext. Related Work (Synthesis)})
         #:name 'rw-synth
         @t{Flow logic [Neilson,Neilson 98]}
         @t{Rhodium [Lerner et. al. 2005]}
         @t{PostHat and all that [Thakur et. al. 2013]})

  (slide #:title (with-size 60 @ic{Future work})
         #:name 'future
         @ic{Synthesize allocators: type system identifies recursion}
         @ic{Explicit allocation: adjunction type}
         @ic{OAAM techniques}
         @ic{Linguistic support for pushdown}
         @ic{Abstraction testing}
         'next
         @ic{Black holes?})
  )

(module+ test
  (require (submod ".." slide-deck)
           (submod ".." sections))

;  (run-stages pd-diagram)

  (run-stages wonderful)
;  (finite)
  )
