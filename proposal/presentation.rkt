#lang at-exp slideshow
(require unstable/gui/slideshow
         racket/splicing
         pict/code
         slideshow-helpers/picts
         unstable/gui/ppict
         syntax/parse/define
         slideshow/flash
         slideshow/play
         file/convertible
         racket/gui/base
         scheme/runtime-path
         "radar.rkt"
         (only-in plot/utils polar->cartesian)
         (only-in plot/private/common/draw pen-colors))

(define logo-path (collection-file-path "prl-logo.png" "talk-utils"))
(define-runtime-path jail-path "jail.png")
(define-runtime-path horse-path "horse-macro.png")
(define-runtime-path prohibited-path "prohibited.pdf")
(define-runtime-path prohibited-png-path "prohibited.png")
(define check-path (collection-file-path "checkmark.jpg" "talk-utils")) ;; 196 x 481
(define xmark-path (collection-file-path "xmark.png" "talk-utils")) ;; 738 x 488

(define-runtime-path js-path "logo_JavaScript.png")
(define-runtime-path racket-path "racket-logo.png")
(define-runtime-path dalvik-path "dalvik-logo.jpg")
(define-runtime-path erlang-path "erlang-logo.png")
(define-runtime-path coq-path "coq_logo.png")
(define bg-slide-assembler
  (lambda (title sep content)
    (inset content (- margin) (- margin) 0 0)))

(define use-pdf? #f)
(define (scale-pdf-or-png pdf-path factor png-path)
  (cond [use-pdf?
         (define pict
           (scale ((dynamic-require (collection-file-path "poppler-main.rkt" "talk-utils") 'page->pict)
                   pdf-path)
                  factor))
         (with-output-to-file
             prohibited-png-path #:exists 'replace
             (λ () (write-bytes (convert (pict->bitmap pict) 'png-bytes))))
         pict]
        [else (bitmap png-path)]))

(current-main-font "Linux Libertine Capitals O")

(module+ pict-utils
  (provide frame* arrows-back-and-forth tagged-shadow-frame)
  (define (frame* box #:color [color "black"] #:show [which '(l r t b)])
    (define w (pict-width box))
    (define h (pict-height box))
    (define vert (colorize (vline 0 h) color))
    (define horz (colorize (hline w 0) color))
    (lb-superimpose
     (rt-superimpose
      (lt-superimpose box
                      (show vert (memv 'l which))
                      (show horz (memv 't which)))
      (show vert (memv 'r which)))
     (show horz (memv 'b which))))
  (define (arrows-back-and-forth base left right)
    (pin-arrow-line 10
                    ;; bottom arrow
                    (pin-arrow-line 10 base
                                    left cb-find right cb-find
                                    #:start-pull 0.5 #:end-pull 0.5
                                    #:start-angle (* 3/2 pi) #:end-angle (* 1/2 pi))
                    ;; top arrow
                    right ct-find left ct-find
                    #:start-pull 0.5 #:end-pull 0.5
                    #:start-angle (* 1/2 pi) #:end-angle (* 3/2 pi)))
  (define (tagged-shadow-frame base top #:margin [frame-margin 20] #:sep [frame-sep 5])
    (define top-frame
      (frame*
       (cc-superimpose
        (colorize (filled-rectangle (pict-width top) (pict-height top)) "white")
        top)
       #:color "gray" #:show '(l t r)))
    (pin-over
     (shadow-frame base #:margin frame-margin #:sep frame-sep)
     (sub1 (+ frame-sep frame-margin)) (+ (- (pict-height top-frame)) frame-margin 1)
     top-frame)))

(module+ slide-deck
  (require slideshow-helpers/slide
           (submod icfp2013-talk/semantics slide-deck)
           icfp2013-talk/color-scheme
           (only-in (submod hopa2013-talk/hopa2013 slide-deck) concrete)
           (only-in icfp2013-talk/pict-helpers
                    join-one braces nstruct production expr call tuple ntuple parens)
           (submod ".." pict-utils))
  (provide run-talk)
  (define (title)
    (parameterize ([current-slide-assembler bg-slide-assembler])
      (slide
       (cc-superimpose
        (bitmap logo-path)
        (vc-append
         (big (bold (para #:align 'center "Systematic Constructions for")))
         (big (bold (para #:align 'center "Higher-Order Program Analyses")))
         (blank-line)
         (para #:align 'center @bt{J. Ian Johnson})
         (blank-line)
         (t "Thesis Proposal")
         (blank-line)
         (t "Northeastern University")
         (blank-line)
         (small (t "2013 November 18")))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Why HOPA?

  (define (why-hopa)
    (slide (big @t{Why care about HOPA?}))
    (run-stages type-jail)
    (run-stages universal)
    (slide (big @t{HOPA is a class of})
           @item{online,}
           @item{computable,}
           @item{abstract interpretations}
           @item{of higher-order languages}))

  (define/staged type-jail #:num-stages 3
    (define base-types (with-size 150 (t "Types")))
    (define type-jail (cc-superimpose base-types (bitmap jail-path)))
    (define strikeout
      (scale-pdf-or-png prohibited-path 2.5 prohibited-png-path))
    (define type-strikeout (cc-superimpose base-types strikeout))
    (match stage
      [0 base-types]
      [1 type-jail]
      [2 type-strikeout]))

  (define/staged universal #:stages [text collage]
    (define base (big @t{HOPA is universal}))
    (define (g p) (show p (>= stage collage)))
    ;; Example languages (Erlang, JS, Dalvik, Coq, Racket)
    (pin-over
     (pin-over
      (pin-over
       (pin-over
        (pin-over base
                  500 -40
                  (g (bitmap coq-path)))
        -200 170
        (g (scale (bitmap erlang-path) 0.5)))
       200 70
       (g (bitmap dalvik-path)))
      160 -350
      (g (bitmap racket-path)))
     -120 -75
     (g (scale (bitmap js-path) 0.1))))

  (define/staged useful
    #:stages [base anim to-name-a-few]
    #:anim-at anim
    #:skip-first
    #:steps 30
    (define big-font 28)
    (define base-pict (big @t{HOPA is useful}))
    (define (vlappend-vec v) (apply vl-append 3 (vector->list v)))
    (define (anim-fn n)
      (with-size 16
        (define superβ @t{(Super-beta) inlining})
        (define constant-prop @t{Constant propagation & folding})
        (define safety @t{Safety test removal})
        (define global @t{Globalization})
        (define arity @t{Arity-raising})
        (define unbox @t{Unboxing})
        (define future @t{Future optimization})
        (define uve @t{Useless variable elimination})
        (define ive @t{Induction variable elimination})
        (define vect @t{Vectorization})
        (define fusion @t{Loop fusion})
        (define strict @t{Strictness promotion})
        (define lazy @t{Laziness refactoring})
        (define strength @t{Strength reduction})
        (define opts (vector superβ constant-prop
                             safety global arity unbox future uve ive vect fusion strict lazy strength))
        (define opt-base
          (vl-append (with-size big-font (colorize @t{Optimizations} "firebrick"))
                     (shadow-frame (ghost (vlappend-vec opts)))))
        (define race @t{Data race detection})
        (define ifl @t{Information flow for security})
        (define termination @t{Termination analysis})
        (define complexity @t{Complexity analysis})
        (define dependence @t{Dependence analysis})
        (define inference @t{Type inference})
        (define shapes @t{Shape analysis})
        (define understandings (vector race ifl termination complexity dependence inference shapes))
        (define understanding-base
          (vl-append (with-size big-font (colorize @t{Program Understanding} "medium forest green"))
                     (shadow-frame (ghost (vlappend-vec understandings)))))
        (define temporal @t{Temporal logic model-checking})
        (define contract @t{Behavioral contract validity})
        (define verifications (vector temporal contract))
        (define verification-base
          (vl-append (with-size big-font (colorize @t{Verification} "steel blue"))
                     (shadow-frame (ghost (vlappend-vec verifications)))))
        (define fader (λ (p n) (fade-pict n (ghost p) p)))
        (define scale0 (chopped-interval-scale 0 2/5))
        (define scale1 (chopped-interval-scale 1/3 4/5))
        (define scale2 (chopped-interval-scale 2/3 1))
        (cc-superimpose
         base-pict
         (hc-append
          20
          ((slide-and-compose opt-base opts superβ fader)
           (scale0 n))
          ((slide-and-compose (fader understanding-base (scale0 n)) understandings race fader)
           (scale1 n))
          ((slide-and-compose (fader verification-base (scale1 n)) verifications temporal fader)
           (scale2 n))))))
  
    (match stage
      [(== base) (cc-superimpose base-pict (ghost (anim-fn 0.0)))]
      [(== anim) anim-fn]
      [(== to-name-a-few)
       (cc-superimpose (anim-fn 1.0) 
                       (shadow-frame (big @t{To name a few})))]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; What's wrong with HOPA?

  (define (whats-wrong)
    (slide (big (t "What's wrong with HOPA?")))
    (run-stages intrinsic/extrinsic)
    (run-stages temporal)
    ;; AAAaaand thesis
    (run-stages thesis-slide))

  (define/staged intrinsic/extrinsic
    #:stages [intrinsic extrinsic 0CFA HORS JPF AAM mine]
    #:title (with-size 42
              (hc-append (t "Intrinsically") (show (t " and Extrinsically") (>= stage extrinsic))))
    (define δ (/ (* -2 pi) 6))
    (define θs (make-vector 6))
    (define extent 300)
    (for/fold ([p 0+0i]) ([i 6])
      (vector-set! θs i (* i δ)))
    (define-values (point-l point-t)
      (for/fold ([l +inf.0] [t +inf.0]) ([θ (in-vector θs)])
        (match-define (vector x y) (polar->cartesian θ extent))
        (values (min l x) (min t y))))
    (define points (for/list ([θ (in-vector θs)])
                     (match-define (vector x y) (polar->cartesian θ extent))
                     (list (- x point-l) (- y point-t))))
    (define qualities (append (list @t{Sound} @t{Fast} @t{Precise})
                              (for/list ([q (in-list (list @t{Maintainable} @t{Design Ease} @t{Grokable}))])
                                (show q (>= stage extrinsic)))))
    (define graphs
      `(("Set-based 0CFA" ,0CFA .
         #((sound . 5) (fast . 4) (precise . 2) (maintainable . 2) (design . 2) (grokable . 3)))
        ("HORS" ,HORS .
         #((sound . 4) (fast . 3) (precise . 4) (maintainable . 2) (design . 1) (grokable . 1)))
        ("JPF" ,JPF .
         #((sound . 2) (fast . 3) (precise . 4) (maintainable . 3) (design . 3) (grokable . 2)))
        ("AAM" ,AAM .
         #((sound . 5) (fast . 1) (precise . 3) (maintainable . 4) (design . 5) (grokable . 5)))
        ("Mine" ,mine .
         #((sound . 5) (fast . 3.75) (precise . 3.75) (maintainable . 4.25) (design . 5) (grokable . 5)))))
    (define dimensions '(sound fast precise maintainable design grokable))
    (pin-over
     (pin-over
      (pin-under
       (panorama
        (for/fold ([p (blank)])
            ([point (in-list points)]
             [quality (in-list qualities)]
             [dim (in-list dimensions)])
          (match-define (list x y) point)
          (define x-offset
            (if (eq? dim 'maintainable)
                (- 50)
                0))
          (pin-over p (+ x x-offset) y quality)))
       225 80
       (show (bitmap
              (send
               (radar-plot (take (map cddr graphs) (max 1 (- stage extrinsic)))
                           #:dimensions dimensions)
               get-bitmap))
             (>= stage 0CFA)))
      -110 -40
      (show
       (shadow-frame
        (apply vl-append gap-size
               (for/list ([g (in-list (take graphs 4))]
                          [i (in-naturals 1)])
                 (show (colorize (t (car g)) (vector-ref pen-colors i))
                       (>= stage (cadr g))))))
       (>= stage 0CFA)))
     -110 360
     (show (colorize (big (t "My work")) (vector-ref pen-colors 5))
           (>= stage mine))))

  (define-syntax-rule (in-sawasdee . body) (parameterize ([current-main-font "Sawasdee"]) . body))

  (define thesis-pict
    (parameterize ([current-font-size 30])
      (in-sawasdee
       (vc-append
        gap-size
        (hc-append (tag-pict (hc-append (tag-pict (t "Precise ") 'precise) (t "and ") (tag-pict (t "performant") 'perf)) 'measure)
                   (t " analyses for higher-order languages"))
        (hc-append (t "can be ") (tag-pict (t "systematically constructed") 'systematic)
                   (t " from their semantics."))))))

  (define/staged thesis-slide #:stages [statement propose-build propose-measure]
    (with-size 30
      (in-sawasdee
       (define ((hilight b) p)
         (show (colorize (filled-rectangle (pict-width p) (pict-height p)) "yellow") b))
       (define bg
         (tagged-shadow-frame
          (pin-under-tag
           (pin-under-tag 
            thesis-pict lt-find 'systematic (hilight (>= stage propose-build)))
           lt-find 'measure (hilight (= stage propose-measure)))
          (inset (big (bt "Thesis:")) 10)))
       (define build-text (big (t "I propose to build and prove")))
       (define build-arrow
         (pin-arrow-line 15
                         (pin-over bg -30 300 build-text)
                         build-text ct-find
                         (first (find-tag bg 'systematic)) cb-find
                         #:start-angle (* 1/3 pi)
                         #:end-angle (* 2/3 pi)))
       (match stage
         [(== statement) bg]
         [(== propose-build) build-arrow]
         [(== propose-measure)
          (define measure-text (big (t "I propose to evaluate")))
          (pin-arrow-line 15
                          (pin-over build-arrow 100 -200 measure-text)
                          measure-text cb-find
                          (first (find-tag bg 'measure)) ct-find
                          #:start-angle (* -1/3 pi)
                          #:end-angle (* -1/2 pi))]))))

  (define/staged temporal #:stages [question horse hors jail]
    (define base (big @t{Temporal properties?}))
    (define good-bad
      (shadow-frame (tabular (list (big @t{Simply-typed tree grammar}) (scale (bitmap check-path) 0.2))
                             (list (big @t{Anything else}) (scale (bitmap xmark-path) 0.2)))))
    (match stage
      [(== question) base]
      [(== horse) (cc-superimpose base (bitmap horse-path))]
      [(== hors) good-bad]
      [(== jail) (cc-superimpose good-bad (bitmap jail-path))]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; What have I done for HOPA?

  (define (what-done)
    (slide (big (t "What have I done for HOPA?")))
    (run-stages what-I-did)
    (run-stages essence-slogan)
    (parameterize ([current-main-font "Andale Mono"])
     (run-stages concrete))

    (parameterize ([use-color? #f])
      (slide (a-state))
      (play (λ (t) (CESK-table sfade t)))
      (play (λ (t) (CESK-table smove t)))
      (slide (CESK-table smove 1.0))
      (slide (CESK-table sCESKM))
      (slide (CESK-table sCESKM-ret))
      (slide (CESK-table sCESKMΞ)))

    (run-stages summaries-cfa2)

    (slide (big (t "Recipe for first-class control"))
           'next
           (hc-append @ctt{κ} (t " as values (storeable)")))

    (run-stages kont-values-problem)

    (define γ (big (ctxtt "\u03b3")))
    (define hat (big (ctxtt " \u0302")))
    (define γ̂
     (panorama
      (pin-over γ (- (/ (pict-width γ) 2) (/ (pict-width hat) 2)
                         14 #| icky fudge factor |#)
                3 #| another icky fudge factor |# hat)))
    
    (slide (big (t "New environment ‶closes″ heap"))
           (big (hc-append (t "Control closure ") @tt{χ} (t " : ") (addr @t{Addr}) @tt{ → } (t "℘(Store)")))
           'next
           (big (t "Context = CContext ∪ SContext"))
           (big (production (hc-append γ (t " ∈ CContext")) (tuple (expr @tt{e}) @idtt{ρ} @σtt{σ} @tt{χ})))
           (big (production (hc-append γ̂ (t " ∈ SContext")) (tuple (expr @tt{e}) @idtt{ρ} (addr @tt{a})))))
    ;; how 1st class control generalizes CFA2 (well, not /how/, but describes the mechanism)
    (run-stages big-jump))

  (define/staged big-jump #:stages [big-rule the-reveal]
    (define base
      (vc-append gap-size
                 (big (t "In the context of"))
                 (big (ntuple (expr @tt{e}) @idtt{ρ} @σtt{σ} @tt{χ} @ctt{κ} @Mtt{M} @Ξtt{Ξ}))
                 (big (t "Interpret"))
                 (tuple (expr @tt{e′}) @idtt{ρ′} (addr @tt{a}))
                 (big (t "as"))
                 (braces (tuple (expr @tt{e′}) @idtt{ρ′} @σtt{σ′} @tt{χ′}) @tt{ ∈ } (call @tt{dom} @Ξtt{Ξ})
                         @tt{ : } @σtt{σ′} @tt{ ∈ } (call @tt{χ} (addr @tt{a})) @t{ and }
                         @tt{χ′ ⊑ χ})))
    (match stage
      [(== big-rule) base]
      [(== the-reveal)
       (cc-superimpose
        base
        (tagged-shadow-frame (hc-append @t{CFA2 + } @tt{call/cc } (citation "ICFP 2011")
                                        @t{ is a special case})
                             (inset (shadow (colorize (big (t "Surprise!")) "firebrick") 10 5) 10)))]))
  
  (define/staged kont-values-problem #:stages [see arrows solution]
    (define store-pict
      (big (join-one (tag-pict @σtt{σ} 'first)
                     @idtt{a}
                     (braces
                      (nstruct "rt" (idtt "e") (idtt "ρ") (tag-pict (pict-if (= stage solution)
                                                                             (colorize @tt{b} "medium forest green")
                                                                             @σtt{σ′}) 'second))))))
    (vc-append
     gap-size
     (big (t "Problem:"))
     (pict-if (= stage arrows)
              (arrows-back-and-forth store-pict (find-tag store-pict 'first) (find-tag store-pict 'second))
              store-pict)
     (blank 50)
     (show (big (t "AAM: break circularity with indirection")) (= stage solution))))

  (define (citation txt #:size [size 18])
     (colorize (with-size size (t (format "[~a]" txt))) "gray"))

  (define/staged what-I-did #:stages [all built-and-evaluated focus]
    (define built-pict
      (in-sawasdee (show (with-size 24 (t "Built, proved and evaluated: 1000x speed-up"))
                         (= stage built-and-evaluated))))
    (define base
      (pin-over
       (cc-superimpose
        (vl-append
         gap-size
         (colorize @t{Done:} (if (< stage focus) "medium forest green" "gray"))
         (tag-pict
          (colorize-if (>= stage focus)
                       @item[@t{Systematic optimizations } (citation "ICFP 2013")]
                       "lightgray")
          'built)
         (colorize @t{Almost done (not yet true) :} (if (< stage focus) "steel blue" "gray"))
         ;; Not going to talk about 1NSAs
         (colorize-if (>= stage focus)
                      @item[@t{Abstract model of stack introspection } (citation "JFP best of ICFP 2012")]
                      "lightgray")
         @item[@t{Systematic summarization } (citation "HOPA workshop 2013")]
         (colorize @t{Work in progress:} (if (< stage focus) "firebrick" "gray"))
         @item{Temporal reasoning through contracts}))
       300 -30
       built-pict))
    ;; Put "slogan" in a frame without a bottom so we can make it look like
    ;; it's part of the shadow frame containing the slogan
    (pin-arrow-line
     15
     base
     built-pict lc-find
     (first (find-tag base 'built)) (λ (p path) (define-values (x y) (ct-find p path)) (values (- x 50) y))
     #:alpha (if (= stage built-and-evaluated) 1 0)
     #:hide-arrowhead? (not (= stage built-and-evaluated))
     #:start-angle pi
     #:start-pull 1.2
     #:end-angle (* -1/3 pi)))

  (define/staged essence-slogan #:stages [items slogan slogan-zoom]
    (define frame-margin 20)
    (define frame-sep 5)
    (define the-slogan
      (tagged-shadow-frame
       (hc-append
        (t "Summarization is ")
        (tag-pict
         (colorize-if (>= stage slogan-zoom) (t "context-sensitive") "medium forest green")
         'context)
        (t " memoization"))
       (inset (shadow (big (colorize @t{Slogan} "steel blue")) 10 5) 10)
       #:margin frame-margin
       #:sep frame-sep))
    (define some-contexts
      (vl-append @t{Heap}
                 @t{Stack root addresses}
                 @t{Continuation marks}
                 @t{Temporal monitor state}))
    (define framed-contexts (shadow-frame some-contexts))
    (define base
      (vc-append gap-size
                 (big (t "Systematic summarization"))
                 (vl-append gap-size
                            @item{AAM-style construction}
                            @item{Rederives (polyvariant) CFA2}
                            @item{Allows stack-inspection and GC}
                            @item{New look at first-class control})))

    (pin-over-hcenter
     base
     (/ (pict-width base) 2) -50     
     (show
      (pin-arrow-line
       15
       (pin-over the-slogan 490 200 (show framed-contexts (>= stage slogan-zoom)))
       framed-contexts
       ;; get at the bottom-right of the frame and not the drop-shadow
       (λ (pict path)
          (define-values (x y) (lt-find pict path))
          (values (+ x frame-sep frame-margin)
                  (+ y frame-margin)))
       (find-tag the-slogan 'context)
       cb-find
       #:alpha (if (>= stage slogan-zoom) 1 0)
       #:hide-arrowhead? (not (>= stage slogan-zoom))
       #:start-angle (* 3/4 pi)
       #:end-angle (* 1/2 pi))
      (>= stage slogan))))

  (define/staged summaries-cfa2 #:stages [cfa2 stack-inspection]
    (rb-superimpose
     (cc-superimpose
      (blank 900 700)
      (vc-append
       gap-size
       (big (hc-append (t "Share ") @σtt{σ} @t{, } @Mtt{M} (t " and ") @Ξtt{Ξ} (t ",")))
       (big (t "Get CFA2 without stack allocation"))    
       (blank 50)
       (show
        (big (hc-append(t "Stack inspection: include more in ") @ctxtt{ctx}))
        (= stage stack-inspection))))
     (citation "Essence of summarization" #:size 20)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; What do I propose to do for HOPA?

  (define (what-do)
    (run-stages proposal)
    (run-stages temporal-contracts)
    (run-stages TC-example)
    (run-stages TC-semantics)
    (run-stages TC-negation)
    (run-stages TC-abstract))

  (define/staged proposal #:stages [what-do case-study slogan]
    (cc-superimpose
     (vc-append gap-size
                (big (t "What I propose to do for HOPA"))
                (blank 50)
                (show (big (t "Case study: Temporal higher-order contracts")) (>= stage case-study)))
     (show (tagged-shadow-frame
            (big (vl-append 5 (t "Abstract runtime monitoring")
                            (t "is software model-checking")))
            (shadow (colorize (inset (big @t{Slogan}) 10) "steel blue") 10 5)) (= stage slogan))))

  (define/staged temporal-contracts #:stages [grammar expression re-entrance] #:title "What is a Temporal Contract?"
    (vl-append gap-size
               (production (t "T") (t "T ∪ T") (t "T ∩ T") (t "¬ T") @tt{Any} @tt{Fail}
                           (t "T · T") (t "T*") (t "ε") (t "pat") (t "〈pat〉T"))
               (production (t "pat") (tt "v") (t "c(pat, …)") (hc-append (tt "!") (t "pat"))
                           (call @tt{ref} @tt{x}) #;(tt "(== x)")
                           (call @tt{bind} @tt{x}) #;(tt "x")
                           @tt{_})
               (production (t "c") (tt "call") (tt "return") (tt "cons") (t "…"))
               (blank-line)
               (show (hc-append (t "Or some nicer language of patterns, ") (it "e.g., ") (t "Racket's"))
                     (>= stage expression))
               (show (hc-append (code (? contracted-fn)) (t " forbid monitor re-entrance"))
                     (>= stage re-entrance))))

  (define/staged TC-example #:stages [fn scontract tcontract-upto tcontract-full]
    #:title "Reader/writer example"
    (define to-negate
      (with-size 20
        (show (code (· ...
                       〈(return c-f (list (bind read) (bind write) (bind close)))〉
                       (· (* (! (return (ref close) _)))
                          (call (ref close))
                          ...
                          #,(show (code (∪ (call (ref read)) (call (ref write) _) (call (ref close))))
                                  (>= stage tcontract-full)))))
              (>= stage tcontract-upto))))
    (define neg (with-size 20
                  (if (<= stage tcontract-upto)
                      (vl-append 2
                                 (ghost (tt "(¬"))
                                 (hc-append (ghost (tt "(")) to-negate (ghost (tt ")"))))
                      (code (¬
                             #,to-negate)))))
    (vc-append gap-size               
               (with-size 20
                (code (define (f path)
                        (define-values (i o) (open-input-output-port path))
                        (list (thunk (read i))
                              (λ (d) (write d o))
                              (thunk (close-input-port i)
                                     (close-output-port o))))
                      ;; TODO: Stage, use sugar for define, shared
                      #,(show
                         (code
                          (rec/c c-f
                            (tmon 'pos 'neg 'contract
                                  (path? . -> . (list/c (-> any/c) (any/c . -> . void?) (-> void?)))
                                  #,neg
                                  f)))
                         (>= stage scontract))))))

  (define (deriv E T #:call? [call? #f])
    (big (hc-append (hb-append (t "∂") (small (t E))) (if call? (parens (t T)) (t T)))))

  (define/staged TC-semantics #:stages [meaning semantics]
    (big
     (vc-append gap-size
                (t "Each ‶event″ steps the temporal contract")
                (hc-append (show (t "⟦") (= stage semantics)) (deriv "E" "T") (show (t "⟧") (= stage semantics))
                           (show (hc-append
                                  @t{ = }
                                  (braces (t "π : Eπ ∈ ⟦T⟧")))
                                 (= stage semantics)))
                (t "∂ standard except matching and negation"))))

  (define (list-pict-if guard then else)
    (let build ([ghosted (if guard else then)] [picts (if guard then else)])
      (match* (ghosted picts)
        [('() '()) '()]
        [((cons g gs) (cons p ps)) (cons (cc-superimpose (ghost g) p) (build gs ps))]
        [((? pair?) '()) (map ghost ghosted)]
        [('() (? pair?)) picts])))

  (define/staged TC-negation #:stages [problematic possible good prefixes problem solution derivative
                                               characterizing interesting]
    #:title (big @t{Negation is problematic})
    (define e-meaning
      (hc-append (t "⟦") @tt{e} (t "⟧")))
    (define neg-lhs (big (t "⟦¬ T⟧ = ")))
    (define bad-neg (big (list @t{Traces ∖ ⟦T⟧ ? } (citation "ICFP 2011"))))
    (define good-neg (big (list (braces @t{ε})
                                @t{ ∪ }
                                (braces @t{π : ∀ π′ ∈ F⟦T⟧∖{ε}. π′ ⋢ π}))))
    (big
     (apply vc-append
            gap-size
            (show
             (if (and (<= possible stage) (< stage solution))
                 (hc-append neg-lhs (lc-superimpose
                                     (blank (pict-width (apply hc-append good-neg)) 1)
                                     (apply hc-append bad-neg)))
                 (hc-append neg-lhs (lc-superimpose
                                     (blank (pict-width (apply hc-append bad-neg)) 1)
                                     (apply hc-append good-neg))))
             (>= stage possible))
            (append
             (list
              (show (hc-append (deriv "E" "¬ T" #:call? #t)
                               @t{ = }
                               (call @t{ν} (deriv "E" "T"))
                               @t{ → }
                               @tt{Fail}
                               @t{, } (call @t{¬} (deriv "E" "T"))) (>= stage derivative))) 
             (list-pict-if
              (and (<= good stage) (< stage solution))
              (list
               (pict-if #:combine cc-superimpose (= stage good)
                        (hc-append e-meaning (t " ∈ ⟦T⟧"))
                        (hc-append (call @t{prefixes} e-meaning) (t " ⊆ prefixes(⟦T⟧)")))
               (show (hc-append (it "e.g. ") (t "E ∈ prefixes(⟦¬ E⟧)")) (= stage problem)))
              (list
               (show (t "⟦¬ ¬ T⟧ = {ε} ∪ {Aπ : A ∈ F⟦T⟧}") (>= stage characterizing))
               (show (t "⟦¬ ¬ ¬ T⟧ = {ε} ∪ {Aπ : A ∉ F⟦T⟧}") (>= stage characterizing))))
             (list
              (show (t "Interesting consequence:") (= stage interesting))
              (show (t "⟦¬ ¬ T⟧ ≠ ⟦T⟧ but ⟦¬ ¬ ¬ ¬ T⟧ = ⟦¬ ¬ T⟧") (= stage interesting)))))))

  (define/staged TC-abstract #:stages [first second]
    (vl-append gap-size
               (big @t{Problems remaining in TC analysis:})
               @item{Abstract derivatives}
               @subitem{Abstract matching}
               @subitem{Precise identification (μ, Γ)}
               @subitem{Weak reference semantics}
               (show
                (vl-append gap-size
                           @item{State explosion}
                           @subitem{Per-state stores exponential}
                           @subitem{Solution? Summarization → Sparseness})
                (= stage second))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; What's the related work?

  (define (what-related)
    (slide (big (t "Related Work")))
    (slide #:title "Sound analysis performance engineering"
           (big (vl-append gap-size
                 @item[@t{Astrée } (citation "FMSD 2009")]
                 @item[@t{Sparrow/Airac } (citation "ASPLAS 2009/2011, SPE 2010, VMCAI 2011, PLDI 2012")])))
    (slide #:title "Systematic analysis constructions"
           (vc-append gap-size
                      (big (vl-append gap-size
                                      @item[@t{AAM } (citation "ICFP 2010")]
                                      @item[@t{Calculational approach } (citation "SAS 2008")]
                                      @item[@t{Calculational design } (citation "Marktoberdorf 1998")]
                                      @item[@t{Pretty-big-step Certified AI} (citation "JFLA 2014")]))))
    (slide #:title "Pushdown analysis"
           (vc-append
            gap-size
            (colorize (t "Higher-order:") "steel blue")
            (big (vl-append gap-size
                            @item[@t{(introspective) PDCFA } (citation "Scheme Workshop 2010, ICFP 2012")]
                            @item[@t{CFA2 } (citation "ESOP 2010, ICFP 2011")]
                            @item[@t{HORS } (citation "LICS 2006, PPDP 2009, POPL 2009, PLDI 2011, ...")]))
            (colorize (t "First-order:") "firebrick")
            (big (vl-append gap-size
                        @item[@t{LTL with regular valuations } (citation "TACS 2001")]
                        @item[@t{Weighted pushdown automata } (citation "SAS 2003, CAV 2006")]))))
    (slide #:title "Temporal properties"
           (vc-append gap-size
            (colorize (t "Dynamic:") "firebrick")
            (big (vl-append gap-size
                            @item[@t{J-LO } (citation "RV 2005")]
                            @item[@t{Tracematches } (citation "OOPSLA 2005")]
                            @item[@t{Temporal contracts } (citation "ICFP 2011")]))
            (colorize (t "Static:") "steel blue")
            (big (vl-append gap-size
                            @item[@t{Cecil } (citation "Software Engineering 1990")]
                            @item[@t{JPF-LTL } (citation "Google summer of code 2010")])))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; When do I propose to do all this crap?

  (define (timeline)
    (slide
     (big @t{When will this happen?})
     (table 3
            (append
             (list @bt{Project} @bt{Date range} @bt{Time})
             (with-size 18
               (list
                @t{Summarization} @t{Proposal - Feb 15} @t{~1.5 mo. (1 mo. paternity leave)}
                @t{Temporal contracts} @t{Feb 15 - Apr 15} @t{2 mo.}
                @t{Stack inspection} @t{Proposal - Apr 15} @t{parallel}
                @t{Writing} @t{Apr 15 - Aug 15} @t{4 mo.}))
             (list @bt{Total} (blank) @bt{7 mo.}))
            lc-superimpose
            cc-superimpose
            gap-size
            gap-size)
     (blank 10)
     (small @t{Committee reads for a while, I make a few slides...})
     (blank 30)
     @t{Defend in September 2014}))

  (define/staged outline #:stages [bullets joke] #:title "Talk Outline"
    (vl-append gap-size
     @item{What have I done?}
     @item{What will I do?}
     @item{What have others done?}
     @item{What is my timeline?}
     (blank-line)
     (show (t "What is the capital of Assyria?") (= stage joke))))

  (define (run-talk [sections '(intro/why useful wrong outline done do related timeline/wrapup)])
    (when (memv 'intro/why sections)
      (title)
      (why-hopa))
    (when (memv 'useful sections) (run-stages useful))
    (when (memv 'wrong sections) (whats-wrong))
    (when (memv 'outline sections) (run-stages outline))
    (when (memv 'done sections) (what-done))
    (when (memv 'do sections) (what-do))
    (when (memv 'related sections) (what-related))
    (when (memv 'timeline/wrapup sections)
      (timeline)
      (slide (pin-over-hcenter
              thesis-pict
              (/ (pict-width thesis-pict) 2) 300
              (parameterize ([current-main-font "Respective Slanted"])
               (with-size 110 (t "Thank You"))))))))

(module+ main
  (require (submod ".." slide-deck))
  (void (run-talk)))