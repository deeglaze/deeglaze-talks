#lang at-exp slideshow
(require unstable/gui/slideshow
         racket/splicing
         pict/code
         slideshow-helpers/picts
         (for-syntax racket/private/norm-define racket/list)
         "../utils/poppler-main.rkt"
         unstable/gui/ppict
         syntax/parse/define
         slideshow/flash
         slideshow/play
         racket/gui/base
         scheme/runtime-path)

(define-runtime-path logo-path "../utils/prl-logo.png")
(define-runtime-path jail-path "jail.png")
(define-runtime-path horse-path "horse-macro.png")
(define-runtime-path prohibited-path "prohibited.pdf")
(define-runtime-path check-path "../utils/checkmark.jpg") ;; 196 x 481
(define-runtime-path xmark-path "../utils/xmark.png") ;; 738 x 488

(define-runtime-path js-path "logo_JavaScript.png")
(define-runtime-path racket-path "racket-logo.png")
(define-runtime-path dalvik-path "dalvik-logo.jpg")
(define-runtime-path erlang-path "erlang-logo.png")
(define-runtime-path coq-path "coq_logo.png")
(define bg-slide-assembler
  (lambda (title sep content)
    (inset content (- margin) (- margin) 0 0)))

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

(module+ stages
  (provide define/staged run-stages)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Convenience syntax for defining staged slides
  (struct anim-info (which-stage skip-first? skip-last? steps delay name layout))
  (struct staged-slide (stage->pict title num-stages animation))
  (define-syntax (define/staged stx)
    (syntax-parse stx
      [(_ header (~or (~once (~or (~seq #:num-stages num:expr) (~seq #:stages [stage-names:id ...])))
                      (~optional (~seq #:title title))
                      ;; TODO?: allow more than one animation in a staged fn?
                      (~optional (~seq #:anim-at anim-at:expr))
                      (~optional (~and #:skip-first? skip-first))
                      (~optional (~and #:skip-last? skip-last))
                      (~optional (~seq #:steps steps:expr))
                      (~optional (~seq #:delay delay:expr))
                      (~optional (~seq #:name name:expr))
                      (~optional (~seq #:layout layout:expr))) ...
                      body ...+)
       #:fail-unless (if (or (attribute skip-first)
                             (attribute skip-last)
                             (attribute steps)
                             (attribute delay))
                         (attribute anim-at)
                         #t)
       "Can only be given when an animation"
       (define num-stages* (if (attribute num)
                               #'num
                               (length (syntax->list #'(stage-names ...)))))
       (define-values (id rhs) (normalize-definition #'(define header body ...) #'lambda #t #f))
       (with-syntax ([num-stages num-stages*])
         (quasisyntax/loc stx 
           (splicing-let-values (#,@(if (attribute stage-names)
                                        #`([(stage-names ...) (values #,@(range num-stages*))])
                                        #'()))
             (define #,id 
               (staged-slide
                (letrec ([#,id #,rhs]) #,id)
                #,(if (attribute title) #'title #'#f)
                num-stages
                #,(if (attribute anim-at)
                      #`(anim-info anim-at
                                   #,(syntax? (attribute skip-first))
                                   #,(syntax? (attribute skip-last))
                                   #,(if (attribute steps) #'steps #'10)
                                   #,(if (attribute delay) #'delay #'0.05)
                                   #,(cond [(attribute name) #'name]
                                           [(attribute title) #'title]
                                           [else #'#f])
                                   #,(if (attribute layout) #'layout #''auto))
                      #'#f))))))]))

  (define/match (run-stages v)
    [((staged-slide fn title num anim))
     (define simple (λ (i) (slide #:title title (fn i))))
     (define do
       (match anim
         [(anim-info at-stage skip-first? skip-last? steps delay name layout)
          (λ (i) (cond [(= i at-stage)
                        (play (fn i)
                              #:steps steps
                              #:delay delay
                              #:name name
                              #:layout layout
                              #:skip-first? skip-first?
                              #:title title)
                        (unless skip-last? (slide ((fn i) 1.0)))]
                       [else (simple i)]))]
         [_ simple]))
     (for ([i num]) (do i))]))

(module+ slide-deck
  (require (submod ".." stages)
           (submod "../icfp2013/semantics.rkt" slide-deck)
           "../icfp2013/color-scheme.rkt"
           (only-in "../icfp2013/pict-helpers.rkt"
                    join-one braces nstruct production expr call tuple ntuple)
           (submod ".." pict-utils))
  (provide run-talk)
  (define (title)
    (parameterize ([current-slide-assembler bg-slide-assembler])
      (slide
       (cc-superimpose
        (bitmap logo-path)
        (vc-append
         (big (bold (para #:align 'center "Systematic Constructions for")))
         (big (bold (para #:align 'center "Higher-Order Program Analysis")))
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
           @item{abstract interpretations,}
           @item{of higher-order languages}))

  (define/staged (type-jail stage) #:num-stages 3
    (define base-types (with-size 150 (t "Types")))
    (define type-jail (cc-superimpose base-types (bitmap jail-path)))
    (define type-strikeout (cc-superimpose base-types (scale (page->pict prohibited-path) 2.5)))
    (match stage
      [0 base-types]
      [1 type-jail]
      [2 type-strikeout]))

  (define/staged (universal stage) #:stages [text collage]
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

  (define/staged (useful stage)
    #:stages [base anim to-name-a-few]
    #:anim-at anim
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
    (slide #:title "Intrinsically"
           (vc-append (blank 100)
                      @t{Soundness}
                      (hc-append @t{Speed} (blank 400) @t{Precision})))
    (slide #:title "Extrinsically"
           (vc-append (blank 100)
                      @t{Maintainability}
                      (hc-append @t{Designability} (blank 400) @t{Grokability})))
    (run-stages temporal)
    ;; AAAaaand thesis
    (run-stages thesis-slide))

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

  (define/staged (thesis-slide stage) #:stages [statement propose-build propose-measure]
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

  (define/staged (temporal stage) #:stages [question horse hors jail]
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
    (slide (big (bt "TODO?: Example")))

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

    (slide (big (t "New environment \"closes\" heap"))
           (big (hc-append (t "Control closure ") @tt{χ} (t " : ") (addr @t{Addr}) @tt{ → } (t "℘(Store)")))
           'next
           (big (t "Context = CContext ∪ SContext"))
           (big (production (hc-append @ctxtt{Γ} (t " ∈ CContext")) (tuple (expr @tt{e}) @idtt{ρ} @σtt{σ} @tt{χ})))
           (big (production (hc-append @ctxtt{Γ̂}(t " ∈ SContext")) (tuple (expr @tt{e}) @idtt{ρ} (addr @tt{a})))))
    ;; how 1st class control generalizes CFA2 (well, not /how/, but describes the mechanism)
    (run-stages big-jump))

  (define/staged (big-jump stage) #:stages [big-rule the-reveal]
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
  
  (define/staged (kont-values-problem stage) #:stages [see arrows solution]
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

  (define/staged (what-I-did stage) #:stages [all built-and-evaluated focus]
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
         @item[@t{Systematic summarization } (citation "HOPA workshop 2013")]
         ;; Not going to talk about 1NSAs
         (colorize-if (>= stage focus)
                      @item[@t{Abstract model of stack introspection } (citation "JFP best of ICFP 2012")]
                      "lightgray")
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

  (define/staged (essence-slogan stage) #:stages [items slogan slogan-zoom]
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

  (define/staged (summaries-cfa2 stage) #:stages [cfa2 stack-inspection]
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
    (run-stages TC-semantics)
    (run-stages TC-negation)
    (run-stages TC-abstract))

  (define/staged (proposal stage) #:stages [what-do case-study slogan]
    (cc-superimpose
     (vc-append gap-size
                (big (t "What I propose to do for HOPA"))
                (blank 50)
                (show (big (t "Case study: Temporal higher-order contracts")) (>= stage case-study)))
     (show (tagged-shadow-frame
            (big (vl-append 5 (t "Abstract runtime monitoring")
                            (t "is software model-checking")))
            (shadow (colorize (inset (big @t{Slogan}) 10) "steel blue") 10 5)) (= stage slogan))))

  (define/staged (temporal-contracts stage) #:stages [grammar]
    (vl-append gap-size
               (production (t "T") (t "T ∪ T") (t "T ∩ T") (t "¬ T") @tt{Any} @tt{Fail}
                           (t "T · T") (t "T*") (t "ε") (t "pat") (t "〈pat〉T"))
               (production (t "pat") (tt "v") (t "c(pat, …)") (hc-append (tt "!") (t "pat"))
                           (call @tt{ref} @tt{x}) (call @tt{bind} @tt{x}) @tt{_})
               (production (t "c") (tt "call") (tt "return") (tt "cons") (t "…"))))

  (define (deriv E T) (big (hc-append (hb-append (t "∂") (small (t E))) (t T))))

  (define/staged (TC-semantics stage) #:stages [meaning semantics]
    (big
     (vc-append gap-size
                (t "Each ‶event″ steps the temporal contract")
                (hc-append (show (t "⟦") (= stage semantics)) (deriv "E" "T") (show (t "⟧") (= stage semantics))
                           (show (hc-append
                                  @t{ = }
                                  (braces (t "π : Eπ ∈ ⟦T⟧")))
                                 (= stage semantics)))
                (t "∂ standard except matching and negation"))))

  (define/staged (TC-negation stage) #:stages [problematic possible good prefixes problem solution derivative]
    (define e-meaning
      (hc-append (t "⟦") @tt{e} (t "⟧")))
    (big
     (vc-append gap-size
                (big @t{Negation is problematic})
                (pict-cond
                 [(and (<= possible stage) (< stage solution))
                  (t "⟦¬ T⟧ = Traces ∖ ⟦T⟧ ?")]
                 [(>= stage solution)
                  (vc-append gap-size
                             (hc-append (t "⟦¬ T⟧ = ")
                                        (braces @t{ε})
                                        @t{ ∪ }
                                        (braces @t{π : ∀ π′ ∈ F⟦T⟧∖{ε}. π′ ⋢ π}))
                             (show (hc-append (deriv "E" "¬ T")
                                              @t{ = }
                                              (call @t{ν} (deriv "E" "T"))
                                              @t{ → }
                                              @tt{Fail}
                                              @t{, ¬ } (deriv "E" "T")) (= stage derivative)))]
                 [else (blank 0)])
                (pict-cond
                 [(= stage good) (hc-append e-meaning (t " ∈ ⟦T⟧"))]
                 [(and (<= prefixes stage) (< stage solution))
                  (vc-append gap-size
                             (hc-append (call @t{prefixes} e-meaning) (t " ⊆ prefixes(⟦T⟧)"))
                             (show (hc-append (it "e.g. ") (t "E ∈ prefixes(⟦¬ E⟧)")) (= stage problem)))]))))

  (define/staged (TC-abstract stage) #:stages [first second]
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
    (slide #:title "Systematic analysis constructions"
           (vc-append gap-size (big (vl-append gap-size
                                               @item[@t{AAM } (citation "ICFP 2010")]
                                               @item[@t{Calculational approach } (citation "SAS 2008")]))))
    (slide #:title "Pushdown higher-order analysis"
           @item{(introspective) PDCFA}
           @item{CFA2}
           @item{HORS})
    (slide #:title "Temporal properties"
           (colorize (t "Dynamic:") "firebrick")
           @item{J-LO}
           @item{Tracematches}
           @item[@t{Temporal contracts } (citation "ICFP 2011")]
           (colorize (t "Static:") "steel blue")
           @item{Cecil}
           @item{JPF-LTL}))

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

  (define (run-talk [sections '(intro/why useful wrong done do related timeline/wrapup)])
    (when (memv 'intro/why sections)
      (title)
      (why-hopa))
    (when (memv 'useful sections) (run-stages useful))
    (when (memv 'wrong sections) (whats-wrong))
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
  (run-talk))