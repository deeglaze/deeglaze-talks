#lang at-exp slideshow
(require (except-in unstable/gui/slideshow big)
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
         (only-in plot/utils polar->cartesian))


(define logo-path (collection-file-path "prl-logo.png" "talk-utils"))
(define-runtime-path oaam-path "oaam.pdf")
(define-runtime-path oaam-png-path "oaam.png")

(define js-path (collection-file-path "logo_JavaScript.png" "talk-utils"))
(define dalvik-path (collection-file-path "dalvik-logo.jpg" "talk-utils"))
(define bg-slide-assembler
  (lambda (title sep content)
    (inset content (- margin) (- margin) 0 0)))

(define use-pdf? #t)
(define output-png? #f)
(define (scale-pdf-or-png pdf-path factor png-path)
  (cond [use-pdf?
         (define pict
           (scale ((dynamic-require (collection-file-path "poppler-main.rkt" "talk-utils") 'page->pict)
                   pdf-path)
                  factor))
         (when output-png?
           (with-output-to-file
              png-path #:exists 'replace
              (λ () (write-bytes (convert (pict->bitmap pict) 'png-bytes)))))
           pict]
        [else (bitmap png-path)]))

(define-values (d-red d-gray d-blue d-dark-blue d-green d-yellow d-yellow-green d-dim-green d-bright-green)
  (values "firebrick"
          "gray"
          "steel blue"
          "dark slate blue"
          "medium forest green"
          "gold"
          "olivedrab"
          "limegreen"
          "lime"))
(define-values (a-red a-gray a-blue a-dark-blue a-green a-yellow a-yellow-green a-dim-green a-bright-green)
  (values d-red d-gray d-blue d-dark-blue d-green d-yellow d-yellow-green d-dim-green d-bright-green))

(define (default-colors!)
  (set! a-red d-red)
  (set! a-gray d-gray)
  (set! a-blue d-blue)
  (set! a-dark-blue d-dark-blue)
  (set! a-green d-green)
  (set! a-yellow d-yellow)
  (set! a-yellow-green d-yellow-green)
  (set! a-dim-green d-dim-green)
  (set! a-bright-green d-bright-green))
(define (darker-gray!)
  (default-colors!)
  (set! a-gray "dark gray"))
(define (even-darker-gray!)
  (default-colors!)
  (set! a-gray "lightslategray"))
(define (darker-gray-bright-green!)
  (default-colors!)
  (set! a-gray "dark gray")
  (set! a-dim-green "yellowgreen")
  (set! a-bright-green "limegreen"))

(define SC1 "Cinzel")
(define SC2 "Linux Libertine Capitals O")
(current-main-font SC1)

(module+ pict-utils
  (provide kinda-big big really-big
           frame* arrows-back-and-forth tagged-shadow-frame hilight-tag
           in-sawasdee citation
           default-colors! darker-gray! even-darker-gray! darker-gray-bright-green!)

  (define-syntax-rule (kinda-big e) (with-size 34 e))
  (define-syntax-rule (big e) (with-size 40 e))
  (define-syntax-rule (really-big e) (with-size 50 e))
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
       #:color a-gray #:show '(l t r)))
    (pin-over
     (shadow-frame base #:margin frame-margin #:sep frame-sep)
     (sub1 (+ frame-sep frame-margin)) (+ (- (pict-height top-frame)) frame-margin 1)
     top-frame))
  
  (define ((hilight b) p)
    (show (colorize (filled-rectangle (pict-width p) (pict-height p)) "yellow") b))
  (define (hilight-tag base finder tag #:show [condition #t])
    (pin-under-tag base finder tag (hilight condition)))

  (define-syntax-rule (in-sawasdee . body) (parameterize ([current-main-font "Sawasdee"]) . body))
  (define (citation txt #:size [size 18])
    (colorize (with-size size (t (format "[~a]" txt))) a-gray)))

(module+ slide-deck
  (require slideshow-helpers/slide
           (submod icfp2013-talk/semantics slide-deck)
           (rename-in (except-in (submod icfp2013-talk/icfp2013 slide-deck) title)
                      [run-talk icfp-talk])
           icfp2013-talk/color-scheme
           (only-in (submod hopa2013-talk/hopa2013 slide-deck) concrete gc-slide)
           (submod hopa2013-talk/call-ret slide-deck)
           (submod hopa2013-talk/hammer-slide slide-deck)
           (only-in icfp2013-talk/pict-helpers
                    join-one braces nstruct production expr call tuple ntuple parens)
           (submod ".." pict-utils))
  (provide run-talk parting)
  (define (title)
    (parameterize ([current-slide-assembler bg-slide-assembler])
      (slide
       (cc-superimpose
        (bitmap logo-path)
        (vc-append
         (big (bold (para #:align 'center "Systematic Constructions for")))
         (big (bold (para #:align 'center "Higher-Order Program Analyses")))
         (blank-line)
         (para #:align 'center @bt{Dionna Glaze})
         (blank-line)
         (t "Thesis Proposal")
         (blank-line)
         (t "Northeastern University")
         (blank-line)
         (small (t "2013 November 20")))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Why HOPA?

  (define (why-hopa)
    (slide (big @t{Program analyses predict program behavior})
           'next
           (blank-line)
           @t{Find bugs,}
           @t{optimization opportunities,}
           @t{verify bug-freedom}
           )
    (slide (big @t{HOPA is a class of})
           @item{online,}
           @item{computable,}
           @item{abstract interpretations}
           @item{of higher-order languages})
    (run-stages useful))

  (define/staged useful
    #:stages [base anim to-name-a-few]
    #:anim-at [anim
               #:skip-first
               #:steps 30]
    (define big-font 28)
    (define small-font 16)
    (define base-pict (big @t{HOPA is useful}))
    (define (vlappend-vec v) (apply vl-append 3 (vector->list v)))
    (define (anim-fn n)
      (with-font SC2
                 (with-size small-font
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
                     (vl-append (with-size big-font (colorize @t{Optimizations} a-red))
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
                     (vl-append (with-size big-font (colorize @t{Program Understanding} a-green))
                                (shadow-frame (ghost (vlappend-vec understandings)))))
                   (define temporal @t{Temporal logic model-checking})
                   (define contract @t{Behavioral contract validity})
                   (define verifications (vector temporal contract))
                   (define verification-base
                     (vl-append (with-size big-font (colorize @t{Verification} a-blue))
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
                      (scale2 n)))))))
  
    (match stage
      [(== base) (cc-superimpose base-pict (ghost (anim-fn 0.0)))]
      [(== anim) anim-fn]
      [(== to-name-a-few)
       (cc-superimpose (anim-fn 1.0) 
                       (shadow-frame (big @t{To name a few})))]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; What's wrong with HOPA?

  (define (whats-wrong)
    (slide (really-big (t "What's wrong with HOPA?")))
    (run-stages intrinsic/extrinsic #:group 'intro)
    ;; Motivate systematic constructions
    (run-stages (grinder #t))
    ;; AAAaaand thesis
    (run-stages thesis-slide))

  (define t-int
    (with-size 42 (hc-append (t "Intrinsically") (ghost (t " and Extrinsically")))))
  (define t-ext
    (with-size 42 (hc-append (t "Intrinsically") (t " and Extrinsically"))))

  (define/staged intrinsic/extrinsic #:stages [[intrinsic #:title t-int]
                                               extrinsic 0CFA HORS JPF AAM mine
                                                         zoom-extrinsic systematic
                                                         bad-aam]
    #:title t-ext
    #:group intro (list intrinsic extrinsic 0CFA HORS JPF AAM mine zoom-extrinsic systematic)
    ;; Quality / 0CFA / HORS / JPF / AAM / Me
    (define goodness `#(,a-red ,a-yellow ,a-yellow-green ,a-dim-green ,a-bright-green))
    (define precise-pointer (blank))
    (define (color-box name number)
      (colorize (filled-rectangle (pict-width name) (pict-height name))
                (vector-ref goodness (sub1 number))))
    (define the-table
      `(("0CFA" ,0CFA .
         #hash((sound . 5) (fast . 5) (precise . 2) (maintainable . 2) (design . 2) (grokable . 3)))
        ("HORS" ,HORS .
         #hash((sound . 4) (fast . 3) (precise . 5) (maintainable . 2) (design . 1) (grokable . 1)))
        ("JPF" ,JPF .
         #hash((sound . 2) (fast . 3) (precise . 5) (maintainable . 3) (design . 3) (grokable . 2)))
        ("AAM" ,AAM .
         #hash((sound . 5) (fast . 1) (precise . 2) (maintainable . 4) (design . 5) (grokable . 5)))
        ("Thesis" ,mine .
         #hash((sound . 5) (fast . 4) (precise . 4) (maintainable . 4) (design . 5) (grokable . 5)))))
    (define impl-names (for/list ([impl the-table])
                         (match-define (list-rest name at-stage quals) impl)
                         (show (inset (t name) 5) (>= stage at-stage))))
    (define dimensions '("Sound" "Fast" "Precise" "Design Ease" "Maintainable" "Grokable"))
    (define dimension-picts (for/list ([txt dimensions]) (inset (t txt) 5)))
    (define worst (t "Worst "))
    (define best (t " Best"))
    (define spectrum
      (apply hc-append 0
             worst
             (append
              (for/list ([c (in-range 1 (add1 (vector-length goodness)))]) (color-box (cc-superimpose worst best) c))
              (list best))))
    (define table-picts
      (append*
       (cons (blank) impl-names)
       (for/list ([quality '(sound fast precise design maintainable grokable)]
                  [txt dimension-picts]
                  [txt-phase (list intrinsic intrinsic intrinsic extrinsic extrinsic extrinsic)])
         (define tpict (tag-pict (show txt (>= stage txt-phase)) quality))
         (cons tpict
               (for/list ([impl (in-list the-table)]
                          [title (in-list impl-names)])
                 (match-define (list-rest name at-stage quals) impl)
                 (define box (color-box
                              (cc-superimpose (blank 0 (pict-height tpict)) title)
                              (hash-ref quals quality)))
                 (show
                  (cond [(equal? name "AAM")
                         (cond
                          [(eq? quality 'design) (tag-pict box 'AAM-start)]
                          [(eq? quality 'precise) (tag-pict box 'AAM-precision)]
                          [else box])]
                        [(and (equal? name "Thesis") (eq? quality 'grokable))
                         (tag-pict box 'Thesis-end)]
                        [else
                         box])
                  (>= stage at-stage)))))))
    (define base (table 6 table-picts rc-superimpose lt-superimpose 0 0))
    (define pin-precision
      (pin-over base -50 150 precise-pointer))
    (define-values (aam-x aam-y) (lt-find base (find-tag base 'AAM-start)))
    (define-values (thesis-x thesis-y) (rb-find base (find-tag base 'Thesis-end)))
    (define prec (find-tag base 'AAM-precision))
    (define-values (prec-x prec-y) (lt-find base prec))
    (define my-delta
      (show
       (shadow
        (inset (thick-filled-rounded-rectangle
                (add1 (- thesis-x aam-x)) (add1 (- thesis-y aam-y))
                #:style 'transparent #:border-color a-red #:border-width 15)
               10)
        #:color a-yellow
        5)
       (>= systematic stage zoom-extrinsic)))
    (define pin-my-delta
      (pin-over pin-precision (- aam-x 10) (- aam-y 10) my-delta)) ;; offset position by inset amount
    (define precision-frame
      (show (shadow
             (inset (thick-filled-rounded-rectangle
                     (pict-width (first prec)) (pict-height (first prec))
                     #:style 'transparent #:border-color a-dark-blue #:border-width 15) 10)
             5 #:color a-yellow)
            (= stage bad-aam)))
    (define aam-precision
      (vc-append
       (pin-over pin-my-delta (- prec-x 10) (- prec-y 10) precision-frame)
       (blank 100)
       (show spectrum (>= stage 0CFA))))
    (define point-to-precision
      (pin-arrow-line 15 aam-precision
                      precise-pointer cc-find
                      (find-tag aam-precision 'precise) lc-find
                      #:line-width 3
                      #:color a-red))
    (cc-superimpose
     (blank 900 500)
     (if (= stage bad-aam) point-to-precision aam-precision)
     (show (rotate (shadow-frame (big (t "Systematic constructions"))) (* 1/8 pi))
           (= stage systematic))))

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
       (define bg
         (tagged-shadow-frame
          (hilight-tag
           (hilight-tag thesis-pict lt-find 'systematic #:show (>= stage propose-build))
           lt-find 'measure #:show (= stage propose-measure))
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

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; What have I done for HOPA?

  (define (what-done)

    (slide (really-big (t "First: Recap of AAM")))
    ;; Recap of AAM
    (run-stages what-is-CESK)
    (parameterize ([pushdown? #f]
                   [use-color? #f])
      (define psCESK (CESK-table sCESK))
      (define psCESK* (CESK-table sCESK*))
      (define psaCESK (CESK-table saCESK))
      (slide psCESK)
      (slide (cc-superimpose psCESK (rotate (shadow-frame (t "Store-allocate continuations")) (* 1/8 pi))))
      (play (λ (n) (fade-pict n psCESK psCESK*)) #:steps 20 #:skip-first? #t)
      (slide psCESK*)
      (slide (cc-superimpose psCESK* (rotate (shadow-frame (t "Nondeterministic bounded store access")) (* 1/8 pi))))
      (play (λ (n) (fade-pict n psCESK* psaCESK)) #:steps 20 #:skip-first? #t)
      (slide psaCESK))
    ;; plug ICFP paper
    (run-stages oaam-plug)
    (parameterize ([use-color? #f])
      (run-stages state-to-edge))
    (parameterize ([current-main-font SC2])
      (run-stages (aam->oaam #f) #:group 'intro)) ;; graph interpretation
    (parameterize ([use-color? #f])
      (icfp-talk '(lazy abscomp) #:main-font SC2 #:deltas? #f)) ;; steal ICFP lazy and abscomp bits whole sale
    ;; evaluation
    (slide #:title "Factor speed-up over naive vs. paper section"
           (force bench-overview))
    (run-stages intrinsic/extrinsic #:stage 'bad-aam) ;; transition from OAAM to pushdown
    (slide (really-big (t "Pushdown abstraction and GC")))
    (run-stages (call/ret #f)) ;; motivate summaries
    (run-stages (call/ret #t)) 
    (hammer #t) ;; additional plug for semantics versus automata
    (run-stages essence-slogan)
    (run-stages high-level-summaries)

    (parameterize ([current-main-font "Andale Mono"])
      (run-stages concrete)))
  
  (define/staged oaam-plug #:stages [plug slogan]
    (cc-superimpose
     (shadow-frame (scale-pdf-or-png oaam-path 0.7 oaam-png-path))
     (show (tagged-shadow-frame
            (big (vl-append
                  (t "Engineering tricks as")
                  (t "semantics refactorings")))
            (inset (shadow (big (colorize @t{Slogan} a-blue)) 10 5) 10))
           (= stage slogan))))

  (define/staged high-level-summaries
    #:stages [an-expression save-to-table some-context some-more-context
                            the-change last-step
                            guarantee]
    (define big-left (big (show @tt{Ξ[(e, σ) ↦ κ]} (>= stage the-change))))
    (define mid (blank 80 1))
    (define big-right (big
                       (pict-cond
                        [(= stage save-to-table) @tt{M[e ↦ v]}]
                        [(>= stage some-context) @tt{M[(e, σ) ↦ (v, σ′)]}])))
    (vc-append
     gap-size
     (big
      (vl-append gap-size
                 (hc-append (pict-cond #:combine rc-superimpose
                                       [(<= stage save-to-table) @tt{e}]
                                       [(= stage some-context) (ntuple @tt{e} @tt{σ})]
                                       [(>= stage some-more-context) (ntuple @tt{e} @tt{σ} @tt{κ})]
                                       [else big-left]) ;; just for the spacing
                            (cc-superimpose (ghost mid)
                                            (hc-append @tt{↦} (superscript (t "*"))))
                            (pict-cond #:combine lc-superimpose
                                       [(<= stage save-to-table) @tt{v}]
                                       [(= stage some-context) (ntuple @tt{v} @tt{σ′})]
                                       [(>= stage some-more-context) (ntuple @tt{v} @tt{σ′} @tt{κ})]
                                       [else big-right])) ;; just for spacing
                 (show (hc-append (ghost big-left) (cc-superimpose (ghost mid) @tt{⇓})) (>= stage the-change))
                 (show (hc-append (rc-superimpose
                                   (ghost big-left)
                                   (ntuple @tt{e} @tt{σ} @tt{_}))
                                  (cc-superimpose (ghost mid)
                                                  (hc-append @tt{↦} (superscript (t "*"))))
                                  (ntuple @tt{v} @tt{σ′} @tt{_})) (>= stage the-change))
                 (show (hc-append (ghost big-left)
                                  (cc-superimpose (ghost mid) @tt{↦})
                                  (ntuple @tt{v} @tt{σ′} @tt{κ}) (t " if ") @tt{κ = Ξ(e, σ)})
                       (>= stage last-step))
                 ;; move memo over to value side
                 (show (hc-append big-left mid big-right) (>= stage save-to-table))))
     (blank-line)
     (show (hc-append @tt{κ} (t " irrelevant to evaluate ") @tt{e}) (>= stage some-more-context))))

  (define/staged outline #:stages [all built-and-evaluated subsumed]
    #:title "Talk overview"
    (define built-pict
      (in-sawasdee (show (with-size 24 (t "Built, proved and evaluated: 1000x speed-up"))
                         (>= stage built-and-evaluated))))
    (define subsumed-pict
      (rotate (in-sawasdee (show (with-size 24 (t "Subsumed by")) (= stage subsumed)))
              (* 0.3 pi)))
    (define abs-model
      (colorize @subitem[@t{Abstract model of stack introspection } (citation "JFP best of ICFP 2012")]
                a-gray))
    (define summarization
      @subitem[@t{Systematic summarization } (citation "HOPA workshop 2013")])
    (define base
      (pin-over
       (pin-over
        (cc-superimpose
         (with-size 28
           (vl-append
            gap-size
            @item[(colorize (t "Contributions and prosed work:") a-blue)]
            (tag-pict
             @subitem[@t{Systematic optimizations } (citation "ICFP 2013")]
             'built)
            ;; Not going to talk about 1NSAs
            abs-model
            summarization
            @subitem{Case study: Temporal contracts}
            ;; next
            @item[(colorize (t "Related Work") a-green)]
            ;; next
            @item[(colorize (t "Timeline") a-red)]
            )))
        300 -50
        built-pict)
       -100 100 subsumed-pict))
    ;; Put "slogan" in a frame without a bottom so we can make it look like
    ;; it's part of the shadow frame containing the slogan
    (pin-arrow-line 15
                    (pin-arrow-line
                     15
                     base
                     built-pict rc-find
                     (first (find-tag base 'built))
                     (λ (p path) (define-values (x y) (rc-find p path)) (values (- x 170) y))
                     #:alpha (if (>= stage built-and-evaluated) 1 0)
                     #:hide-arrowhead? (not (>= stage built-and-evaluated))
                     #:start-angle 0
                     #:start-pull .5
                     #:end-angle pi
                     #:end-pull .5
                     #:line-width 2)
                    abs-model (λ (p path) (define-values (x y) (lc-find p path)) (values (+ x 40) (- y 15)))
                    summarization (λ (p path) (define-values (x y) (lc-find p path)) (values (+ x 40) (- y 15)))
                    #:start-angle (* 1.1 pi) #:start-pull 1.2
                    #:end-angle (* 1.9 pi)
                    #:alpha (if (= stage subsumed) 1 0)
                    #:hide-arrowhead? (not (= stage subsumed))
                    #:line-width 2))

  (define/staged essence-slogan #:stages [items slogan slogan-zoom]
    (define frame-margin 20)
    (define frame-sep 5)
    (define the-slogan
      (tagged-shadow-frame
       (hc-append
        (t "Summarization is ")
        (tag-pict
         (colorize-if (>= stage slogan-zoom) (t "context-sensitive") a-green)
         'context)
        (t " memoization"))
       (inset (shadow (big (colorize @t{Slogan} a-blue)) 10 5) 10)
       #:margin frame-margin
       #:sep frame-sep))
    (define some-contexts
      (with-font SC2
                 (vl-append @t{Heap}
                            @t{Stack root addresses}
                            @t{Continuation marks}
                            @t{Temporal monitor state})))
    (define framed-contexts (shadow-frame some-contexts))
    (define base
      (vc-append gap-size
                 (big (t "Systematic summarization"))
                 (vl-append gap-size
                            @item{AAM-style construction}
                            @item{Generalizes state-of-the-art}
                            @item{Allows stack-inspection and GC}
                            @item{New look at first-class control})))
    (define frame-y 350)
    (pin-over-hcenter
     base
     (/ (pict-width base) 2) -50     
     (show
      (pin-arrow-line
       15
       (pin-over the-slogan 490 frame-y (show framed-contexts (>= stage slogan-zoom)))
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

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; What do I propose to do for HOPA?

  (define (what-do)
    (run-stages proposal)
    (run-stages TC-example)
    (run-stages TC-semantics)
    (run-stages TC-negation #:group 'less-theory)
    (run-stages TC-abstract))

  (define/staged proposal #:stages [what-do case-study slogan]
    (cc-superimpose
     (vc-append gap-size
                (big (t "Why so much precision?"))
                (blank 50)
                (show (kinda-big (t "Case study: Temporal higher-order contracts")) (>= stage case-study)))
     (show (tagged-shadow-frame
            (big (vl-append 5 (t "Abstract runtime monitoring")
                            (t "is software model-checking")))
            (shadow (colorize (inset (big @t{Slogan}) 10) a-blue) 10 5)) (= stage slogan))))

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
                       #,(show
                          (code
                           (rec/c c-f
                                  (tmon (path? . -> . (list/c (-> any/c) (any/c . -> . void?) (-> void?)))
                                        #,neg
                                        f)))
                          (>= stage scontract))))
               (t "Spec: no calls after close")))

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

  (define/staged TC-negation #:stages [problematic good monitor-strategy prefixes
                                       problem post-problem derivative correctness solution hilight-denotation
                                       characterizing interesting]
    #:title (big @t{Negation is problematic})
    #:group less-theory (list problematic good monitor-strategy prefixes
                              problem post-problem derivative correctness solution hilight-denotation)
    (big
     (let ()
      (define e-meaning
        (hc-append (t "⟦") @tt{e} (t "⟧")))
      (define neg-lhs (t "⟦¬ T⟧ = "))
      (define bad-neg (hc-append @t{Traces ∖ ⟦T⟧ ? } (citation "ICFP 2011")))
      (define good-neg (hc-append (braces @t{ε})
                                  @t{ ∪ }
                                  (braces @t{π : ∀ π′ ∈ } (tag-pict @t{F⟦T⟧} 'denotation) @t{∖{ε}. π′ ⋢ π})))
      (define neg-deriv (deriv "E" "¬ T" #:call? #t))
      (define neg-deriv-sem
        (hc-append neg-deriv
                   @t{ = }
                   (call @tt{done?} (deriv "E" "T"))
                   @t{ → }
                   @tt{Fail}
                   @t{, } (call @t{¬} (deriv "E" "T"))))
      (define full-trace-semantics (hc-append e-meaning (t " ∈ ⟦T⟧")))
      (define partial-trace-semantics (hc-append (call @t{prefixes} e-meaning) (t " ⊆ prefixes(⟦T⟧)")))
      (define (strikeout p #:show [condition #t])
        (cc-superimpose
         p
         (show (colorize (filled-rectangle (* 1.2 (pict-width p)) 4) a-red) condition)))
      (define pre-solution
        (vc-append
         gap-size
         (hc-append neg-lhs bad-neg)
         (pict-cond
          [(and (<= good stage) (< stage solution))
           (vc-append
            gap-size
            (pict-cond #:combine cc-superimpose
                       [(<= good stage monitor-strategy)
                        (strikeout full-trace-semantics #:show (= stage monitor-strategy))]
                       [(<= stage post-problem)
                        (strikeout partial-trace-semantics #:show (= stage post-problem))])
            (show (colorize (cond
                             [(= stage monitor-strategy)
                              (t "Ineffective monitor strategy")]
                             [(= stage prefixes) (t "?")]
                             [else (t "!")]) a-red)
                  (<= monitor-strategy stage problem))
            (show (vc-append (hc-append (it "No, consider ") (t "⟦¬ E⟧ = Traces ∖ {E} "))
                             (hc-append (t "so EE ∈ Traces ∖ {E}"))
                             (colorize (t "Thus E ∈ prefixes(⟦¬ E⟧)") a-red)) (<= problem stage post-problem)))]
          [else
           (vc-append
            gap-size
            (show (t "⟦¬ ¬ T⟧ = {ε} ∪ {Aπ : A ∈ F⟦T⟧}") (>= stage characterizing))
            (show (t "⟦¬ ¬ ¬ T⟧ = {ε} ∪ {Aπ : A ∉ F⟦T⟧}") (>= stage characterizing))
            (show (t "Interesting consequence:") (= stage interesting))
            (show (t "⟦¬ ¬ T⟧ ≠ ⟦T⟧ but ⟦¬ ¬ ¬ ¬ T⟧ = ⟦¬ ¬ T⟧") (= stage interesting)))])))
      (define post-solution
        (vc-append gap-size
                   (colorize (t "Idea: solve operationally") a-blue)
                   (blank-line)
                   neg-deriv-sem
                   (blank-line)
                   (show (t "Correctness criterion:") (>= stage correctness))
                   (show (hc-append (t "⟦") neg-deriv (t "⟧ = {π : Eπ ∈ ⟦¬ T⟧}")) (>= stage correctness))
                   (show (t "Denotationally:") (>= stage solution))
                   (show (hilight-tag (hc-append neg-lhs good-neg) lt-find 'denotation #:show (= stage hilight-denotation))
                         (>= stage solution))))
      (if (<= stage post-problem)
          pre-solution
          post-solution))))

  (define/staged TC-abstract #:stages [first second third]
    (define Γ (with-font SC2 (t "Γ")))
    (define μ (with-font SC2 (t "μ")))
    (vc-append
     gap-size
     (vl-append gap-size
                (big @t{Problems remaining in TC analysis:})
                @item{Abstract derivatives}
                (show
                 (vl-append gap-size
                            @subitem{Combinatorial blowup}
                            @subitem[@t{Precise identification } (parens μ (t ",") Γ)]
                            @subitem[Γ (t " ⇒ weak reference semantics")])
                 (>= stage second))
                @item{State explosion}
                (show
                 (vl-append gap-size
                            @subitem[Γ (t "⇒ per-state stores ⇒ exponential")]
                            @subitem{Solution? Summarization → Sparseness})
                 (>= stage third)))
     (show
      (small
       (colorize
        (vc-append 3
                   (hc-append Γ (t " = abstract garbage collection"))
                   (hc-append μ (t " = abstract counting"))
                   (t "[ICFP 2006]"))
        a-dark-blue))
      (>= stage second))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; What's the related work?

  (define (what-related)
    (slide (really-big (t "Related Work")))
    (slide #:title "Related Work Outline"
           (big
            (vl-append gap-size
                       @item{Design and implementation}
                       @item{Pushdown analysis}
                       @item{(H-O) Temporal properties})))
    (slide #:title "Analysis design and implementation"
           (vc-append
            gap-size
            (big (vl-append
                  gap-size
                  @item[@t{Calculational approach } (citation "Marktoberdorf 1998, SAS 2008")]
                  @item[@t{AAM } (citation "ICFP 2010")]
                  @item[@t{Pretty-big-step Certified AI } (citation "JFLA 2014")]
                  @item[@t{Astrée } (citation "FMSD 2009")]
                  @item[@t{Sparrow/Airac } (citation "ASPLAS 2009/2011, SPE 2010, VMCAI 2011, PLDI 2012")]))))
    (slide #:title "Pushdown analysis"
           (vc-append
            gap-size
            (colorize (t "Higher-order:") a-blue)
            (big (vl-append gap-size
                            @item[@t{(introspective) PDCFA } (citation "Scheme Workshop 2010, ICFP 2012")]
                            @item[@t{CFA2 } (citation "ESOP 2010, ICFP 2011")]
                            @item[@t{HORS } (citation "LICS 2006, PPDP 2009, POPL 2009, PLDI 2011, ...")]))
            (colorize (t "First-order:") a-red)
            (big (vl-append gap-size
                            @item[@t{LTL with regular valuations } (citation "TACS 2001")]
                            @item[@t{Weighted pushdown automata } (citation "SAS 2003, CAV 2006")]))))
    (slide #:title "Temporal properties"
           (vc-append gap-size
                      (colorize (t "Dynamic:") a-red)
                      (big (vl-append gap-size
                                      @item[@t{J-LO } (citation "RV 2005")]
                                      @item[@t{Tracematches } (citation "OOPSLA 2005")]
                                      @item[@t{Temporal contracts } (citation "ICFP 2011")]))
                      (colorize (t "Static:") a-blue)
                      (big (vl-append gap-size
                                      @item[@t{Cecil } (citation "Software Engineering 1990")]
                                      @item[@t{HORS}]
                                      @item[@t{Trace effect analysis } (citation "JFP 2008")])))))

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

  (define/staged parting #:stages [open thanks]
    (pin-over-hcenter
     thesis-pict
     (/ (pict-width thesis-pict) 2) 300
     (show
      (parameterize ([current-main-font "Respective Slanted"])
        (colorize (with-size 110 (t "Thank You")) a-dark-blue))
      (= stage thanks))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Extra
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

  (define (1st-class-control)
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
                             (inset (shadow (colorize (big (t "Surprise!")) a-red) 10 5) 10)))]))
  
  (define/staged kont-values-problem #:stages [see arrows solution]
    (define store-pict
      (big (join-one (tag-pict @σtt{σ} 'first)
                     @idtt{a}
                     (braces
                      (nstruct "rt" (idtt "e") (idtt "ρ") (tag-pict (pict-if (= stage solution)
                                                                             (colorize @tt{b} a-green)
                                                                             @σtt{σ′}) 'second))))))
    (vc-append
     gap-size
     (big (t "Problem:"))
     (pict-if (= stage arrows)
              (arrows-back-and-forth store-pict (find-tag store-pict 'first) (find-tag store-pict 'second))
              store-pict)
     (blank 50)
     (show (big (t "AAM: break circularity with indirection")) (= stage solution))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Main

  (define (run-talk [sections '(intro/why wrong outline done do related timeline/wrapup)])
    (when (memv 'intro/why sections)
      (title)
      (why-hopa)) ;; universal, optimizations, understanding, verification
    (when (memv 'wrong sections) (whats-wrong)) ;; hard to do everything easily, well and understandably.
    (when (memv 'outline sections) (run-stages outline))
    (when (memv 'done sections) (what-done))
    (when (memv 'do sections) (what-do))
    (when (memv 'related sections) (what-related))
    (when (memv 'timeline/wrapup sections)
      (timeline)
      (run-stages parting))
    (when (memv 'extras sections)
      (1st-class-control)
      (run-stages gc-slide)
      (slide #:title "Continuation marks?"
             (t "Like GC, but an abstraction of")
             (tt "(current-continuation-marks)")
             (hc-append (t "instead of ") (tt "A")))
      (run-stages temporal-contracts))))

(module+ main
  (require (submod ".." slide-deck))
  (even-darker-gray!)
  (void (run-talk)))
