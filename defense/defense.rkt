#lang at-exp slideshow

#|

To play:

At command line:
racket defense.rkt

In Dr. Racket:
Click run or press F5


==================================================
== Outline of talk ===============================
==================================================
Introduction:
 People can write interpreters. There is evidence of this.
 When I say people, I don't mean academics like you.
   I mean people in the wild. In their natural habitat.
 People can't (or don't) write abstract interpreters.
 The benefits abstract interpreters provide are bountiful:
  - Understand code
    - value flow / type inference
    - control flow / dependence analysis
  - Harden code
    - array out-of-bounds access / generally crash-freedom
    - contract verification
    - information flow security
    - data race detection
  - Improve code
    - remove runtime enforcement of proven properties
    - optimization in general (long list)
 The detractors are unfortunately also bountiful:
  It's too hard to make right.
  It's too hard to make fast.
  It's too hard to make precise (useful/less annoying)
  It's too hard to understand or apply the literature (assumes first-order)
  It's too easy to waste your time formalizing programming rather than doing programming.
  Perceived cost is greater than perceived value.

 How do we lower these costs?
 Right - make it systematic/automatic and concretely executable
 Fast - amazingly easy to systematize folklore implementation techniques.
 Precise - Memoize => precise return flow. Base values: AI literature has it in the bag.
 Understand - All in terms of what language programmers know: interpreters.
 Waste your time - I already did that. And I made a language so you don't have to.

 Which brings me to my thesis:
 ------------------------------
 Precise and performant analyses for higher-order languages can be
 systematically and algorithmically constructed from their semantics.
 ------------------------------

Outline (an abstract) for the rest of the talk:
Why abstract machines are a good analysis target.
Two easy implementation techniques that are effective.
The return flow problem, and the semantics to fix it.
A language design for implementing and abstracting abstract machines.
Related work
Conclusion/future work

------------------------------
-- Why abstract machines?
------------------------------
They're at a sweet spot between efficiently implementable and easily specified.
  [Compare reduction semantics of LC and CEK]
They're executable.
They're used to model higher-order languages (thanks Matthias).
They require a relatively small language to express.

Where does analysis fit in? (let's beat a dead horse for the PL people)
Finite state space => finite reduction relation => finite approximation of program execution.

To get a finite state space, we disallow unbounded nesting of data in a machine state via
store allocation, and then we bound the address space. Store access becomes non-deterministic.

Boom. Sound. Executable. Slow.

------------------------------
-- Implementation
------------------------------
Lazy non-determinism (use ICFP slides)
Store deltas (need new slides?)
Speedup graph.

------------------------------
-- Precise return flow
------------------------------
Use DLS slides?

------------------------------
-- Language design
------------------------------
We've seen some abstract machines now.
Let's decompose the rules:

Pattern ↦ Template [side-conditions]

We want a language of abstract machines where we can flip a switch to go from
concrete execution to abstract execution.

We thus have language support for a store, implicit addresses for structure,
and some muckity muck for equality.

 [Should I discuss equality? Don't discuss metafunctions.]

The few parameters are `alloc`, `make-variant`, and `tick`
The make-variant function has a checkable contract, so we always stay sound.

 [Add if there's time:
  Won't go into details, but a type system can identify unbounded nesting at
  variant constructions, so we can synthesize a skeleton make-variant that
  heap-allocates what it must in order to get a finite abstraction.]

------------------------------
-- Related work
------------------------------
Implementation/Pushdown/Modeling semantics. Pull from diss.

------------------------------
-- Conclusion/future work
------------------------------
I proved my thesis, damn it.
Future work:
 * Type system for language can identify unbounded nesting to insert
   implicit addresses. Can use adjunction on "heapified types" to
   make all addresses explicit, which is better for implementation.
 * Make it fast.
 * Make some external domains for base values that don't suck.
 * Add pushdown abstraction.
 * Add black holes.
 * Dogfood.







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
         (only-in icfp2013-talk/icfp2013 fanout-pict lazy-pict speedup-pict)
         "wtable.rkt"
         (rename-in
          (only-in (submod icfp2013-talk/icfp2013 slide-deck)
                   intro
                   grinder
                   grinder-grinder
                   what-is-CESK)
          [intro aam-intro]))
(latex-path "/usr/bin/pdflatex")
(latex-debug? #t)
(set-page-numbers-visible! #t)

(define SCREEN-WIDTH 1024)
(define SCREEN-HEIGHT 768)

(define bg-slide-assembler
  (lambda (title sep content)
    (inset content (- margin) (- margin) 0 0)))

(define-syntax-rule (scode e ...)
  (parameterize ([current-font-size 28])
    (code e ...)))

(define-syntax-rule (wct . forms) (parameterize ([current-main-font "Cantarell"]) . forms))
(define (ct s) (wct (t s)))
(define (cbt s) (wct (bt s)))
(define (cit s) (wct (it s)))

;(define (thanks s)  (text (string-downcase s) "Respective" (current-font-size)))
(define (thanks s) (text s "Respective" (current-font-size)))

(define-syntax-rule (wit . forms) (parameterize ([current-main-font "Inconsolata"]) . forms))
(define (ic s) (wit (t s)))
(define (iic s) (wit (it s)))

(define-syntax-rule (wkt . forms) (parameterize ([current-main-font "Kaushan Script"]) . forms))
(define (kt s) (wkt (t s)))
(define (kit s) (wkt (it s)))

(define use-pdf? #t)
(define-syntax-rule (error-bitmap e) (error 'todo "Render bitmap ~a" 'e))
(define-runtime-path neu-path "neu-logo-bg.png")
(define-runtime-path one-ring-path "one-ring.png")
(define-runtime-path redex-path "../icfp2013/plt-redex.jpeg")
(define-runtime-path yada-path "yada.jpg")

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

(define-runtime-path shifting-path "shifting-gears.png")
(define-runtime-path bird-path "bird2.png")


(define redex-pict (scale (bitmap redex-path) 0.2))

(define c-neu '(#xad 0 0))
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
      (colorize @cbt{J. Ian Johnson} c-neu)
      (blank 1 80)
      (with-size 22 (colorize @ic{2015, March 30} c-neu))
      (blank-line))))))

(module+ slide-deck
  (provide introduction abstract-interpreters wonderful terrible
           thesis-slide talk-outline
           semantics omega step-omega big-states finitize finite-structure when-lookup yada
           stack-push relevance relevance-useful relevance-memoization
           lang-intro my-language oneness-intro oneness-problem oneness-solution)
  (define/staged introduction #:stages [make write DT popular weird foreward inscrutable]
    #:name 'intro
    (with-size 50
      (cc-superimpose
       (vl-append gap-size
                  (show @ic{People make their own languages.} (>= stage make))
                  (show @ic{They write interpreters.} (>= stage write))
                  (show @ic{Higher-order. Dynamically typed.} (>= stage DT))
                  (show @ic{Some languages become popular.} (>= stage popular))
                  (show (hc-append gap-size @ic{Lots of code that} @iic{does stuff.})
                        (>= stage weird)))
       (show
        (shadow-frame
         (colorize
          (vr-append
           (with-size 45
             (vl-append @ic{So many new languages!}
                        @ic{So many new interpreters!}))
           (with-size 28 @iic{~ Hal Abelson, foreword to EOPLv3}))
          "midnight blue"))
        (>= stage foreward))
       (show
        (rotate
         (shadow-frame
          (colorize
           (vr-append @ic{So many ways to go wrong!}
                      @iic{~ me, right now})
           c-neu))
         (/ pi 16))
        (>= stage inscrutable)))))

  (define/staged abstract-interpreters #:stages [two-questions
                                                 do?
                                                 not-do?
                                                 test-lots
                                                 what? sure
                                                 types
                                                 not-types
                                                 AI]
    #:name 'two-questions
    (with-size 45
      (vl-append gap-size
                 (show @ic{Two questions a dev asks:} (>= stage two-questions))
                 (show @ic{Does it do what it's supposed to do?} (>= stage do?))
                 (show (cc-superimpose (blank 800 1)
                                       (colorize @ic{Test. Test. Test.} c-neu)) (>= stage test-lots))
                 (show (hc-append gap-size @ic{Does it} @iic{not}
                                  @ic{do what it's} @iic{not}
                                  @ic{supposed to do?})
                       (>= stage not-do?))
                 (cc-superimpose
                  (blank 800 1)
                  (vc-append gap-size
                             (pict-cond [(= stage what?) @ic{Test?}]
                                        [(>= stage sure) (hc-append gap-size
                                                                    (colorize @ic{Test.} c-neu)
                                                                    @ic{But what else?})])
                             (let ()
                               (define ty @ic{types})
                               (define base
                                 (show (hc-append gap-size @ic{Static}
                                                  (tag-pict (pict-if (< stage AI)
                                                                     ty
                                                                     (colorize @ic{analysis} "midnight blue"))
                                                            'types))
                                       (>= stage types)))
                               (define-values (x y) (lt-find base (list ty)))
                               (define bar
                                 (show
                                  (colorize
                                   (filled-rectangle (* 1.2 (pict-width ty)) 5)
                                   c-neu)
                                  (= stage not-types)))
                               (define-values (x2 y2) (mk-center x y ty bar))
                               (pin-over base x2 y2 bar)))))))

  (define/staged wonderful #:stages [understand flow
                                     harden security
                                     improve optimizations]
    #:name 'wonderful
    #:title (ic "Static analysis is wonderful!")
    (ht-append
     gap-size
     (vc-append
      (colorize @ic{Understand code} "forest green")
      (show
       (with-size 22
         (shadow-frame
          (table 1
                 (list @ic{Value flow}
                       @ic{Control flow}
                       @ic{Type inference})
                 lt-superimpose
                 lt-superimpose
                 gap-size gap-size)))
       (>= stage flow)))
     (show
      (vc-append
       (colorize @ic{Harden code} c-neu)
       (show
        (with-size 22
          (shadow-frame
           (table 1
                  (list @ic{Crash-freedom}
                        @ic{Contract verification}
                        @ic{Information flow}
                        @ic{Data race detection})
                  lt-superimpose
                  lt-superimpose
                  gap-size gap-size)))
        (>= stage security)))
      (>= stage harden))
     (show
      (vc-append
       (colorize @ic{Improve code} "midnight blue")
       (show
        (with-size 22
          (shadow-frame
           (table 1
                  (list @ic{no runtime enforcement}
                        @ic{optimization})
                  lt-superimpose
                  lt-superimpose
                  gap-size gap-size)))
        (>= stage optimizations)))
      (>= stage improve))))

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
                                            crit-easy language]
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
                           @ic{already wasted}))
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
                            (lt-superimpose ms (fade-pict n3 (ghost overline) overline)))
                          ms)
                      (fade-pict n3 (ghost colon) colon)))
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
                                               (+ (* (- 1 n2) ms-x) (* n2 ls-x)) ms-y
                                               (hc-append ms:
                                        ;(blank 20 1)
                                                          (if (>= stage (+ st crit-right))
                                                              rs
                                                              (blank 0)))))
                           (>= stage st)))
                   (if (< stage move-criteria)
                       (pin-over
                        middle-right
                        (+ (* (- 1 n1) ls-x)
                           (* n1 (- (- (pict-width ls))
                                    ;; magic number to make it go offscreen
                                    80)))
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
          (let* ([p0 (place 0)]
                 [frm
                  (cond
                   [(= stage language)
                    (shadow-frame
                     (colorize @ic{I made a language} c-neu))]
                   [else (blank)])]
                 [res (cc-superimpose p0 frm)])
            res))))

  (define thesis-pict
    (parameterize ([current-font-size 30])
      (in-sawasdee
       (vc-append
        gap-size
        (hc-append (tag-pict (hc-append (tag-pict (t "Precise ") 'precise) (t "and ") (tag-pict (t "performant") 'perf)) 'measure)
                   (t " analyses for higher-order languages"))
        (hc-append (t "can be ") (tag-pict (t "systematically and algorithmically constructed") 'systematic))
        (hc-append (t " from their ") (tag-pict (t "semantics.") 'semantics))))))

  (define/staged thesis-slide #:stages [statement built measure semantics]
    #:name 'thesis
    (with-size 30
      (in-sawasdee
       (define bg
         (tagged-shadow-frame
          (hilight-tag
           (hilight-tag
            (hilight-tag
             thesis-pict lt-find 'systematic #:show (<= built stage measure))
            lt-find 'measure #:show (= stage measure))
           lt-find 'semantics #:show (= stage semantics))
          (inset (big (bt "Thesis:")) 10)))
       (define build-text (big (t "I built and proved")))
       (define build-arrow
         (pin-arrow-line 15
                         (pin-over bg -30 300 build-text)
                         build-text ct-find
                         (first (find-tag bg 'systematic)) cb-find
                         #:start-angle (* 1/3 pi)
                         #:end-angle (* 2/3 pi)))
       (match stage
         [(or (== statement) (== semantics)) bg]
         [(== built) build-arrow]
         [(== measure)
          (define measure-text (big (t "I evaluated")))
          (pin-arrow-line 15
                          (pin-over build-arrow 100 -200 measure-text)
                          measure-text cb-find
                          (first (find-tag bg 'measure)) ct-find
                          #:start-angle (* -1/3 pi)
                          #:end-angle (* -1/2 pi))]))))

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
                (F (ic "Conclusion/future work") related))))

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

  (define/staged omega #:stages [lets alsoUU as-beta forever initial]
    #:name 'eval-example
    (define lets-pict (with-size 40 (hc-append (ic "Let's evaluate ") (ct "(R 0)") (ic " where "))))
    (with-size 40
      (vl-append gap-size
                 (hc-append lets-pict (ct "R = (λ (k) (cons k (R k)))"))
                 (show (hc-append (ct "(R 0) ↦β (cons 0 (R 0))")
                                  (show (ct " ↦β (cons 0 (cons 0 (R 0)))…")
                                        (>= stage forever)))
                       (>= stage as-beta))
                 (show (ct "CESK initial state: ev〈(R 0) ρ₀ σ₀ []〉") (>= stage initial))
                 (show (ct "ρ₀ = [R ↦ a₀]〉") (>= stage initial))
                 (show (hc-append (ct "σ₀ = [a₀ ↦ 〈") (kt "B") (ct ",ρ₀〉]")) (>= stage initial))
                 (show (hc-append (kt "B") (ct " = (λ (k) (cons k (R k)))")) (>= stage initial)))))

  (define step-omega
    (let ()
      (define states
        (list "ev〈(R 0) ρ₀ σ₀ []〉"
              "ev〈R ρ₀ σ₀ [(appL 0 ρ₀)]〉"
              "co〈[(appL 0 ρ₀)] 〈B,ρ₀〉 σ₀〉"
              "ev〈0 ρ₀ σ₀ [(appR 〈B,ρ₀〉)]〉"
              "co〈[(appR 〈B,ρ₀〉)] 0 σ₀〉"
              "ev〈(cons k (R k)) ρ₀[k ↦ a₁] σ₀[a₁ ↦ 0] []〉"
              "ev〈k ρ₁ σ₁ [(consL (R k) ρ₁)]〉"
              "co〈[(consL (R k) ρ₁)] 0 σ₁〉"
              "ev〈(R k) ρ₁ σ₁ [(consR 0)]〉"
              "ev〈R ρ₁ σ₁ [(appL k ρ₁),(consR 0)]〉"
              "co〈[(appL k ρ₁),(consR 0)] 〈B,ρ₀〉 σ₁〉"
              "ev〈k ρ₁ σ₁ [(appR 〈B,ρ₀〉),(consR 0)]〉"
              "co〈[(appR 〈B,ρ₀〉),(consR 0)] 0 σ₁〉"))
      (define (prepend-B str)
        (hc-append 0 (kt "B") (ct str)))
      (define ((str->pict prepend) str)
        ;; replace B with metavariable
        (define strs (string-split str "B"))
        (apply hc-append 0 (cons 
                            (ct (string-append prepend (first strs)))
                            (map prepend-B (rest strs)))))
      (define picts
        (with-size 50
          (append
           (cons ((str->pict "  ") (first states)) (map (str->pict "↦ ") (rest states)))
           (list
            (vl-append gap-size (ct "↦ ev〈(cons k (R k)) ρ₀[k ↦ a₂] σ₁[a₂ ↦ 0]")
                       (ct "         [(consR 0)]〉"))))))
      (wtable 1 picts lc-superimpose lc-superimpose 5 5
              #:title (with-size 50 (hc-append (kt "B") (ct " = (λ (k) (cons k (R k)))")))
              #:dilate fast-start)))

  (define/staged big-states #:stages [big prob1 prob2 prob3 solution expl1 expl2]
    #:name 'big-states
    #:title (with-size 50 (ic "The state space gets big"))
    (with-size 50
     (vl-append gap-size
                (show (ic "Problem 1: ∞ addresses") (>= stage prob1))
                (show (ic "Problem 2: ∞ structure") (>= stage prob2))
                (show (ic "Problem 3: ∞ values (e.g. ℕ)") (>= stage prob3))
                (blank 30)
                (show (ic "Solution: finitize (implicit) allocation") (>= stage solution))
                (show (ic "Bounded allocation bounds state space") (>= stage expl1))
                (show (ic "↦ defines a finite graph") (>= stage expl2)))))

  (define/staged finitize #:stages [meaning reuse alive before now both either or-both]
    #:name 'finitize
    (with-size 48
      (pin-over
       (vl-append gap-size
                  @ic{What does it mean to finitize allocation?}
                  (show @ic{Some allocations will reuse addresses.} (>= stage reuse))
                  (show @ic{Old usage not dead!} (>= stage alive))
                  (hc-append 0 (show @ct{[α(a₁) ↦ v₁]} (>= stage before)) (show @ct{⊔[α(a₂) ↦ v₂]} (>= stage now)))
                  (show @ct{[â₁ ↦ {v₁, v₂}]} (>= stage both))
                  (show (hc-append gap-size @ic{Lookup} @ct{â₁} @ic{means} @iic{either} @ct{v₁} @iic{or} @ct{v₂}
                                   (show @ic{(or both)} (>= stage or-both)))
                        (>= stage either)))
       550 125
       (show (pdf->pict addresses-path 0.7)
             (>= stage reuse)))))

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

   (define/staged when-lookup #:stages [when? aam example-rule example-expr fanout me lazy]
     (vc-append gap-size
      @ic{Question: when do we lookup?}
      (show (hc-append gap-size @ic{AAM: whenever a rule says.}) (>= stage aam))
      (show (hc-append gap-size @ct{〈x ρ σ κ〉 ↦ 〈v ρ' σ κ〉} @ic{where} @ct{〈v,ρ'〉 ∈ σ(ρ(x))}) (>= stage example-rule))
      (show (code (f x y)) (>= stage example-expr))
      (show (force fanout-pict) (>= stage fanout))
      (show (hc-append gap-size @ic{Me: whenever a looked up value is} @iic{inspected.}) (>= stage me))
      (show (vc-append (arrow 20 (/ pi -2))
                       (force lazy-pict))
            (>= stage lazy))))

   (define/staged yada #:stages [elaine graph clear abscomp compiled timestamp replay
                                 simple1 simple2 simple3 ugly compiler]
     #:name 'yada
     (cc-superimpose
      (pin-over
       (pin-over
        (cc-superimpose
         (pin-over
          (pin-over
           (pin-over
            (pin-over
             (pin-over
              (cc-superimpose
               (pin-balloon (wrap-balloon (vl-append gap-size @ic{Yada} @ic{yada} @ic{yada...})
                                          'se 50 75)
                            (bitmap yada-path)
                            ;; Elaine mouth coordinates
                            242 257)
               (show (force speedup-pict) (>= stage graph)))
              -100 -10
              (show (scale (bitmap iswim-path) 0.7) (>= stage clear)))
             350 0
             (show (scale (bitmap abscomp-path) 0.85) (>= stage abscomp)))
            350 400
            (show (scale (bitmap compiled-path) 0.85) (>= stage compiled)))
           500 0
           (show (bitmap timestamp-path) (>= stage timestamp)))
          500 300
          (show (bitmap replay-path) (>= stage replay)))
         (show (with-size 60 (shadow-frame @ic{Simple transform})) (>= stage simple1)))
        -50 0
        (show (rotate (shadow-frame (with-size 40 @ic{Simple transform})) (/ pi 16))
              (>= stage simple2)))
       550 20
       (show (rotate (shadow-frame (with-size 30 @ic{Simple transform})) (/ pi -16))
             (>= stage simple3)))
      (vc-append
       gap-size
       (show (shadow-frame (with-size 100 (colorize @kt{Ugly mess} c-neu)))
             (>= stage ugly))
       (show (shadow-frame (with-size 100 (colorize @kt{Human compiler} c-neu)))
             (>= stage compiler)))))

   (define/staged lang-intro #:stages [general built-in functions allocation sound concrete]
     #:name 'lang-intro
     #:title (with-size 50 @ic{I made a language})
     (with-size 50
       (vl-append gap-size
                  (blank 30)
                  (show
                   (hc-append gap-size
                              @iic{Pattern}
                              @ct{↦}
                              @iic{Expression}
                              @iic{[Side-conditions]})
                   (>= stage general))
                  (blank 30)
                  (show @ic{Equality is built-in}
                        (>= stage built-in))
                  (show @ic{First-order metafunctions} (>= stage functions))
                  (show @ic{All allocation is mediated}
                        (>= stage allocation))
                  (show @ic{Any allocation strategy is sound}
                        (>= stage sound))
                  (show @ic{Fresh allocation ensures completeness}
                        (>= stage concrete)))))

   (define-syntax left-nest
     (syntax-rules ()
       [(_ fn base) base]
       [(_ fn base [args ...] rest ...)
        (left-nest fn (fn base args ...) rest ...)]))

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
             "p \\in \\mathit{Pattern} &::= x \\texttt{:=} p \\mid \\texttt{(}n\\ p \\ldots\\texttt{)}"
             "\\mid \\texttt{Address} \\mid \\texttt{(External}\\ E\\texttt{)} \\mid \\texttt{\\_} \\\\"

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
        (vc-append @ct{Contexts}
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

   (define/staged relevance-useful #:stages [useful list trick]
     #:name 'relevance-useful
     #:title (with-size 70 @kt{Relevance is useful})
     (with-size 50
       (cc-superimpose
        (show
         (vl-append gap-size
                    @ic{Garbage collection}
                    @ic{Access control security}
                    @ic{Continuation marks}
                    (hc-append (blank 40) @ic{Configuration})
                    (hc-append (blank 40) @ic{User-programmable context}))
         (>= stage list))
        (show (shadow-frame @ic{Stack property ∈ Context}) (>= stage trick)))))

   (define/staged relevance-memoization #:stages [base diff then subst]
     #:name 'relevance-memoization
     #:title (with-size 70 @kt{Relevance justifies short-circuiting})
     (with-size 50
      (define expr (colorize @ct{e} "darkgreen"))
      (define ctx (colorize (filled-ellipse 20 20) "darkgreen"))
      (define out (colorize @ct{〈v,σ〉} "darkgreen"))
      (define cont1 (colorize @ct{cont} "darkgreen"))
      (define cont2 (colorize @ct{cont} "cadet blue"))
      (vl-append
       gap-size
       (hc-append @ic{Run } expr @ic{ in }
                   ctx @ic{ with } cont1
                   @ic{ to } out @ic{ with } cont1)
       (show
        (hc-append @ic{Later see } expr @ic{ in } ctx @ic{ with } cont2)
        (>= stage diff))
       (show
        (hc-append @ic{Immediately jump to } out @ic{ with } cont2)
        (>= stage then))
       (show @ic{Irrelevance means substitutability}
        (>= stage subst)))))

   (define/staged oneness-intro #:stages [uses relation equality]
     #:title (with-size 70 @kt{The importance of “oneness”})
     #:name 'oneness
     (with-size 50
       (vl-append
        gap-size
        (vl-append (hc-append @ic{Observable allocation})
                   (blank 15)
                   (hc-append (blank 50) @tt{ref ()} @ic{, } @tt{(gensym)} @ic{, etc.})
                   (show @ic{Oneness and sameness are related} (>= stage relation))
                   (show @ic{Equality motivated this language} (>= stage equality))))))

   (define/staged oneness-problem #:stages [question of-course follow-up abs concretization ctx ctx2]
     (with-size 50
       (vl-append
        gap-size
        (show (hc-append @ic{Question: Does } @ct{a} @ic{ equal } @ct{a} @ic{?})
              (>= stage question))
        (show @kt{Of course!} (>= stage of-course))
        (blank 20)
        (show (hc-append @ic{Question: Does } @ct{â} @ic{ equal } @ct{â} @ic{? }
                         (pict-cond
                          [(or (= stage abs) (= stage concretization)) @ic{Well...}]
                          [(= stage ctx) (colorize @ic{Yes!} "firebrick")]
                          [(= stage ctx2) (colorize @ic{Maybe?} "olive")]))
              (>= stage follow-up))
        (hc-append (show (pdf->pict addresses-path) (>= stage abs))
                   (pict-cond #:combine lt-superimpose
                    [(= stage concretization) (pdf->pict gamma-path)]
                    [(= stage ctx) (pdf->pict gamma1-path)]
                    [(= stage ctx2) (pdf->pict gammaω-path)])))))
   
   (define/staged oneness-solution #:stages [how proof]
     (with-size 50
       (vl-append
       gap-size
       (show (hc-append @ic{Semantics tracks oneness [Might 2006]}) (>= stage how))
       (show (hc-append @ic{Bisimilarity with concrete semantics}) (>= stage proof)))))
)

(module+ sections
  (require (submod ".." slide-deck))
  (provide intro-to-thesis semantics-to-aam example finite pushdown small-pdcfa
           shifting-gears AAML related lazy redex-great)
  (define (intro-to-thesis)
    (title)
    (run-stages introduction)
    (run-stages abstract-interpreters)
    (run-stages wonderful)
    (run-stages (terrible #f))
    (run-stages thesis-slide #:stage '(statement built measure)))

  (define (semantics-to-aam)
    (run-stages semantics)
    (slide #:name 'aam?
           (with-size 60 (vc-append gap-size @iic{Abstract}
                                    @ic{abstract machines?})))
    (run-stages (grinder #t) #:stage '(static big-semantics (output . 0)))
    (run-stages what-is-CESK))

  (define (example)
    ;; We've got some time. Let's evaluate Ω.
    ;; AAM in 3 slides.
    ;; Step 1: identify allocation points.
    (run-stages omega)
    (run-stages step-omega)
    (run-stages big-states))
  
  (define (finite)
    ;; I use slightly different language than orthodox, so
    ;; this isn't entirely beating a dead horse.
    ;; Step 2: finitize allocation
    ;; Step 3: join and nondeterminism
    (run-stages finitize)
    (run-stages finite-structure)
    (slide (with-size 72
             (vc-append gap-size
                        @kt{Generally, finitize structure.}
                        @kt{But, the stack is special.}))))

  (define (small-pdcfa)
    (slide
     #:title (with-size 50 @kt{That's it.})
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
        0.4)))))

(define (pushdown)
    (run-stages talk-outline #:stage 'pushdown)
    (run-stages why-not-aam)
    (slide (with-size 60 (vl-append @kt{Insight:}
                                    @kt{delimit computations &}
                                    @kt{catalog contexts by revelant state})))
    (run-stages fix-aam)
    (run-stages fix-zoom)
    (run-stages relevance)
    (run-stages relevance-useful)
    (run-stages relevance-memoization)
    (small-pdcfa))

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


    (define (lazy)
      (run-stages when-lookup)
      (run-stages yada))

    (define (AAML)
;      (run-stages lang-intro)
;      (run-stages my-language)
      (run-stages oneness-problem)
      (run-stages oneness-solution)
      (slide (pin-over (blank SCREEN-WIDTH SCREEN-HEIGHT)
                       -20 -20
                       (bitmap one-ring-path)))
      
      #|
    
      What do I want to get across with the language design?
      I have metafunctions, I have external values/metafunctions.

      ;; If the env were in the monad, match would be able to hide the choose-refinement bit.
      ;; Monad operations: return, bind, resolve, update, lookup,
      ;; choose-refinement, alloc, make-variant
    
      |#

    )

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
           (with-size 50
             (vl-append
              gap-size
              @ct{Four categories:}
              (ht-append
               gap-size
               (blank 40)
               (vl-append gap-size
                          @ct{Analysis implementation}
                          @ct{Pushdown analysis}
                          @ct{Semantic frameworks}
                          @ct{Analysis synthesis})))))

    (slide #:title (with-size 60 @ic{Related Work (Implementation)})
           #:name 'rw-impl
           @t{Olin's kCFA}
           @t{Constraint-based analysis [Heintze 93] [Steckler,Wand 96]}
           @t{Abstract compilation [Boucher,Feeley 96]}
           @t{Structural AI (Astrée)})

    (slide #:title (with-size 60 @ic{Related Work (Semantics)})
           #:name 'rw-semantics
           @t{PLT Redex}
           @t{K framework}
           @t{Term reduction systems})
  
    (slide #:title (with-size 60 @ic{Related Work (Synthesis)})
           #:name 'rw-synth
           @t{Flow logic}
           @t{Rhodium}
           @t{HOIST}
           @t{PostHat and all that})

    (slide #:title (with-size 60 @ic{Future work})
           #:name 'future
           @ic{Synthesize allocators: type system identifies recursion}
           @ic{Explicit allocation: adjunction type}
           @ic{OAAM techniques}
           @ic{Linguistic support for pushdown}
           @ic{Abstraction testing}
           'next
           @ic{Black holes?})))

(module+ main
  (require (submod ".." slide-deck)
           (submod proposal/presentation slide-deck)
           (submod ".." sections))

  (intro-to-thesis)
  (run-stages talk-outline)
  (run-stages thesis-slide #:stage 'semantics)
  (semantics-to-aam)
  (example) ;;
  (finite)
  ;; Finitizing the stack is actually not necessary.
  (pushdown)

  (run-stages talk-outline #:stage 'oaam)
  (run-stages (grinder #t) #:stage '(output faster-output))
  (lazy)

  (shifting-gears)
  
  (AAML)
  
  (related)
  (run-stages (terrible #t) #:stage 'crit-easy)
  (run-stages parting)

  ;; Below the fold, for private session only

  (slide (with-size 40 (ic "Proposal vs. Dissertation"))
         (ht-append gap-size
                    (vl-append (ic "Proposal")
                               (ic "Sparseness")
                               (ic "Pushdown shift/reset")
                               (ic "Pushdown stack inspection")
                               (ic "Verify Temporal HO-contracts"))
                    
                    (vl-append (ic "Dissertation")
                               (ic "Cut")
                               (ic "Proved/unevaluated")
                               (ic "Proved/unevaluated")
                               (ic "Semantics / language"))))

  (redex-great)
  )

(module+ test
  (require (submod ".." slide-deck)
           (submod ".." sections))

  (small-pdcfa))
