#lang at-exp slideshow

(require unstable/gui/slideshow
         "poppler-main.rkt"
         unstable/gui/ppict
         net/sendurl
         slideshow/code
         slideshow/flash
         slideshow/play
         racket/gui/base
         scheme/runtime-path
         slideshow/balloon slideshow/face
         (rename-in "pict-helpers.rkt" [addr paddr])
         (submod "semantics.rkt" slide-deck))
(set-page-numbers-visible! #f)
#|
Outline:
Soundness, precision, performance, maintainability, extensibility,
   (Fritz would add "goodness": unimprovability, predictibility under program transformations,
                                scale well with parameters, adaptive)
The math of static analysis tends to imply the first, and half of the second
   (since real precision comes from evaluation)
This talk is about Soundness, performance, and maintainability.
  Precision is orthogonal. Extensibility is easy in the AAM approach (see monadic abstract interpretation).
We give a recipe of transformations to semantics to interpret them faster, with easy proofs of correctness.
We do not claim to have the most novel and performant ways to implement analyses;
   we are giving the community tools to implement reasonable static analyses
   without necessarily depending on scary, untrustworthy engineering.
Paper (focus on global store):
 Store-allocate values
 Frontier-based semantics
 Store-counting
 Lazy non-determinism
 Abstract compilation
 Locally log-based store-deltas
 Representation amenable to imperative implementation
Philosophy: make your algorithmic trickiness part of the semantics. Nonstandard not just for the result, but for computing the result.
Write your semantics (〈e, ρ, σ, κ, t〉, ↦) transform your semantics. Implement your semantics.
|#

(define (showif p b) (if b p (ghost p)))

;; Should be in sync with texpict/code (used by pict/code)
(define comma @tt{, })
(paren-color "brown")

(define bg-slide-assembler
  (lambda (title sep content)
    (inset content (- margin) (- margin) 0 0)))

(define-syntax-rule (scode e ...)
  (parameterize ([current-font-size 28])
    (code e ...)))

(current-main-font "Inconsolata")
(define-runtime-path logo-path "prl-logo.png")
(define-runtime-path aam-path "vanhorn2010abstract.pdf")
(define-runtime-path grinder-path "grinder.png")
(define-runtime-path speedup-path "all-relative-time.pdf")
(define-runtime-path fanout-path "fanoutdot2.pdf")
(define-runtime-path lazy-path "lazydot.pdf")
(define-runtime-path graph0-path "introspective-base.pdf")
(define-runtime-path graph1-path "introspective-lazy.pdf")
(define-runtime-path graph2-path "introspective-lazyc.pdf")
(define-runtime-path comp-store-path "comp-store.pdf")

(define (title)
 (parameterize ([current-slide-assembler bg-slide-assembler])
   (slide
    (cc-superimpose
     (bitmap logo-path)
     (vc-append
      (big (bold (para #:align 'center "Optimizing Abstract Abstract Machines")))
      (blank-line)
      (para #:align 'center
            (colorize @bt{J. Ian Johnson} "navy")
            ", Nicholas Labich, "
            (colorize @t{Matt Might} "brown")
            " and David Van Horn")
      (small (para #:align 'center (tt "{ianj,labichn,dvanhorn}@ccs.neu.edu, matt@might.net")))
      (blank 20)
      (para #:align 'center (vc-append (colorize @t{Northeastern University} "navy")
                                       (colorize @t{Boston, MA, USA} "navy"))
            (colorize (vc-append @t{University of Utah}
                                 @t{Salt Lake City, UT, USA})
                      "brown")))))))

(define (greent txt) (colorize (t txt) grn))
(define (oranget txt) (colorize (t txt) "orange"))
(define (light-gray txt) (colorize (t txt) (list 200 200 200)))
(define tyellow (make-object color% #xFF #xFF #x00 0.3))

(define (intro stage)
 (slide (cc-superimpose
         (with-size 60 (para #:align 'center @it{Abstract} @t{abstract machines?}))
         (showif (shadow-frame (scale (page->pict aam-path) 0.7)) (= stage 1)))))

(define (grinder stage)
  (λ (n)
     (define in-anchor (blank 0))
     (define out-anchor (blank 0))
     (define analysis ((if (= stage 0) values frame)
                       (with-size 48 @t{Analysis})))
     (define program (showif @t{Program} (= stage 1)))
     (define report (showif @t{Report} (= stage 1)))
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
            (pin-arrow-line 15 analysis-box analysis cb-find report ct-find)
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
     (pin-arrow-line 15
                     (pin-arrow-line 15 anchored-grinder semantics cb-find
                                     in-anchor ct-find)
                     out-anchor rc-find
                     analysis lc-find)))

(define (analysis-qualities stage)
  (define-values (intro show-qualities fritz ext-show aam oaam) (apply values (range 6)))
  (define sound @t{Soundness})
  (define prec @t{Precision})
  (define perf @t{Performance})
  (define maint @t{Maintainability})
  (define ext* @t{Extensibility})
  (define ext (tag-pict (show (if (> stage ext-show) (colorize ext* "gray") ext*) (> stage intro)) 'ext))
  (define cite (hc-append (blank 50) (with-size 24 @t{[Sergey, et al. PLDI 2013]})))
  (define henglein (t "Henglein's \u201cgoodness\u201d"))
  (define base
    (vc-append gap-size
               (show sound (> stage intro))
               (show (if (= stage aam) (colorize @it{Precision} "dimgray") prec) (> stage intro))
               (if (= stage aam) (colorize perf "gray") perf)
               (show maint (> stage intro))
               ext
               (showif (if (>= stage aam) (colorize henglein "gray") henglein) (>= stage fritz))
               (show
                (cond
                 [(= stage aam) (with-size 72 (colorize @t{AAM} "firebrick"))]
                 [else (with-size 72 (colorize @t{OAAM} "medium forest green"))])
                (>= stage aam))))
  (slide #:title "A good static analysis has"
         (pin-over
          base
          (find-tag base 'ext)
          (λ (p path) (define-values (x y) (rt-find p path)) (values x (- y 10)))
          (show (if (> stage ext-show) (colorize cite "gray") cite)
                        (>= stage ext-show)))))

(define (talk-focus stage)
  (define in-talk item)
  (define in-paper (if (>= stage 1) (λ (p) (colorize (item p) "gray")) item))
  (slide (with-size
          48
          (parameterize ([current-gap-size (* 2 gap-size)])
            (cc-superimpose
             (vl-append
              (in-paper @t{Store-allocate values})
              (in-paper @t{Frontier-based semantics})
              (in-paper @t{Store-counting})
              (in-talk @t{Lazy non-determinism})
              (in-talk @t{Abstract compilation})
              (in-talk @t{Locally log-based store-deltas})
              (in-paper @t{Mutable frontier and store}))
             (showif (shadow-frame (colorize @t{Goal: Directly implementable math} "firebrick")) (= stage 2)))))))

(define (bench-overview stage)
  (define AAM-point (blank 0))
  (define OAAM-point (blank 0))
  (define AAM-text (with-size 48 @t{AAM}))
  (define OAAM-text (with-size 48 @t{OAAM}))
  (define-values (AAM-x AAM-y) (values 40 245))
  (define-values (OAAM-x0 OAAM-y0) (values 760 0))
  (define-values (OAAM-x1 OAAM-y1) (values 768 175))
  (define speedup
    (pin-over
     (pin-over
      (pin-over
       (pin-over
        (pin-over
         (page->pict speedup-path) OAAM-x0 OAAM-y0 (ellipse 15 OAAM-y1))
        OAAM-x1 OAAM-y1 OAAM-point)
       AAM-x AAM-y AAM-point)
      (- AAM-x (pict-width AAM-text)) (+ AAM-y (* 2 (pict-height AAM-text))) AAM-text)
     (- OAAM-x1 (pict-width OAAM-text)) (+ AAM-y (* 2 (pict-height AAM-text))) OAAM-text))
  (cc-superimpose
   (pin-arrow-line 15
                   (pin-arrow-line 15 speedup AAM-text ct-find AAM-point cb-find)
                   OAAM-text ct-find OAAM-point cb-find
                   #:end-angle (/ pi 2))
   (vc-append 20
              (show (shadow-frame (with-size 48 (colorize @t{Competitive performance,}
                                                          "medium forest green")))
                    (> stage 0))
              (show (shadow-frame (with-size 48 (colorize @t{implementation takes minimal effort,}
                                                          "medium forest green")))
                    (> stage 1))
              (show (shadow-frame (with-size 48 (colorize @t{and we have simple proofs}
                                                          "firebrick")))
                    (> stage 2)))))


 (title)

 (for ([i 2]) (intro i))

 (slide ((grinder 0) 0))
 (play (grinder 1) #:steps 100)
 (slide ((grinder 1) 1))

 (for ([i 4]) (slide (bench-overview i)))

 (for ([i 6]) (analysis-qualities i))
 (for ([i 3]) (talk-focus i))

 (slide (with-size 60 @t{First: recap of AAM}))

 (parameterize ([pushdown? #f])

   (slide (semantics))
   (slide (the-shift))
   (slide (CESK-table sCEK))
   (slide (CESK-table sCESK))

   (slide (CESK-table saCESK))
   (slide (cc-superimpose
           (CESK-table saCESK)
           (shadow-frame (with-size 50 @t{Lazy non-determinism?}))))
   (slide (CESK-table lazy))

   (slide (code (f x y))
    (page->pict fanout-path)
    'next
    (vc-append (arrow 20 (/ pi -2))
               (page->pict lazy-path)))
   (slide (hc-append (* 2.5 gap-size)
                     (scale (page->pict graph0-path) 0.15)
                     (arrow 20 0)
                     (scale (page->pict graph1-path) 0.15)))

   (define (boucher-feeley p)
     (rt-superimpose
      p
      (with-size 24 (colorize @t{[Boucher & Feeley CC 1996]} id-color))))
   (slide (boucher-feeley
           (cc-superimpose
            (CESK-table saCESK)
            (shadow-frame (with-size 50 @t{Abstract compilation?})))))
   (slide (boucher-feeley (CESK-table abscomp)))

   (define (drama stage)
     (define junk (vector (code Lorem)
                          (code ipsum)
                          (code dolor)
                          (code sit)
                          (code amet)
                          (code consectetuer)
                          (code adipiscing)
                          (code elit)
                          (code sed)
                          (code diam)
                          (code nonummy)
                          (code nibh)
                          (code euismod)
                          (code tincidunt)
                          (code ut)
                          (code laoreet)
                          (code dolore)
                          (code magna)
                          (code aliquam)
                          (code erat)
                          (code volutpat)
                          (code Ut)
                          (code wisi)
                          (code enim)
                          (code ad)
                          (code minim)
                          (code veniam)
                          (code quis)
                          (code nostrud)
                          (code exerci)
                          (code tation)
                          (code ullamcorper)
                          (code suscipit)
                          (code lobortis)
                          (code nisl)
                          (code ut)
                          (code aliquip)
                          (code ex)
                          (code ea)
                          (code commodo)
                          (code consequat)
                          (code Duis)
                          (code autem)
                          (code vel)
                          (code eum)
                          (code iriure)
                          (code dolor)
                          (code in)
                          (code hendrerit)
                          (code in)
                          (code vulputate)
                          (code velit)
                          (code esse)
                          (code molestie)
                          (code consequat)
                          (code vel)
                          (code illum)
                          (code dolore)
                          (code eu)
                          (code feugiat)
                          (code nulla)
                          (code facilisis)
                          (code at)
                          (code vero)
                          (code eros)
                          (code et)
                          (code accumsan)
                          (code et)
                          (code iusto)
                          (code odio)
                          (code dignissim)
                          (code qui)
                          (code blandit)
                          (code praesent)
                          (code luptatum)
                          (code zzril)
                          (code delenit)
                          (code augue)
                          (code duis)
                          (code dolore)
                          (code te)
                          (code feugait)
                          (code nulla)
                          (code facilisi)
                          (code Nam)
                          (code liber)
                          (code tempor)
                          (code cum)
                          (code soluta)
                          (code nobis)
                          (code eleifend)
                          (code option)
                          (code congue)
                          (code nihil)
                          (code imperdiet)
                          (code doming)
                          (code id)
                          (code quod)
                          (code mazim)
                          (code placerat)
                          (code facer)
                          (code possim)
                          (code assum)
                          (code Typi)
                          (code non)
                          (code habent)
                          (code claritatem)))
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
                      ([(indent) 11]
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
   (slide (with-size 22 (drama 0)))
   (define fake-match (with-size 3 (drama 1)))
   (slide fake-match)

   (slide (cc-superimpose
           fake-match
           (shadow-frame @t{Compile away dispatch overhead})))

   (slide (vc-append
           (hc-append (t "\u27e6\u2022\u27e7") @tt{:} (t "Expr \u2192 Env \u00d7 Store \u00d7 Kont \u2192 State"))
           (parameterize ([abs-comp? #t]) (CESK-table saCESK))))
   (slide (hc-append (* 2.5 gap-size)
                     (scale (page->pict graph1-path) 0.15)
                     (arrow 20 0)
                     (scale (page->pict graph2-path) 0.15)))

   (define ((proof-discussion stage) n)
     (vc-append gap-size
                @t{The proof is interesting (a simple use of WEBs).}
             @t{It is a complete abstraction,}
             @t{but not if the store is global.}
             (show
              @t{Roughly, stores don't line up} (> stage 0))
             (show (pin-over
                    (pin-over (blank 300)
                              (* n 125) 0 (rectangle 100 200))
                    (- 300 (* n 175)) 0 (scale (page->pict comp-store-path) 0.4642))
                   (> stage 0))))
   (slide ((proof-discussion 0) 0))
   (play (proof-discussion 1) #:steps 15)
   (slide ((proof-discussion 1) 1))

   (slide (cc-superimpose
           (CESK-table saCESK)
           (shadow-frame (with-size 50 @t{Store deltas?}))))
   (slide (CESK-table σ-big))

   )

 (slide #:title (with-size 40 (hc-append @t{Store widening } (bold @t{(WIP slide)})))
          (item @t{All explored states share one store})
          (item @t{Reusing ↦ produces full stores})
          (item @t{The next store is the ⨆ of those}))

(define (dinky)
  (code (code:line (define (dinky-helper-function x lst)
                     (map (λ (y) (- y x)) lst)))))
(slide (code ...
             #,(dinky)
             ...))

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
         (typeset-code
          (datum->syntax
           #f
           (for/fold ([acc '()]) ([form (in-list all-forms-rev)]
                                  [i (in-naturals)])
             (define (itag p) (tag-pict p (formi i)))
             (cond
              [(= i (quotient len 2))
               (list* (itag form)
                      #'(code:line) (tag-pict (dinky) 'helper) acc)]
              [else (cons (itag form) acc)]))))
         len)))))

(define context
  (let ()
    (define-values (p* len) (with-size 3 (a-program)))
    (define p
      (pin-under-all p* 'helper
                     (filled-flash-frame
                      (ghost (with-size 3 (dinky)))
                      #:bgcolor (make-object color% #xFF #xFF #x00 0.8))))
    (define (bump base path [size 10])
      (define-values (x y) (lt-find base path))
      (define bw (pict-width base))
      (define h (pict-height (car path)))
      (pin-under base 0 y
                 (pin-over
                  (frame (colorize (filled-rectangle (+ size bw) h) "white"))
                  1 0
                  (colorize (filled-rectangle (- bw 2) (add1 h)) "white"))))
    (λ (stage)
       (cond [(= stage 0) p]
             [(= stage 1)
              (define helper-path (find-tag p 'helper))
              (define-values (x y) (rt-find p helper-path))
              (define impact @t{Small impact on σ})
              (pin-arrow-line
                      15
                      (pin-over p (+ x (/ (pict-width impact) 2))
                                (- y (* 2 (pict-height impact)))
                                impact)
                      impact lb-find (car helper-path) rt-find)]
             [(= stage 2)
              (pin-under p 0 0 (rectangle (pict-width p) (pict-height p)))]
             [(= stage 3)
              (pin-under (bump p (find-tag p 'helper))
               0 0 (rectangle (pict-width p) (pict-height p)))]
             [(= stage 4)
              (for/fold ([p* p]) ([i (in-range len)])
                (pin-under (bump p (find-tag p ()))))]))))
(for ([i 4]) (slide (context i)))

(talk-focus 1)
(talk-focus 0)
(slide #:title "Before we part"
       @t{A few guidelines for implementing analyses}
       @t{Intuitive, simple, and competitive}
       'next
       @t{A simple matter of engineering?}
       @t{Build your tricks into the semantics}
       'next
       (blank 70)
       (with-size 60 (colorize (bold @t{Thank you}) "dark slate blue")))