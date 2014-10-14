#lang at-exp slideshow

#|
EXTERNAL DEPS:
Packages (removable with racket-poppler? = #f and require poppler-main instead of racket-poppler):
racket-poppler (raco pkg install racket-poppler)
slideshow-helpers (raco pkg install git://github.com/ianj/slideshow-helpers#master)
 (no longer rsvg : raco pkg install git://github.com/ianj/racket-rsvg#master)

Libraries (removable with use-pdf? = #f):
libpoppler

Fonts (free on fontsquirrel.com):
Cantarell
Kaushan Script
Inconsolata
Respective

Papers (removable dependency with use-pdf? = #f):
Abstracting Control (locally as "abstracting-control.pdf")
 url: http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=AEA0F8AF807727EB1353B012FC8D7E41?doi=10.1.1.43.8753&rep=rep1&type=pdf
;; (already in repo) Abstracting Abstract Machines (locally as "../icfp2013/vanhorn2010abstract.pdf")
Soft Contract Verification (locally as "soft-contract-verification.pdf")
|#

(require (except-in unstable/gui/slideshow stage)
         (for-syntax syntax/parse)
;         rsvg
         images/logos
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
(define (cit s) (text s (cons 'italic "Cantarell") (current-font-size)))

;(define (thanks s)  (text (string-downcase s) "Respective" (current-font-size)))
(define (thanks s) (text s "Respective" (current-font-size)))

(define (ic s) (text s "Inconsolata" (current-font-size)))
(define (iic s) (text s (cons 'italic "Inconsolata") (current-font-size)))

(define (kt s) (text s "Kaushan Script" (current-font-size)))
(define (kit s) (text s (cons 'italic "Kaushan Script") (current-font-size)))
(define-syntax-rule (wkt . forms) (parameterize ([current-main-font "Kaushan Script"]) . forms))

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
(define-runtime-path logo-path "prl-plum-logos.png" )
(define-runtime-path redex-path "../icfp2013/plt-redex.jpeg")
(define-runtime-path regehr-path "regehr.png")
(define-runtime-path swarm-path "swarm.png")
(define-runtime-path get-bonus-path "get-bonus.png")
(define-runtime-path hekate-path "hekate.png")
(define-runtime-path typesafe-path "typesafe-logo.png")
(define-runtime-path akka-path "akka-logo.png")
(define-runtime-path paddle-ball-path "paddle-ball.png")
(define-runtime-path doggy-bag-path "doggy-bag.jpg")
(define-runtime-path tangle-path "tangled-task-ball.jpg")


(define-runtime-path forbidden8-path "forbidden8.png")
(define-runtime-path forbidden16-path "forbidden16.png")

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
#;
         (with-output-to-file png-path #:exists 'replace
           (λ () (write-bytes (convert (pict->bitmap pict) 'png-bytes))))
         pict]
        [else (bitmap png-path)]))

(define aam-pict (delay (pdf-scale-or-bitmap aam-path aam-png 0.7)))
(define phil-pict (delay (pdf-scale-or-bitmap phil-path phil-png 0.7)))
(define delim-pict (delay (pdf-scale-or-bitmap delim-path delim-png 0.7)))
(define delim-pict3 (delay (pdf-scale-or-bitmap delim-path delim-png 3)))

(define tyellow (make-object color% #xFF #xFF #x00 0.3))

;(define (svg->pict path [α 1.0]) (svg-file->pict path α))

(module+ utils
  (provide bracket-left bracket-right fewer-col faster-col mkei rainbow-rect)
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
  (define bracket-right (bracket rt-superimpose))
  (define bracket-left (bracket lt-superimpose)))

(module+ slide-deck

  (provide title what-do-I-do regehr whiskey false-dichotomy
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
         (with-size 60 (bold (colorize @ct{Abstracting Abstract Control} '(15 15 74))))
         (blank 40)
         (with-size 32
           (ht-append gap-size
                      (vc-append
                       (colorize @cbt{J. Ian Johnson} "navy")
                       (with-size 20 (ic "ianj@ccs.neu.edu"))
                       (blank-line)
                       (with-size 28 (vc-append @ct{Northeastern University}
                                                @ct{Boston, MA, USA})))
                      (vc-append
                       @ct{David Van Horn}
                       (with-size 20 (ic "dvanhorn@cs.umd.edu"))
                       (blank-line)
                       (with-size 28
                        (vc-append @ct{University of Maryland}
                                   @ct{College Park, MD, USA}))))))))))

  (define/staged what-do-I-do #:stages [sounds-abstract delim aam static not-really abstract doing especially]
    #:anim-at [sounds-abstract #:steps 10 #:skip-last]
    #:anim-at [delim #:steps 10 #:skip-first #:skip-last] ;; slide from left "abstract control"
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
                            (show (pin-over analysis 80 -25
                                            (show (bitmap forbidden8-path)
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
                           (vc-append
                            (show
                             (shadow-frame
                              (hc-append gap-size @cit{Really} @ct{semantics of abstract execution}))
                             (>= stage abstract))
                            (show
                             (shadow-frame
                              (hc-append gap-size @ct{How do we understand what programs are} @cit{doing?}))
                             (>= stage doing))
                            (show
                             (shadow-frame (hc-append gap-size @ct{Especially with} @cit{continuations}))
                             (>= stage especially))))]))

  ;; Point at parts of John Regehr's tweet
  (define/staged regehr #:stages [base any-lang some-lang some-parts]
    #:title (wkt @titlet{Is there hope?})
    (define bash (blank 0))
    (define start (blank 0))
    (define any
      (if (< stage some-parts)
          (show
           (shadow-frame
            (hc-append gap-size
                       (cc-superimpose
                        (show @cit{any} (= stage any-lang))
                        (show @cit{some} (= stage some-lang)))
                       (hc-append 0
                                  @ct{dynamic language}
                                  (show @ct{s} (= stage some-lang)))))
           (> stage base))
          (shadow-frame
           (hc-append gap-size
                      @cit{some features} @ct{of}
                      @cit{some}
                      @ct{dynamic languages}))))
    (define α 1.6)
    (define p (scale (bitmap regehr-path) α))
    (define pinned
      (pin-over
       (pin-over
        (pin-over
         p
         (* α 248) (* α 203)
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

                  @ct{Arrays}
                  (show @ct{λ} (>= stage 80s))
                  (show @ct{Polymorphism} (>= stage 90s))
                  (show @ic{eval} (>= stage 2000s))
                  (show @ic{eval} (>= stage now))

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
      (vc-append (shadow-frame @ct{We have some technical debt})
                 (show (shadow-frame @ct{λ is still hard}) (>= stage λhard)))
      (>= stage technical-debt))))

  (define/staged false-dichotomy #:stages [mk-spectrum point-to-static point-to-dynamic
                                                       example0 example1 example2 example3 example4
                                                       spin know-more
                                                       plug-phil symbolic
                                                       its-all-symbolic which-symbols]
    #:title (wkt (with-size 60 @t{False dichotomy}))
    #:anim-at [mk-spectrum #:steps 10]
    #:anim-at [spin #:steps 20 #:skip-first]
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
    (define static-placement (- (+ sw lr-pad)))
    (define (spec n*)
      (define n (fast-end n*))
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
           (define less-runtime (with-size 40 @ic{Know less about context}))
           (define more-runtime (with-size 40 @ic{Know more about context}))
           (define-values (sx sy) (cb-find base (list static)))
           (define-values (dx dy) (cb-find base (list dynamic)))
           (define left-startθ 0.1)
           (define left-endθ (- pi 0.1))
           (define left-startp (blank 0))
           (define left-endp (blank 0))
           (define right-startθ (+ pi 0.1))
           (define right-endθ -0.1)
           (define right-startp (blank 0))
           (define right-endp (blank 0))
           (define radius (/ (+ (pict-width base) (/ (+ lr-pad static-placement sw dw) 2)) 2))
           (define (pinθ θ base p)
             (pin-over base
                       (+ (* radius (cos θ)) (/ (pict-width base) 2))
                       (+ (* radius (sin θ)) (/ (pict-height base) 2)) p))
           (define less-text
             (pin-over-hcenter base (+ 30 (/ (pict-width less-runtime) 2)) 175 less-runtime))
           (define more-text
             (pin-over-hcenter less-text (+ 30 (/ (pict-width more-runtime) 2)) -150 more-runtime))
           (define (mk-arrows lr?)
             (define pinned
               (pinθ right-endθ
                     (pinθ right-startθ
                           (pinθ left-endθ (pinθ left-startθ (blank 0) left-startp) left-endp)
                           right-startp)
                     right-endp))
             (define left-arrow
               (pin-arrow-line 15
                               pinned
                               left-startp cb-find left-endp cb-find
                               #:start-angle (- (/ pi 2))
                               #:start-pull 0.5
                               #:end-angle (/ pi 2)
                               #:end-pull 0.5
                               #:line-width 4))
             (if lr?
                 (pin-arrow-line 15
                                 left-arrow
                                 right-startp ct-find right-endp ct-find
                                 #:start-angle (/ pi 2)
                                 #:start-pull 0.5
                                 #:end-angle (- (/ pi 2))
                                 #:end-pull 0.5
                                 #:line-width 4)
                 left-arrow))
           (define (mk-arrows-overlay lr?)
             (cc-superimpose (if lr? more-text less-text) (mk-arrows lr?)))

           (define (mk-phil)
             (define phil (shadow-frame (force phil-pict)))
             (pin-over (mk-arrows-overlay #t) 0 (- (/ (pict-height phil) 2)) phil))
           (define the-more-you-know
             (vc-append 0 (shadow-frame @ct{The more you know about unknowns})
                        (shadow-frame @ct{The more divergence you catch})
                        (show (shadow-frame @ct{The more you verify})
                              (= stage plug-phil))))
           (cond
            [(= stage point-to-static) (mk-arrows-overlay #f)]
            [(= stage point-to-dynamic) (mk-arrows-overlay #t)]
            [(<= example0 stage example4)
             (define Z @it{ℤ})
             (define factZ (hc-append @ic{fact } Z))
             (define factZ2 (hc-append @ic{fact } Z))
             (define ex-pict
               (shadow-frame
                (vl-append
                 @ic{fact 0 = 1}
                 @ic{fact n = n * fact (n - 1)}
                 (blank 20)
                 (show (hc-append (ghost @it{↦ }) factZ) (>= stage example1))
                 (show (hc-append @it{↦ } @ic{1}) (>= stage example2))
                 (show (hc-append @it{↦ } Z (ic " * fact (") Z (ic " - 1)")) (>= stage example2))
                 (show (hc-append @it{= } Z @ic{ * } factZ2) (>= stage example3)))))
             (define ex-pict-arrow
               (pin-arrow-line 15 ex-pict factZ2 cb-find factZ rc-find
                               #:start-angle (/ pi -16)
                               #:start-pull 2.2
                               #:end-angle (* pi 9/8)
                               #:end-pull 2.2
                               #:color "firebrick"
                               #:line-width 4))
             (cc-superimpose (mk-arrows-overlay #t)
                             (if (= stage example4)
                                 ex-pict-arrow
                                 ex-pict))]
            [(= stage spin) (λ (n) (cc-superimpose more-text (rotate (mk-arrows #t) (lerp 0 (- (* 2 pi)) n))))]
            [(= stage know-more) (cc-superimpose
                                  (cc-superimpose more-text (mk-arrows #t))
                                  the-more-you-know)]
            [(= stage plug-phil) (cc-superimpose (mk-phil) the-more-you-know)]
            [else
             (cc-superimpose
              (mk-phil)
              (show (shadow-frame @ct{“Symbolic execution”})
                    (>= stage symbolic))
              (show (rotate (shadow-frame (hc-append gap-size @cit{All}
                                                     @ct{computation is}
                                                     @ct{symbolic}))
                            (/ pi 6))
                    (>= stage its-all-symbolic))
              (show (rotate (shadow-frame
                             (hc-append gap-size @cit{Which} @ct{symbols?}))
                            (- (/ pi 6)))
                    (>= stage which-symbols)))])]))

  (define shift-rule (with-size 40 @t{E[F[(shift k e)]] ↦ E[e{k := (λ (x) F[x])}]}))

  (define/staged what-good-symbols #:stages [continuations they-are-functions functions?
                                                           symbols not-symbols good-abstraction]
    (with-size 40
      (vl-append gap-size
                 (vl-append 20
                            (hc-append gap-size
                                       @kt{What are good symbols for}
                                       (colorize @kt{continuations?} "steelblue"))
                            (show (hc-append (blank 100)
                                             shift-rule)
                                  (>= stage they-are-functions)))
                 (show (hc-append gap-size @kt{What are good symbols for}
                                  (hc-append 2 (colorize @kt{λ} "orange red") @kt{s?}))
                       (>= stage functions?))
                 (pict-if #:combine cc-superimpose (<= symbols stage not-symbols)
                          (cc-superimpose
                           (blank SCREEN-WIDTH 50)
                           (show (hc-append gap-size
                                            (colorize @ct{'κ} "steelblue")
                                            (colorize @ct{'λ} "orange red"))
                                 (>= stage symbols))
                           (show (bitmap forbidden8-path) (>= stage not-symbols)))
                          (show (ht-append gap-size
                                           @cbt{Claim:}
                                           (vl-append
                                            (hc-append gap-size @ct{A} @cit{good} @ct{abstract scheme})
                                            (hc-append gap-size @ct{spans from} (colorize @ct{static} "blue")
                                                       @ct{to}
                                                       (colorize @ct{dynamic} "red"))))
                                (>= stage good-abstraction))))))

  (define letsλ (wkt @titlet{Let's talk about λ}))

  ;; Abstract machines are a good target because they're runnable, close to how we might write VMs,
  ;; and have easily abstractable components.
  (define/staged abstract-machines #:stages [[redex #:title (ghost letsλ)]
                                             [cesk #:title (ghost letsλ)]
                                             code environment store kont]
    #:title letsλ
    (define codep @it{code})
    (define envp @it{environment})
    (define storep @it{store})
    (define kontp @it{kontinuation})
    (define commasep @it{, })
    (define tuple (hc-append (it "〈") codep commasep envp commasep storep commasep kontp (it "〉")))
    (define code-elab (shadow-frame
                       (vl-append
                        @it{e ::= x}
                        @it{∣ (e e)} @it{∣ (λ x e)})))
    (define env-elab (shadow-frame (vl-append @it{ρ ::= [x ↦ a ...]}
                                              @it{a ∈ Addr})))
    (define store-elab (shadow-frame (vl-append @it{σ ::= [a ↦ v ...]}
                                                @it{v ::= 〈(λ x e), ρ〉})))
    (define kont-elab (shadow-frame (vl-append @it{κ ::= halt ∣ (push φ κ)}
                                               @it{φ ::= (apL e ρ)}
                                               @it{∣ (apR v)})))
    (define ceskp
      (shadow-frame
       (with-size 32 (hc-append gap-size @ct{CESK} tuple))))
    (define-values (cx cy) (cb-find ceskp (list codep)))
    (define-values (ex ey) (cb-find ceskp (list envp)))
    (define-values (sx sy) (cb-find ceskp (list storep)))
    (define-values (kx ky) (cb-find ceskp (list kontp)))
    (cc-superimpose
     (vc-append (with-size 60 @kt{Abstract machines})
                (scale (bitmap redex-path) 0.5))
     (show (pin-over-hcenter
            (pin-over-hcenter
             (pin-over-hcenter
              (pin-over-hcenter
               ceskp
               cx cy #:y-translate 50 (show code-elab (>= stage code)))
              ex ey #:y-translate 200 (show env-elab (>= stage environment)))
             sx sy #:y-translate 50 (show store-elab (>= stage store)))
            kx ky #:y-translate 175 (show kont-elab (>= stage kont)))
           (>= stage cesk))))

  (define/staged pushdown-diagram #:stages [stepping]
    #:title letsλ
    #:anim-at [stepping #:steps 10 #:delay 0.6]
    (define num-actions 10)
    (define κ (with-size 60 @it{κ}))
    (define φ (with-size 60 @it{φ}))
    (define φ₀₁ (hc-append φ (with-size 30 @it{01})))

    (define lang (it "〈"))
    (define rang (it "〉"))
    (define block (rectangle (+ 5 (max (pict-width κ) (pict-width φ₀₁)))
                             (+ 5 (max (pict-width κ) (pict-height φ₀₁)))))
    (define kont (cc-superimpose block κ))
    (define-values (actions max-κ)
      (values #((+ 0) (+ 1) (+ 2) (- 2) (- 1) (+ 3) (- 3) (+ 4) (+ 5) (- 5))
              3)
      #;
      (let mk ([actions-rev '()] [made 0] [i 0] [κ '()] [κ-size 0] [max-size 0])
        (if (= made num-actions)
            (values (apply vector (reverse actions-rev)) max-size)
            (let try ()
              (match (random 2)
                ;; push
                [0 (mk (cons `(+ ,i) actions-rev)
                       (add1 made)
                       (add1 i)
                       (cons i κ)
                       (add1 κ-size)
                       (max (add1 κ-size) max-size))]
                ;; pop
                [1 (match κ
                     [(cons φ κ*) (mk (cons `(- ,φ) actions-rev)
                                      (add1 made)
                                      i
                                      κ*
                                      (sub1 κ-size)
                                      max-size)]
                     ['() (try)])]
                ;; ε
                                        ;[2 (mk (cons 'ε actions-rev) (add1 made) i κ κ-size max-size)]
                )))))
    ;; If a good set of actions, we should determinize for the actual presentation.
    (displayln actions)
    (define (apply-action κ a)
      (match a
        ['ε κ]
        [`(+ ,i) (cons i κ)]
        [`(- ,i) (match κ [(cons (== i) κ*) κ*] [_ (error 'apply-action "Bad action ~a" a)])]))

    (define (apply-actions i)
      (for/fold ([κ '()]) ([a (in-vector actions 0 i)])
        (apply-action κ a)))

    (define (render-frame i)
      (cc-superimpose block (hc-append φ (with-size 30 (it (number->string i))))))
    (define (render-kont κ)
      (match κ
        [(cons i κ*) (vc-append (render-frame i) (render-kont κ*))]
        ['() kont]))

    (define (CES i) (hb-append @it{CES} (with-size 30 (lb-superimpose
                                                       (ghost (it "22"))
                                                       (it (number->string i))))))
    (define (step i)
      (define konti (apply-actions i))
      (define konti* (apply-action konti (vector-ref actions i)))
      (define ς* (with-size 60
                   (ht-append (hc-append (it " ↦ ") lang (CES (add1 i)) rang)
                              (render-kont konti*))))
      (panorama
       (lt-superimpose
        (ghost (apply vc-append (make-list (add1 max-κ) block)))
        (with-size 60 (ht-append
                       (hc-append lang (CES i) rang)
                       (render-kont konti)
                       (if (> i 0)
                           ς*
                           (ghost ς*)))))))
    (λ (n) (step (min (sub1 num-actions) (mkei (* n num-actions))))))

  (define/staged pushdown-example #:stages [transducer
                                            list
                                            list->list
                                            view-filter
                                            reduce show-backlink control-flow]
    #:title (wkt @titlet{Clojure Transducers})
    ;; can't break into multiple lines since code uses newline info
    (define base
      (with-size 30
       (code (#,(tag-pict (code sequence) 'seq) #,(tag-pict (code (comp #,(tag-pict (code (filter even?)) 'filter) #,(tag-pict (code (map inc)) 'map))) 'trans) #,(tag-pict (code (range 10)) 'rng)))))
    (define (hilight b tag)
      (define path (find-tag b tag))
      (define p (first path))
      (define-values (x y) (cc-find b path))
      (pin-under-center b x y
                        (colorize
                         (filled-flash (* 3/2 (pict-width p))
                                       (* 3/2 (pict-height p))
                                       10 0.25 0)
                         "yellow")))
    (define (hilight-and-point base tag txt x-ofs y-ofs [below? #t] #:find [finder cb-find])
      (define h (hilight base tag))
      (define path (find-tag h tag))
      (define-values (x y) (finder h path))
      (define pinned (pin-over h (+ x x-ofs) (+ y y-ofs) txt))
      (pin-arrow-line 15 pinned
                      txt (if below?
                              (λ (p pt)
                                 (define-values (x y) (ct-find p pt))
                                 (values x (+ y 15)))
                              (λ (p pt)
                                 (define-values (x y) (cb-find p pt))
                                 (values x (- y 60))))
                      path (if below? cb-find ct-find)))
    (define lst-txt (shadow-frame (hc-append gap-size @ic{List} @ct{collection})))
    (define lst-obj (hilight-and-point base 'rng lst-txt -170 20))
    (define reduce-update-txt
      (shadow-frame @ct{Functionally update object protocol}))
    (define point (hilight-and-point lst-obj 'trans reduce-update-txt -80 -220 #f))
    (define coll-pict
      (with-size 20
        (code (reify clojure.core.protocols/CollReduce
                (coll-reduce [_ f1 init]
                             (#,(tag-pict (code clojure.core.protocols/coll-reduce) 'coll-reduce)
                              coll
                              (fn [ret v] (if (even? v) (f1 ret v) ret))
                              init))))))
    (define filter-txt
      (shadow-frame (if (>= stage show-backlink)
                        (hilight coll-pict 'coll-reduce)
                        coll-pict)))
    (define filter-point
      (hilight-and-point point 'filter filter-txt -230 120))
    (define reduce-pict
      (hilight-and-point filter-point
                         'seq (shadow-frame @ct{Invoke reducer to build new list})
                         -30 -350 #f))
    (define red-arrow
      (pin-arrow-line 15
                      reduce-pict
                      (find-tag reduce-pict 'coll-reduce) ct-find
                      (find-tag reduce-pict 'map) cb-find
                      #:line-width 4
                      #:color "red"))
    (cond [(= stage transducer) base]
          [(= stage list) lst-obj]
          [(= stage list->list) point]
          [(= stage view-filter) filter-point]
          [(= stage reduce) reduce-pict]
          [(= stage show-backlink) red-arrow]
          [(= stage control-flow) (cc-superimpose red-arrow (scale (bitmap tangle-path) 0.5))]))

  (define/staged pushdown #:stages [λ fun-app decomposed fn-red
                                      pop have-value fn-on-stack app-red
                                      irrelevant irrelevant2 irrelevant3
                                      relevant relevant2
                                      fade-irrelevant call-it-bullet chuck on-return
                                      finitize-addr boom]
    #:title letsλ
    #:anim-at [fade-irrelevant #:steps 10 #:skip-first]
    (define cκ (colorize-if (>= stage irrelevant) @t{κ} "steelblue"))
    (define cκ2 (pict-if (<= stage fade-irrelevant) cκ @t{•}))
    (define cE (colorize-if (>= stage irrelevant) @it{E} "steelblue"))
    (define future (colorize @t{e'} "orange red"))
    (define contractum (colorize @it{e{x := v}} "green"))
    (define bl (blank 0))
    (define (fade-out n p)
      (fade-pict n p (ghost p)))
    (define (screen n)
      (with-size 32
        (vl-append
         (fade-out n (hc-append
                      (t "〈")
                      (colorize-if (= stage fun-app) @t{(f e)} "orange red")
                      (t " ρ σ ")
                      cκ
                      (t "〉 ↦ 〈")
                      (colorize-if (= stage decomposed) @t{f} "green")
                      (t " ρ σ (push ")
                      (colorize-if (= stage decomposed) @t{(apL e ρ) } "green")
                      cκ
                      (t ")〉")))
         (fade-out n (show (hc-append (blank 100) @it{E ::= (E e) ∣ (v E) ∣ []}) (>= stage fn-red)))
         (show (hc-append
                (t "〈")
                (colorize-if (= stage have-value) @t{v} "orange red")
                (t " σ (push ")
                (colorize-if (= stage fn-on-stack) @t{(apR (λ x e) ρ') } "orange red")
                cκ
                (t ")〉 ↦ 〈")
                (colorize-if (= stage app-red)
                             @t{e ρ'[x ↦ a] σ[a ↦ v] }
                             "green")
                cκ2 (t "〉"))
               (>= stage pop))
         (pict-if (< stage chuck)
                  (fade-out n (show (hc-append (blank 100)
                                               cE @it{[((λ x e) v)] ↦ } cE
                                               (it "[")
                                               contractum
                                               (it "]")) (>= stage app-red)))
                  (hc-append gap-size @ct{Chuck} cκ @ct{in a table: }
                             (hc-append @t{Ξ' := Ξ} (t "[")
                                        cκ2 @t{ ↦ Ξ(•) ∪ } (t "{") cκ (t "}") (t "]"))))
         (pict-if (< stage on-return)
                  (fade-out n (show (hc-append gap-size
                                               @ct{The contexts}
                                               cκ
                                               @ct{and}
                                               cE
                                               @ct{are irrelevant}) (>= stage irrelevant)))
                  (hc-append gap-size @t{〈v σ •〉 ↦ } (hc-append (t "〈v σ ") cκ (t "〉"))
                             @ct{where} cκ @ct{∈} @t{Ξ(•)}))
         (fade-out n (show (hc-append gap-size @ct{If } (hc-append contractum @t{ ↦* } future)
                                      (show
                                       (hc-append gap-size @ct{then}
                                                  (hc-append cE (it "[") contractum (it "] ↦* ") cE
                                                             (it "[") future (it "]")))
                                       (>= stage irrelevant3)))
                           (>= stage irrelevant2)))
         (fade-out n (show (hc-append (colorize @t{e} "green") @ct{, } (colorize @t{x} "green") @ct{, and } (colorize @t{v} "green") @ct{ are relevant})
                           (>= stage relevant)))
         (show (hc-append gap-size @ct{So} @t{〈e, x, v, ρ', σ〉} @ct{are relevant}
                          (show (hc-append (ct "(") @ct{call it } cκ2 (ct ")"))
                                (>= stage call-it-bullet)))
               (>= stage relevant2)))))
    (cond
     [(< stage fade-irrelevant) (screen 0)]
     [(= stage fade-irrelevant) screen]
     [(< fade-irrelevant stage finitize-addr) (screen 1)]
     [(>= stage finitize-addr)
      (rt-superimpose
       (cc-superimpose
        (screen 1)
        (vc-append 30
                   (shadow-frame (hc-append @ct{Finitize } @it{Addr}))
                   (show (shadow-frame
                          @ct{Boom, pushdown analysis (the state space forms a DAG)})
                         (>= stage boom))))
       (show @cit{[Vardoulakis & Shivers 2011]} (= stage boom)))]))

  (define/staged intro-continuations #:stages [what-if good-friend]
    (vc-append gap-size
               (with-size 60 (hc-append gap-size
                                        @kt{What if “the stack” isn't a}
                                        @kit{stack}
                                        @kt{?}))
               (show shift-rule (>= stage good-friend))))

  (define/staged why-continuations #:stages [just-toy not-just-toy good-for used-by]
    #:title (wkt @titlet{Who cares about continuations?})
    #:anim-at [good-for #:steps 20 #:skip-first]
    (define-values (i0 i1 i2 i3 i4 i5)
      (with-size 40
        (values @ct{RESTful web applications} ;; PLT webserver
                @ct{Event-driven programming} ;; Hekate
                @ct{Cloud computing}          ;; Swarm
                @ct{Actors} ;; Typesafe's React platform, Akka's futures
                @ct{Operating systems} ;; DrRacket
                @ct{(Game engines?)}))) ;; get-bonus!
    (define is (vector i0 i1 i2 i3 i4 i5))
    (define off-screen (blank 0))
    (define placement*
      (parameterize ([current-gap-size (* 2 gap-size)])
        (apply vl-append (for/list ([i (in-vector is)]) (ghost i)))))
    (define-values (x y) (lt-find placement* i0))
    (define placement (pin-over placement* x SCREEN-HEIGHT off-screen))
    (define slider (slide-and-compose placement is off-screen))
    (cond
     [(<= stage not-just-toy)
      (cc-superimpose (bitmap paddle-ball-path)
                      (show (bitmap forbidden16-path)
                            (= stage not-just-toy)))]
     [(= stage good-for)
      slider]
     [(= stage used-by)
      (pin-over
       (pin-over
        (pin-over
         (pin-over
          (pin-over
           (pin-over (slider 1) -220 -220 (bitmap (plt-logo)))
           100 -20 (scale (bitmap swarm-path) 0.5))
          -220 250 (scale (bitmap get-bonus-path) .6))
         -50 400 (bitmap hekate-path))
        300 300 (scale (bitmap typesafe-path) .3))
       300 -200 (scale (bitmap akka-path) .1))]))

  (define/staged continuation-example #:stages [produce consume sum shifted circular]
    #:anim-at [shifted #:steps 10]
    (define each
      (vl-append
       (code (define (enumerate t)
               (reset (let L ([t t])
                        (if (leaf? t)
                            (shift r (cons t (delay (r '()))))
                            (for-each L t))))))
       (show
        (code (define (fold f init l)
                (match l
                  [(cons (leaf x) prm)
                   (f x (fold f init (force prm)))]
                  [_ init])))
        (>= stage consume))
       (show
        (code (fold + 0
                    (enumerate (list (list (leaf 1))
                                     (leaf 2)
                                     (list (leaf 3)
                                           (list (leaf 4)))))))
        (>= stage sum))))
    (define bad @ct{σ[ra ↦ (comp •)]})
    (define σ0 (ct "σ"))
    (define σ1 (ct "σ'"))
    (define bad-unf (hc-append σ0 (ct "[ra ↦ ( comp 〈e, ρ, ") σ1 (ct "〉)]")))
    (cond
     [(= stage shifted)
      (λ (n) (cc-superimpose each (shadow-frame (fade-pict n bad bad-unf))))]
     [(= stage circular)
      (define arrowed
        (pin-arrow-line 15
                        (pin-arrow-line 15
                                        (shadow-frame bad-unf)
                                        σ0 cb-find σ1 cb-find
                                        #:start-angle (/ pi -2) #:start-pull .6
                                        #:end-angle (/ pi 2) #:end-pull .6
                                        #:line-width 3
                                        #:color "red")
                        σ1 ct-find σ0 ct-find
                        #:start-angle (/ pi 2) #:start-pull .6
                        #:end-angle (/ pi -2) #:end-pull .6
                        #:line-width 3
                        #:color "red"))
      (cc-superimpose each arrowed)]
     [else each]))

  (define/staged how-continuations #:stages [what-do-we-do? cesk->ceskc reset shift]
    (vc-append gap-size
     (hc-append gap-size @t{reset} @ct{introduces a} @cit{meta-stack})
     (show @t{〈e, ρ, σ, κ〉 ⇒ 〈e, ρ, σ, κ, C〉} (>= stage cesk->ceskc))
     (show @t{〈(reset e) ρ σ κ C〉 ↦ 〈e ρ σ halt κ∘C〉} (>= stage reset))
     (show @t{〈(shift k e) ρ σ κ C〉 ↦ 〈e ρ[k ↦ a] σ[a ↦ (comp κ)] halt C〉} (>= stage shift))))

  (define/staged takeaway #:stages [delimit if-capture make-space-dag thank-you]
    #:title (wkt (with-size 60 @t{Takeaway}))
    #:layout 'top
    (pin-over
     (cc-superimpose
      (blank SCREEN-WIDTH
             (- SCREEN-HEIGHT
                (pict-height (wkt (with-size 60 @t{Takeaway})))
                (* 2 (current-gap-size))))
      (with-size 34
        (vc-append (* 2 (current-gap-size))
         (vl-append
          (* 2 (current-gap-size))
          @ct{Delimit computations by relevant state}
          (show @ct{Abstract captured “relevance” with an address}
                (>= stage if-capture))
          (show @ct{Break cycles in state space with addresses}
                (>= stage make-space-dag)))
         (show (with-size 80 @thanks{Thank you}) (>= stage thank-you)))))
     25 -130 (scale (bitmap doggy-bag-path) 0.3)))
  ;; CESK
  ;; K is "stack-like"
  ;; K is not "stack-like," but only sometimes.
  ;; TODO
  ;; We have a recipe for respecting the stack.

  (define (run-talk [sections '(intro features spectrum paper1 paper2 conclusion)]
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
               (run-stages false-dichotomy)
               (run-stages what-good-symbols)
               ;; We have a recipe for handling the stack precisely and abstractly.
               )

      (section paper1
               (run-stages pushdown-example)
               (run-stages abstract-machines)
               (run-stages pushdown-diagram)
               (run-stages pushdown))

      (section paper2
               (run-stages intro-continuations)
               (run-stages why-continuations)
               (run-stages continuation-example)
               (slide
                @ct{Todo: explain what we do}))
      (section conclusion
               (run-stages takeaway)))))

(module+ main
  (require (submod ".." slide-deck))

 (void (run-talk))
 '|That's all, folks!|)

(module+ test
  (require (submod ".." slide-deck))
  (void (run-talk '(paper2))))
