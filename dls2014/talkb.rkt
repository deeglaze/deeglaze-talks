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
(set-page-numbers-visible! #t)
(define use-pdf? #f)
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
(define-runtime-path gc-path "abstract-models-of-memory-management.pdf")
(define-runtime-path cm-path "cm-machine.pdf")
(define-runtime-path cfa2-path "cfa2-1st-class.pdf")

(define-runtime-path aam-png "../icfp2013/vanhorn2010abstract.png")
(define-runtime-path phil-png "soft-contract-verification.png")
(define-runtime-path delim-png "abstracting-control.png")
(define-runtime-path gc-png "abstract-models-of-memory-management.png")
(define-runtime-path cm-png "cm-machine.png")
(define-runtime-path cfa2-png "cfa2-1st-class.png")

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
           (λ () (write-bytes (convert
                               (page->bitmap (pdf-page (open-pdf pdf-path) 0) (* 4 scale-factor))
                               'png-bytes))))
         pict]
        [else (scale (bitmap png-path) 0.25)]))

(define aam-pict (delay (pdf-scale-or-bitmap aam-path aam-png 0.7)))
(define gc-pict (delay (pdf-scale-or-bitmap gc-path gc-png 0.7)))
(define cm-pict (delay (pdf-scale-or-bitmap cm-path cm-png 0.7)))
(define cfa2-pict (delay (pdf-scale-or-bitmap cfa2-path cfa2-png 0.7)))
(define phil-pict (delay (pdf-scale-or-bitmap phil-path phil-png 0.7)))
(define delim-pict (delay (pdf-scale-or-bitmap delim-path delim-png 0.7)))
(define delim-pict3 (delay (pdf-scale-or-bitmap delim-path delim-png 3)))

(define tyellow (make-object color% #xFF #xFF #x00 0.3))

;(define (svg->pict path [α 1.0]) (svg-file->pict path α))

(module+ utils
  (provide bracket-left bracket-right fewer-col faster-col mkei rainbow-rect
           fade-out)
  (define (fade-out n p) (fade-pict n p (ghost p)))
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

  (provide title what-do-I-do
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

  (define/staged what-do-I-do #:stages [sounds-abstract aam delim other-papers]
    #:anim-at [sounds-abstract #:steps 10 #:skip-last]
    #:anim-at [aam #:steps 10 #:skip-first #:skip-last] ;; slide from left "abstracting abstract machines"
    #:anim-at [delim #:steps 10 #:skip-first #:skip-last] ;; slide from right "abstracting control"
    #:anim-at [other-papers #:steps 30]
    (define sounds (with-size 72 @kt{“Sounds abstract”}))
    (define (mk-title n)
      (pin-over-hcenter (blank 0) 0 0 sounds
                        #:y-translate (lerp (- (/ (pict-height sounds) 2))
                                            (- (/ SCREEN-HEIGHT 2))
                                            n)))

    (define delimp (shadow-frame (force delim-pict)))
    (define aamp (shadow-frame (force aam-pict)))
    (define gcp* (force gc-pict))
    (define (white-frame p)
      (define p* (inset p 20))
      (inset
       (inset
        (frame (cc-superimpose
                (colorize (filled-rectangle (pict-width p*) (pict-height p*)) "white")  
                p*)
               #:color "gray")
        4.0 0 4.0 46.0)
       20))
    (define gcp (white-frame (force gc-pict)))
    (define cmp (white-frame (force cm-pict)))
    (define cfa2p (white-frame (force cfa2-pict)))
    (define is (vector gcp cmp cfa2p))
    (define (mk-aam n)
      (pin-over-vcenter
       (mk-title 1)
       0 35 aamp
       #:x-translate (lerp (- (+ (pict-width aamp) (/ SCREEN-WIDTH 2)))
                           aamx
                           n)))
    (define (mk-delim n)
      (pin-over-vcenter
       (mk-aam 1) 0 35 delimp
       #:x-translate (lerp (/ SCREEN-WIDTH 2) 5 n)))
    (define aamx (- (pict-width aamp)))
    #|slide in GC paper, CM paper and CFA2 paper|#
    (define offscreen (blank 0))    
    (define placement
      (pin-over
       (pin-over
        (pin-over
         (pin-over (mk-delim 1) 5 SCREEN-HEIGHT offscreen)
         5 -200 (ghost gcp))
        5 -96 (ghost cmp))
       5 55 (ghost cfa2p)))
    (define mk-others (slide-and-compose placement is offscreen))
    (cond
     [(= stage sounds-abstract) (compose mk-title fast-start)]
     [(= stage aam) (compose mk-aam fast-start)]
     [(= stage delim) (compose mk-delim fast-start)]
     [(= stage other-papers) (compose mk-others fast-start)]))

  (define shift-rule (with-size 40 @t{E[F[(shift k e)]] ↦ E[e{k := (λ (x) F[x])}]}))

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

  (define/staged continuation-example #:stages [produce consume sum shifted circular
                                                kill-example move-frame
                                                where-put intro-χ but-relevant give-up
                                                abstraction-rule use-rule spans]
    #:anim-at [shifted #:steps 10]
    #:anim-at [kill-example #:steps 10 #:skip-first]
    #:anim-at [move-frame #:steps 10]
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
    (define bad-unf (hc-append σ0 (ct "[ra ↦ ( comp 〈e, ρ, ") (if (>= stage move-frame)
                                                                   @ct{a}
                                                                   σ1) (ct "〉)]")))
    (define p (shadow-frame bad-unf))
    (define frame-x (- (/ (pict-width each) 2) (/ (pict-width p) 2)))
    (cond
     [(= stage shifted)
      (λ (n) (cc-superimpose each (shadow-frame (fade-pict n bad bad-unf))))]
     [(= stage circular)
      (define arrowed
        (pin-arrow-line 15
                        (pin-arrow-line 15
                                        p
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
     [(= stage kill-example)
      (λ (n) (cc-superimpose (fade-out n each) p))]
     [(= stage move-frame)
      (λ (n) (pin-over (ghost each) frame-x
                       (lerp (- (/ (pict-height each) 2)
                                (/ (pict-height p) 2)) 0 (fast-start n))
                       p))]
     [(<= where-put stage spans)
      (cc-superimpose
       (pin-over (ghost each) frame-x 0
                 (pin-over-hcenter
                  p 200 175
                  (vl-append gap-size
                             (show @ct{Where do we put a?} (>= stage where-put))
                             (show @ct{χ' := χ[a ↦ χ(a) ∪ {σ'}]} (>= stage intro-χ))
                             (show @ct{But a is reachable from σ, so χ is relevant (• ≡ 〈e, ρ, σ, χ〉)} (>= stage but-relevant))
                             (show @ct{Give up a bit and grow χ monotonically} (>= stage give-up))
                             (show @ct{A(〈e, ρ, σ, χ'〉, χ, a) = (χ ⊔ χ' ⊔ [a ↦ {σ}], 〈e, ρ, a〉)}
                                   (>= stage abstraction-rule))
                             (show @ct{R(〈e, ρ, a〉, χ) = ⋃{Ξ(〈e, ρ, σ, χ'〉) : σ ∈ χ(a), χ' ⊑ χ}}
                                   (>= stage use-rule)))))
       (show (rotate
              (shadow-frame (hc-append gap-size
                                       @ct{Choice of allocator spans from}
                                       (colorize @ct{static} "blue")
                                       @ct{to}
                                       (colorize @ct{dynamic} "red")))
              (/ pi 16)) (= stage spans)))]
     [else each]))


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

  (define (run-talk [sections '(intro sounds why paper2 conclusion)]
                    #:main-font [main-font "LModern"])
    (parameterize ([current-main-font main-font])
      (define-syntax-rule (section name body ...) (when (memv 'name sections) body ...))
      (section intro
               (title))
      (section sounds
               (run-stages what-do-I-do))
      (section why
               (run-stages why-continuations))

      (section paper2
               (run-stages intro-continuations)
               (run-stages continuation-example))
      (section conclusion
               (run-stages takeaway)))))

(module+ main
  (require (submod ".." slide-deck))

 (void (run-talk))
 '|That's all, folks!|)

(module+ test
  (require (submod ".." slide-deck))
  (void (run-talk '(sounds))))
