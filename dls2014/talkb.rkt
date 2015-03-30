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


#|
Talk outline:

+ I'm improving the AAM framework's treatment of control structure to better handle constructs like return flow and shift/reset.
+ AAM is this great way of easily (& correctly!) turning your language interpreter into an abstract interpreter (and back again!)
+ You may not think continuations matter, but they do!
</motivation>
- AAM's great, but control structure is ruined.
+ The mantra of AAM is "break recursive structure with heap allocation"
+ Example: Function calls have bad return flow
+ The one big idea in this talk is relevance delimiters.
+ [return to example and illustrate relevance]
</function calls>
+ Does this work for continuations?
+ [little example** showing what shift/reset do]
+ Example with continuations
+ What happens when capture relevance delimiters?
+ We can't make a big split like with Xi, since chi is still relevant!
+ Well, throw chi back in. Cyclic! Crap!
+ Solution: squash chis together and have one parent chi.
+ Example works now
+ What can't we do, and what do we want to do?
  * Can't do modular semantics for shift/reset yet, but we influenced symbolic evaluation work.
  * Heaps can be big and irrelevant
+ Takeaway

|#


(require (except-in unstable/gui/slideshow stage)
         (for-syntax syntax/parse)
;         rsvg
         (only-in 2htdp/planetcute speech-bubble)
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
(define-runtime-path grinder-path "../icfp2013/grinder.png")
(define-runtime-path cousot-path "cousot.png")
(define-runtime-path canyon-path "grand-canyon.jpg")
(define-runtime-path wadler-path "wadler.jpg")
(define-runtime-path black-hole-path "supermassive-black-hole.jpg")

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

  (provide title what-do-I-do why-not-aam fix-aam fix-zoom
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
       5 54 (ghost cfa2p)))
    (define mk-others (slide-and-compose placement is offscreen))
    (cond
     [(= stage sounds-abstract) (compose mk-title fast-start)]
     [(= stage aam) (compose mk-aam fast-start)]
     [(= stage delim) (compose mk-delim fast-start)]
     [(= stage other-papers) (compose mk-others fast-start)]))

  (define/staged why-aam
    #:stages [interpreter why-abs-int examples unit-interpreter could-be-concrete you-need-me]
    #:title (wkt @titlet{Abstracting Abstract Machines})
    (define intp @ct{Interpreter})
    (define absp (frame (inset @ct{Abstract interpreter} 20)
                        #:color (if (>= stage unit-interpreter)
                                    "black"
                                    "white")))
    (define alloc @ct{Allocator})
    (define grind (bitmap grinder-path))
    (define in-point (blank 0))
    (define out-point (blank 0))
    ;; grinder in and out points
    (define-values (in-x in-y) (values 170 7))
    (define-values (out-x out-y) (values 367 133))
    (define pinned (pin-over
                    (pin-over grind out-x out-y out-point)
                    in-x in-y in-point))
    (define in-out
      (pin-over-hcenter
       (pin-over-vcenter
        (pin-over-hcenter pinned in-x -100 intp)
        (+ 50 out-x) out-y absp)
       (+ 50 out-x (/ (pict-width absp) 2))
       (- out-y 150)
       (show alloc (>= stage unit-interpreter))))
    (define arrows
      (panorama
       (pin-arrow-line 15
                       (pin-arrow-line 15
                                       in-out intp cb-find in-point ct-find #:line-width 3)
                       out-point rc-find absp lc-find #:line-width 3)))
    (define arrows2
      (pin-arrow-line 15
        arrows
         alloc cb-find absp ct-find #:line-width 3))
    (define α 0.5)
    (define cousot (frame (scale (bitmap cousot-path) α)))
    (define words @ct{Everything is an abstract interpretation!})
    (define mouth-x (* α 397))
    (define mouth-y (* α 245))
    (define saying
      (panorama (pin-balloon (wrap-balloon words 'sw -40 50) cousot mouth-x mouth-y)))
    (define (pin-cousot base)
      (pin-over base -120 365
                (pin-over saying
                          400 65
                          (show
                           (vl-append 3
                                      @ct{Flow analysis}
                                      @ct{Symbolic evaluator}
                                      @ct{Termination/productivity analysis}
                                      @ct{White-box fuzzer})
                           (= stage examples)))))
    (cond [(= stage interpreter) arrows]
          [(<= why-abs-int stage examples) (pin-cousot arrows)]
          [(= stage unit-interpreter) arrows2]
          [(>= stage could-be-concrete)
           (cc-superimpose
            (pin-arrow-line 15 arrows2 absp rc-find intp rc-find
                            #:start-angle 0
                            #:end-angle pi
                            #:start-pull 0.5
                            #:end-pull 0.75
                            #:line-width 3)
            (show (shadow-frame
                   (hbl-append (colorize @ct{Con:} "firebrick")
                               @ct{ loses important control structure}))
                  (= stage you-need-me)))]))

  (define (what-is-aam)
    (define α 0.8)
    (pin-over (blank SCREEN-WIDTH SCREEN-HEIGHT)
              -20 -20
              (cc-superimpose (filled-rectangle SCREEN-WIDTH SCREEN-HEIGHT)
                                 (pin-over-center (scale (bitmap canyon-path) α) (* α 707) (* α 295)
                                                  (scale (bitmap grinder-path) 0.1)))))

  (define heap-slogan (with-size 60 @kt{Heap-allocate recursion}))
  (define/staged what-is-aam2 #:stages [what-does-it-do abs-step
                                        [slogan #:title heap-slogan]
                                        [slogan-graph0 #:title heap-slogan]
                                        [slogan-graph1 #:title heap-slogan]
                                        [cont-zoom #:title heap-slogan]
                                        [old-cont #:title heap-slogan]
                                        [new-cont #:title heap-slogan]
                                        [old-heap #:title heap-slogan]
                                        [new-heap #:title heap-slogan]]
    #:title (ghost heap-slogan)
    (define (hat p)
      (ct-superimpose p (ct "^")))
    (define small-grind (scale (bitmap grinder-path) 0.15))
    (define step-progress
      (panorama
       (with-size 60
         (hc-append @ct{s ↦ s'}
                    (show (hc-append small-grind
                                     (hat @ct{s}) @ct{ }
                                     (hat @ct{↦}) @ct{ }
                                     (hat @ct{s}) @ct{'})
                          (>= stage abs-step))))))
    (define node0 (filled-ellipse 40 40))
    (define node1 (filled-ellipse 40 40))
    (define node2 (colorize (filled-ellipse 40 40) (if (= stage slogan-graph1) "red" "black")))
    (define-values (node2-x node2-y) (values 240 210))
    (define (graph-nodes tag0 tag2)
      (pin-over-center
       (pin-over-center
        (pin-over-center (blank 255 300) 125 60 (tag-pict node0 tag0))
        30 210 node1)
       node2-x node2-y (tag-pict node2 tag2)))
    (define (graph-base-arrows tag0 tag2)
      (pin-arrow-line 15
                      (pin-arrow-line 15 (graph-nodes tag0 tag2) node0 cb-find node1 ct-find
                                      #:line-width 3)
                      node0 cb-find node2 ct-find #:line-width 3))
    (define back-arrow
      (pin-arrow-line 15
                      (graph-base-arrows 'node0 'node2) node2 rc-find node0 rc-find
                      #:start-angle (* pi 1/6)
                      #:start-pull 0.2
                      #:end-angle pi
                      #:end-pull 0.2
                      #:line-width 3
                      #:color (if (= stage slogan-graph1) "red" "black")
                      #:style (if (= stage slogan-graph1) 'dot-dash 'solid)))
    (define addr-text (colorize @ct{Addr} "red"))
    (define addr-arrow
      (pin-arrow-line 15
                      (pin-over-vcenter back-arrow
                                        (+ 60 node2-x) node2-y
                                        (tag-pict addr-text 'naddr))
                      node2 rc-find addr-text lc-find
                      #:color "red"
                      #:style 'dot
                      #:line-width 3
                      #:hide-arrowhead? #t))
    (define (with-heap base show?)
      (define scaled (scale (graph-base-arrows 'dummy 'ignore) 0.2))
      (define h
        (pin-over base 210 30
                  (show
                   (hc-append (ct "heap[") (tag-pict addr-text 'haddr)
                              (ct " ↦ {")
                              scaled (ct "}]"))
                   show?)))
      (if show?
          (pin-arrow-line
           15
           (pin-arrow-line 15 h (find-tag h 'node2) rc-find (find-tag h 'haddr) cb-find
                           #:color "red" #:line-width 3)
           scaled ct-find (find-tag h 'node0) ct-find
           #:line-width 3 #:color "red"
           #:start-angle (* 2/3 pi)
           #:start-pull 0.1
           #:end-angle (* 5/4 pi)
           #:end-pull 0.1)
          h))
    (with-size 60
      (cc-superimpose
       (vc-append gap-size
                  (pict-if #:combine rb-superimpose (< stage slogan)
                           step-progress
                           (pin-balloon (wrap-balloon (with-size 30 @ct{〈code, heap, cont〉}) 'se 5 20)
                                        step-progress 0 45))
                  (show @ct{cont : List[Activation-Frame]} (>= stage cont-zoom))
                  (show (hc-append @ct{cons :}
                                   (hc-append @ct{X -> }
                                              (pict-if (= stage old-cont)
                                                       @ct{List[X]}
                                                       (colorize @ct{Addr} "firebrick"))
                                              @ct{ -> List[X]}))
                        (>= stage old-cont))
                  (show (hc-append @ct{heap : }
                                   (hc-append (ct "Map[Addr, ")
                                              (pict-if (= stage old-heap)
                                                       @ct{Value}
                                                       (colorize @ct{Set[Value]} "firebrick"))
                                              (ct "]")))
                        (>= stage old-heap))
                  (show (hc-append @ct{h[a ↦ v]}
                                   small-grind
                                   (colorize @ct{h[a ↦ h(a) ∪ {v}]} "firebrick"))
                        (= stage new-heap)))
       (show (shadow-frame (cc-superimpose
                            (blank 600 300)
                            (panorama
                             (with-heap
                              (if (= stage slogan-graph0)
                                  back-arrow
                                  addr-arrow)
                              (= stage slogan-graph1)))))
             (<= slogan-graph0 stage slogan-graph1)))))

  (define call0 "darkgreen")
  (define call1 "cadet blue")
  (define call-ctx0 (colorize (filled-ellipse 20 20) call0))
  (define call-ctx1 (colorize (filled-ellipse 20 20) call1))
  (define (why-not-picts show-delim wrap? use? show-block?)
    (define delim0? (and (number? show-delim) (>= show-delim 0)))
    (define delim1? (and (number? show-delim) (>= show-delim 1)))
    (define req (code (read-request f)))
    (define (req-frame col)
      (thick-rectangle (* 1.1 (pict-width req))
                       (* 1.1 (pict-height req))
                       4
                       #:color col))
    (define codes
      (vl-append gap-size
                 (show
                  (code (λ #,(tag-pict (code (j)) 'entry)
                           (if (good-json? j)
                               (let ([r #,(tag-pict (code (f j)) 'f-call)])
                                 (if (good-html? r)
                                     #,(tag-pict (code r) 'return)
                                     (blame 'f)))
                               (blame 'user))))
                  wrap?)
                 (show
                  (pin-under-tag
                   (pin-under-tag
                    (code (document.write `(p ,#,(tag-pict req 'read0)
                                              ,#,(tag-pict (code (read-request f)) 'read1))))
                    lt-find
                    'read0
                    (λ _ (show (req-frame call0) delim0?)))
                   lt-find 'read1 (λ _ (show (req-frame call1) delim1?)))
                  use?)
                 (show
                  (if (pict? show-block?)
                      show-block?
                      (hc-append (code read-request)
                                 @ct{ blocks until json is read, then calls }
                                 (code f)))
                  (not (not show-block?)))))
    (pin-over-vcenter codes (find-tag codes 'entry) rc-find
                      (hc-append 5
                                 (show call-ctx0
                                       delim0?)
                                 (show call-ctx1
                                       delim1?))))

  (define/staged why-not-aam
    #:stages [say wrap use line0 line1 line2 line3 line4 cycle proper]
    (define cycol (if (= stage cycle) "red" "black"))
    (define cycol1 (cond [(= stage cycle) "red"]
                         [(= stage proper) call0]
                         [else "black"]))
    (define call0-width (if (= stage proper) 5 3))
    (define propcol0 (if (= stage proper) call0 "black"))
    (define propcol1 (cond [(= stage proper) call1]
                           [(= stage cycle) "red"]
                           [else "black"]))
    (define propcol2 (if (= stage proper) call1 "black"))
    (define codes (why-not-picts #f (>= stage wrap) (>= stage use) (>= stage use)))
    (define read0->call
      (pin-arrow-line 15 codes (find-tag codes 'read0) rc-find (find-tag codes 'f-call) ct-find
                      #:start-angle (* 1/6 pi)
                      #:end-angle (* -1/3 pi)
                      #:start-pull 0.4 #:end-pull 0.8
                      #:line-width call0-width #:color propcol0
                      #:style (if (= stage proper) 'long-dash 'solid)))
    (define call->ret
      (pin-arrow-line 15 read0->call
                      (find-tag codes 'f-call) cb-find
                      (find-tag codes 'return) lc-find
                      #:start-angle (* 1/6 pi)
                      #:end-angle 0
                      #:start-pull 0.2 #:end-pull 0.7
                      #:line-width 3 #:color cycol))
    (define ret->read0
      (pin-arrow-line 15 call->ret (find-tag codes 'return) rc-find (find-tag codes 'read0) ct-find
                      #:start-angle 0 #:end-angle (* -1/3 pi) #:end-pull 0.4
                      #:line-width call0-width
                      #:color cycol1 #:style (if (= stage proper) 'long-dash 'solid)))
    (define read1->call
      (pin-arrow-line 15 ret->read0
                      (find-tag codes 'read1) rc-find
                      (find-tag codes 'f-call) ct-find
                      #:start-angle (* 1/6 pi)
                      #:end-angle (* -1/3 pi)
                      #:start-pull 0.4
                      #:end-pull 0.8
                      #:line-width 3 #:color propcol1))
    (define ret->read1
      (pin-arrow-line 15 read1->call (find-tag codes 'return) rc-find (find-tag codes 'read1) ct-find
                      #:start-angle 0 #:end-angle (* -1/3 pi) #:end-pull 0.4
                      #:line-width 3 #:color propcol2))

    (vl-append gap-size
               (hc-append @ct{Say we have some function } (code f : json -> html))
               (show @ct{We wrap it to validate its input and output} (>= stage wrap))
               (cond [(<= stage use) codes]
                     [(= stage line0) read0->call]
                     [(= stage line1) call->ret]
                     [(= stage line2) ret->read0]
                     [(= stage line3) read1->call]
                     [(<= line4 stage proper) ret->read1])))

  (define/staged fix-aam
    #:stages [revisit first-call second-call]
    (define codes (why-not-picts (and (> stage revisit) (sub1 stage)) #t #t #f))
    (define ctx (ic "Ξ = ["))
    (define M (ic "M = ["))
    (define close (ic "]"))
    (define call-map0 (hc-append call-ctx0 (ic " ↦ {") (thick-rectangle 40 25 2 #:color call0) (ic "}")))
    (define call-map1  (hc-append call-ctx1 (ic " ↦ {") (thick-rectangle 40 25 2 #:color call1) (ic "}")))
    (define call-memo0 (hc-append call-ctx0 (ic " ↦ {") (colorize (ic "〈⟦r⟧h₀,h₀〉") call0) (ic "}")))
    (define call-memo1  (hc-append call-ctx1 (ic " ↦ {") (colorize (ic "〈⟦r⟧h₁,h₁〉") call1) (ic "}")))    
    (define pin-ctx
      (pin-over
       (pin-over-vcenter
        (pin-over-vcenter codes (find-tag codes 'read0) rc-find
                          (tag-pict (show call-ctx0 (>= stage first-call)) 'ctx0))
        (find-tag codes 'read1) rc-find (tag-pict (show call-ctx1 (>= stage second-call)) 'ctx1))
       0 400
       (vl-append
        gap-size
        ;; State of Ξ:
        (cond
         [(= stage revisit) (hc-append ctx close)]
         [(= stage first-call) (hc-append ctx call-map0 close)]
         [(= stage second-call) (hc-append ctx call-map0 (ic ", ") call-map1 close)])
        ;; State of M:
        (cond
         [(= stage revisit) (hc-append M close)]
         [(= stage first-call) (hc-append M call-memo0 close)]
         [(= stage second-call) (hc-append M call-memo0 (ic ", ") call-memo1 close)])
        )))
    (define read0->call
      (pin-arrow-line 15 pin-ctx (find-tag pin-ctx 'ctx0) rc-find (find-tag codes 'f-call) ct-find
                      #:start-angle (* 1/6 pi)
                      #:end-angle (* -1/3 pi)
                      #:start-pull 0.4 #:end-pull 0.8
                      #:line-width 6 #:color call0
                      #:style 'long-dash))
    (define call->ret
      (pin-arrow-line 15 read0->call
                      (find-tag codes 'f-call) cb-find
                      (find-tag codes 'return) lc-find
                      #:start-angle (* 1/6 pi)
                      #:end-angle 0
                      #:start-pull 0.4 #:end-pull 0.9
                      #:line-width 3))
    (define ret->read0
      (pin-arrow-line 15 call->ret (find-tag codes 'return) rc-find (find-tag pin-ctx 'read0) ct-find
                      #:start-angle 0 #:end-angle (* -1/3 pi) #:end-pull 0.4
                      #:line-width 6
                      #:style 'long-dash
                      #:color call0))
    (define read1->call
      (pin-arrow-line 15 ret->read0
                      (find-tag pin-ctx 'ctx1) rc-find
                      (find-tag codes 'f-call) ct-find
                      #:start-angle (* 1/6 pi)
                      #:end-angle (* -1/3 pi)
                      #:start-pull 0.4
                      #:end-pull 0.8
                      #:line-width 3 #:color call1))
    (define ret->read1
      (pin-arrow-line 15 read1->call (find-tag codes 'return) rc-find (find-tag codes 'read1) ct-find
                      #:start-angle 0 #:end-angle (* -1/3 pi) #:end-pull 0.4
                      #:line-width 3 #:color call1))

    (cond [(= stage revisit) codes]
          [(= stage first-call) ret->read0]
          [(= stage second-call) ret->read1]))

  (define (cyclic base left right)
    (define lr
      (pin-arrow-line 15
                      base
                      left cb-find right cb-find
                      #:start-angle (/ pi -2) #:start-pull .6
                      #:end-angle (/ pi 2) #:end-pull .6
                      #:line-width 3
                      #:color "red"))
    (pin-arrow-line 15
                    lr
                    right ct-find left ct-find
                    #:start-angle (/ pi 2) #:start-pull .6
                    #:end-angle (/ pi -2) #:end-pull .6
                    #:line-width 3
                    #:color "red"))

  (define/staged fix-zoom #:stages [AAM just-addrs relevance structure circular the-trick]
    #:title (wkt @titlet{What's really going on here?})
    (define h0 (ic "h"))
    (define h1 (ic "h'"))
    (define popup
      (inset (hbl-append h0 (ic "[〈c,") h1 (ic "〉 ↦ {cont}]")) 30))
    (define popup* (if (>= stage circular)
                       (cyclic popup h0 h1)
                       popup))
    (vc-append 50
     (vl-append gap-size
                (hbl-append @ct{AAM told us } @ic{cons : X -> Addr -> List[X]})
                (show (hc-append @ct{Are } call-ctx0 @ct{ just fancy addresses?})
                      (>= stage just-addrs))
                (show
                 (vl-append gap-size
                            (hbl-append @ct{States are } @ic{〈code heap stack〉} @ct{ and the stack is irrelevant})
                            (hc-append call-ctx0 @ct{ is } @ic{〈code heap〉}))
                 (>= stage relevance)))
     (show popup* (>= stage structure))
     (show (hc-append call-ctx0 @ct{ are stored in a stratified heap: Ξ})
           (>= stage the-trick))))

  (define/staged non-blocking-example #:stages [recall show-code unfold problem stratify]
    #:anim-at [unfold #:steps 5]
    (define h0 @ct{h})
    (define h1 @ct{h'})
    (define stored1 (hbl-append (ct "〈c,") h1 (ct "〉")))
    (define (store-pict p)
      (hbl-append h0 (ct "[ka ↦ {(comp ") p (ct ")}]")))
      
    (define (mk n)
      (nbe (>= stage show-code)
           (show (shadow-frame
                  (if (>= stage problem)
                      (cyclic (store-pict stored1) h0 h1)
                      (store-pict (fade-pict n call-ctx0 stored1))))
                 (>= stage unfold))
           (hbl-append (code read-request) @ct{ uses non-blocking I/O})))
    (cond 
     [(< stage unfold) (mk 0)]
     [(= stage unfold) mk]
     [(> stage unfold)
      (cc-superimpose
       (mk 1)
       (show (rotate (shadow-frame @ct{Can we stratify like with Contexts?}) (/ pi 16))
             (= stage stratify)))]))

  (define (nbe show-code? top bot)
    (define req-def
      (code (define #,(tag-pict (code (read-request f)) 'req-head)
              #,(tag-pict (code (shift k (evloop-until-evt
                                          (read-request-evt f)
                                          k)))
                          'body))))
    (cc-superimpose
     (why-not-picts #f #t #t bot)
     (show (vc-append (shadow-frame req-def)
                      top)
           show-code?)))
  
  (define/staged non-blocking-solution #:stages [first run0 run1 different-addressing]
    (define call0-left (arc 20 20 (* 1/2 pi) (* 3/2 pi) #t #:color call0))
    (define call1-left (arc 20 20 (* 1/2 pi) (* 3/2 pi) #t #:color call1))

    (define call0-h (arc 20 20 (* 1/2 pi) 0 #f #:color call0))
    (define call1-h (arc 20 20 (* 1/2 pi) 0 #f #:color call1))
    (define call0-χ (arc 20 20 0 (* 3/2 pi) #f #:color call0))
    (define call1-χ (arc 20 20 0 (* 3/2 pi) #f #:color call1))
    (define ka0 (colorize-if (= stage different-addressing) (ct "ka") call0))
    (define ka1 (colorize-if (= stage different-addressing) (ct "ka") call1))
    (define a0 (colorize-if (= stage different-addressing) (ct "a") call0))
    (define a1 (colorize-if (= stage different-addressing) (ct "a") call1))
    (define base (nbe #t
                      (shadow-frame
                       (vl-append gap-size
                                  (vl-append 
                                   (hc-append (ct "h = [")
                                              (if (>= stage run0)
                                                  (hc-append ka0 (ct " ↦ {(comp 〈") call0-left (ct ",") a0 (ct "〉)")
                                                             (if (= stage run1)
                                                                 (hc-append (ct ", (comp 〈") call1-left (ct ",") a1 (ct "〉)"))
                                                                 (blank 0))
                                                             (ct "}")
                                                             (if (= stage different-addressing)
                                                                 (ct ",")
                                                                 (blank 0)))
                                                  (blank 0))
                                              (if (< stage different-addressing) (ct "]") (blank 0)))
                                   (show (hc-append (ghost (ct "h = ["))
                                                  ka1 (ct " ↦ {(comp  〈") call1-left (ct ",") a1 (ct "〉)}]"))
                                       (= stage different-addressing)))
                                  (hc-append (ct "χ = ")
                                             (if (>= stage run0) (hc-append call0-χ @ct{ ⊔ }) (blank 0))
                                             (if (>= stage run1) (hc-append call1-χ @ct{ ⊔ }) (blank 0))
                                             (cond
                                              [(= stage first) (ct "[]")]
                                              [(< stage different-addressing)
                                               (hc-append
                                                (ct "[a ↦ {")
                                                (if (>= stage run0) call0-h (blank 0))
                                                (if (>= stage run1) (hc-append (ct ", ") call1-h) (blank 0))
                                                (ct "}]"))]
                                              [else (hc-append (ct "[") a0 (ct " ↦ {") call0-h (ct "}] ⊔ [") a1 (ct " ↦ {") call1-h (ct "}]"))]))))
                      (blank 0)))
    (define with-ctxs
      (pin-over-vcenter base (find-tag base 'req-head) rc-find
                        (hc-append 5
                                   (show call-ctx0 (>= stage run0))
                                   (show call-ctx1 (>= stage run1)))))
    with-ctxs)

  (define/staged no-stratification #:stages [say-we-do extra-env is-relevant why-relevant new-relevance what-χ χ-circ cant-do-it]
    #:title (wkt @titlet{Of course not!})
    (define ra (colorize @ic{a} "red"))
    (define χ0 (ic "χ"))
    (define χ1 (ic "χ'"))
    (define circ (hbl-append χ0 (ic "(") ra (ic ")") @ct{ ∋ } (ic "〈h',") χ1 (ic "〉")))
    (cc-superimpose
     (vc-append 120
      (vl-append gap-size
                 (vc-append gap-size
                            (hbl-append (ic "〈(shift k e), heap, ") call-ctx0 (ic "〉") @ct{ produces } (ic "heap(ka)") @ct{ ∋ } (ic "(comp 〈c,") ra (ic "〉)"))
                            (show (hbl-append (ic "χ(") ra (ic ")") @ct{ ∋ } (ic "h'")) (>= stage extra-env)))
                 (show (hbl-append @ct{Well, now } (ic "χ") @ct{ is relevant!}
                                  (show (hbl-append @ct{ Since } (ic "χ") @ct{ closes the heap}) (>= stage why-relevant)))
                       (>= stage is-relevant))
                 (show (hc-append call-ctx0 @ct{ ≡ } @ic{〈c', h', χ'〉}) (>= stage new-relevance)))
      (show (if (>= stage χ-circ) (cyclic circ χ0 χ1) circ) (>= stage what-χ)))
     (show (shadow-frame (hbl-append χ0 @ct{ and } @ic{heap} @ct{ are mutually recursive! Can't stratify!})) (>= stage cant-do-it))))

  (define shift-rule (with-size 40 @t{E[F[(shift k e)]] ↦ E[e{k := (λ (x) F[x])}]}))

  (define/staged intro-continuations #:stages [what-if good-friend]
    (vc-append gap-size
               (with-size 60 (hbl-append gap-size
                                        @kt{What if “the stack” isn't a}
                                        @kit{stack}
                                        @kt{?}))
               (show shift-rule (>= stage good-friend))))

  (define/staged why-continuations #:stages [just-toy not-just-toy good-for used-by
                                                      hard-to-understand]
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
     [(= stage good-for) slider]
     [(>= stage used-by)
      (define users
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
         300 -200 (scale (bitmap akka-path) .1)))
      (define wadler
        (panorama
         (pin-balloon (wrap-balloon @ct{I don't understand!} 'sw -40 50)
                      (frame (bitmap wadler-path))
                       257 ;; wadler mouth x
                       103 ;; wadler mouth y
                       )))
      (cc-superimpose
       users
       (show wadler (= stage hard-to-understand)))]))

  (define/staged toy-continuation #:stages [toy stack stack-fun stack-run
                                            shift-meaning
                                            what-evaluated k-means subst-k beta result]
    #:title shift-rule
    (define shift-stack
      (pin-over (rectangle 30 200)
                0 95 (cc-superimpose (rectangle 30 20) @ic{#})))
    (define brack-k
      (pin-over
       (pin-over shift-stack 35 0 (bracket-right 10 100 5))
       50 30 @ct{now a function}))
    (define run-point (blank 0))
    (define run @ct{run from here})
    (define runy 120)
    (define pointed
      (pin-arrow-line 10
                      (pin-over-vcenter
                       (pin-over brack-k 30 runy run-point)
                       50 runy run)
                      run lc-find run-point rc-find))
    (define cont (colorize-if (= stage subst-k) @ic{(λ (x) (+ 2 x))} "red"))
    (pin-over
     (vl-append gap-size
                (blank 50)
                @ic{(+ 10 (reset (+ 2 (shift k (+ 40 (k (k 3)))))))}
                (show (hc-append 0 @ic{  }
                                 (show (ic "k = (λ (x) ") (>= stage k-means))
                                 (ic "(+ 2 ")
                                   (pict-if #:combine cc-superimpose
                                            (>= stage k-means) @ic{x} @ic{[]})
                                 (ic ")")
                                 (show (ic ")") (>= stage k-means)))
                      (>= stage shift-meaning))
                (pict-cond
                 [(<= what-evaluated stage k-means) @ic{(+ 10 (+ 40 (k (k 3))))}]
                 [(= stage subst-k) (hc-append (ic "(+ 10 (+ 40 (") cont (ic " (") cont (ic " 3)"))]
                 [(>= stage beta) @ic{(+ 10 (+ 40 (+ 2 (+ 2 3))))}])
                (show @ic{57} (= stage result)))
     300 -150 (show (pict-cond [(= stage stack) shift-stack]
                               [(= stage stack-fun) brack-k]
                               [(>= stage stack-run) pointed])
                    (>= stage stack))))

  (define/staged future #:stages [grinder want-scale black-hole relevance possible-solution]
    #:title (wkt @titlet{Where do we stand?})
    (vl-append gap-size
               (hc-append (scale (bitmap grinder-path) 0.4) @ct{ abstract languages and respect control})
               (show @ct{Want shift/reset in modular semantics} (>= stage want-scale))
               (show (hc-append (blank 300 1) (ct "(what if (comp ") call-ctx0 (ct ") is ") (scale (bitmap black-hole-path) 0.5) (ct ")") ) (>= stage black-hole))
               (show
                (hc-append (ct "Not all the heap is relevant ") (show (ct "[Stefan Staiger-Stӧhr diss]") (>= stage possible-solution)))
                (>= stage relevance))))

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
          (show @ct{Squash abstracted relevance objects} (>= stage if-capture))
          (show @ct{Break cycles in state space with addresses}
                (>= stage make-space-dag)))
         (show (with-size 80 @thanks{Thank you}) (>= stage thank-you)))))
     25 -130 (scale (bitmap doggy-bag-path) 0.3)))
  ;; CESK
  ;; K is "stack-like"
  ;; K is not "stack-like," but only sometimes.
  ;; TODO
  ;; We have a recipe for respecting the stack.

  (define (run-talk [sections '(intro motivation how-aam why-not the-one-idea
                                      idea-and-conts non-blocking conclusion)]
                    #:main-font [main-font "LModern"])
    (parameterize ([current-main-font main-font])
      (define-syntax-rule (section name body ...) (when (memv 'name sections) body ...))
      (section intro
               (title)
               (run-stages what-do-I-do))
      (section motivation
               (run-stages why-aam)
               (run-stages why-continuations)
               (slide (with-size 60 @ic{</motivation>})))

      (section how-aam
               (slide (what-is-aam))
               (run-stages what-is-aam2))
      (section why-not
               (run-stages why-not-aam))

      (section the-one-idea
               (slide (with-size 60 (vl-append @kt{Insight:}
                                               @kt{delimit computations &}
                                               @kt{catalog contexts by revelant state})))
               (slide (vr-append 100
                                 (ht-append (with-size 60 @kt{The stack doesn't matter}) @ct{*})
                                 @ct{*yet}))
               (run-stages fix-aam)
               (run-stages fix-zoom))

      (section idea-and-conts
               (run-stages intro-continuations)
               (run-stages toy-continuation))

      (section non-blocking
               (run-stages non-blocking-example)
               (run-stages no-stratification)
               (slide
                (with-size 60 @kt{Squash it})
                'next
                (hbl-append @ct{Instead of } @ic{χ ⊔ [a ↦ 〈h',χ'〉]})
                'next
                (hbl-append @ct{we do } @ic{χ ⊔ χ' ⊔ [a ↦ {h'}]})
                'next
                @ct{⟦〈c',a〉⟧ = {cont ∈ Contexts(〈c',h',χ'〉) : h' ∈ χ(a), χ' ⊑ χ}})

               (run-stages non-blocking-solution))

      (section conclusion
               (run-stages future)
               (run-stages takeaway)))))

(module+ main
  (require (submod ".." slide-deck))

 (void (run-talk))
 '|That's all, folks!|)

(module+ test
  (require (submod ".." slide-deck))
  (void (run-talk '(conclusion))))
