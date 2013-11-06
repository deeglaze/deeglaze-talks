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

;; XXX: straight from texpict/private/common-unit.rkt
(module+ disgusting-copy-paste
  (provide frame-without-bottom)
  (define default-seg 5)
  (define (quotient* a b)
    (if (integer? a)
        (quotient a b)
        (/ a b)))
  (define (rlist b a) (list a b))
  (define (dash-line width height rotate seg)
    (let ([vpos (quotient* height 2)])
      (make-pict
       `(picture
         ,@(rotate width height)
         ,@(if (>= seg width)
               `((put ,@(rotate 0 vpos) (line ,@(rotate 1 0) ,width)))
               (let* ([remain (+ (- width (floor width))
                                 (remainder (floor width) (* 2 seg)))]
                      [count (inexact->exact (floor (quotient* width (* 2 seg))))]
                      [lremain (quotient* remain 2)]
                      [rremain (- remain lremain)])
                 `((put ,@(rotate 0 vpos) (line ,@(rotate 1 0) ,lremain))
                   ,@(let loop ([count count][pos lremain])
                       (if (zero? count)
                           null
                           (cons `(put ,@(rotate (+ pos seg) vpos) 
                                       (line ,@(rotate 1 0) ,seg))
                                 (loop (sub1 count) (+ pos seg seg)))))
                   (put ,@(rotate (- width rremain) vpos) 
                        (line ,@(rotate 1 0) ,rremain))))))
       (car (rotate width height))
       (cadr (rotate width height))
       (cadr (rotate 0 height)) 0
       null
       #f
       #f)))
  (define dash-hline
    (case-lambda 
      [(width height) (dash-hline width height default-seg)]
      [(width height seg) (dash-line width height list seg)]))

  (define dash-vline
    (case-lambda 
      [(width height) (dash-vline width height default-seg)]
      [(width height seg) (dash-line height width rlist seg)]))
  (define (extend-pict box dx dy dw da dd draw)
    (let ([w (pict-width box)]
          [h (pict-height box)]
          [d (pict-descent box)]
          [a (pict-ascent box)])
      (make-pict (if draw draw (pict-draw box))
                 (+ w dw) (+ h da dd) 
                 (max 0 (+ a da)) (max 0 (+ d dd))
                 (list (make-child box dx dy 1 1 0 0))
                 #f
                 (pict-last box))))
  ;; Aaaand the one new thing
  (define (frame-without-bottom box)
    (define w (pict-width box))
    (define h (pict-height box))
    (define seg (max w h))
    (extend-pict
     box 0 0 0 0 0
     `(picture
       ,w ,h
       (put 0 0 ,(pict-draw box))
       ;;(put 0 0 ,(pict-draw (dash-hline w 0 seg)))
       (put 0 ,h ,(pict-draw (dash-hline w 0 seg)))
       (put 0 0 ,(pict-draw (dash-vline 0 h seg)))
       (put ,w 0 ,(pict-draw (dash-vline 0 h seg)))))))

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
           (only-in "../icfp2013/pict-helpers.rkt" join-one braces nstruct production expr call tuple ntuple)
           (submod ".." disgusting-copy-paste))
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

  (define/staged (useful stage) #:stages [base anim to-name-a-few] #:anim-at anim #:steps 30
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
    (run-stages temporal))

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
    #;
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
    (slide
     (big (t "In the context of"))
     (big (ntuple (expr @tt{e}) @idtt{ρ} @σtt{σ} @tt{χ} @ctt{κ} @Mtt{M} @Ξtt{Ξ}))
     (big (t "Interpret"))
     (tuple (expr @tt{e′}) @idtt{ρ′} (addr @tt{a}))
     (big (t "as"))
     (braces (tuple (expr @tt{e′}) @idtt{ρ′} @σtt{σ′} @tt{χ′}) @tt{ ∈ } (call @tt{dom} @Ξtt{Ξ})
              @tt{ : } @σtt{σ′} @tt{ ∈ } (call @tt{χ} (addr @tt{a})) @t{ and }
              @tt{χ′ ⊑ χ})))
  
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

  (define/staged (what-I-did stage) #:stages [all focus]
    ;; Put "slogan" in a frame without a bottom so we can make it look like
    ;; it's part of the shadow frame containing the slogan
    (cc-superimpose
     (vl-append
      gap-size
      (colorize @t{Done:} (if (= stage all) "medium forest green" "gray"))
      (colorize-if (>= stage focus)
                   @item[@t{Systematic optimizations } (colorize @t{[ICFP 2013]} "gray")]
                   "lightgray")
      (colorize @t{Almost done (not yet true) :} (if (= stage all) "steel blue" "gray"))
      @item[@t{Systematic summarization } (colorize @t{[HOPA 2013]} "gray")]
      ;; Not going to talk about 1NSAs
      (colorize-if (>= stage focus)
                   @item[@t{Abstract model of stack introspection } (colorize @t{[JFP best of ICFP 2012]} "gray")]
                   "lightgray")
      (colorize @t{Needs work:} (if (= stage all) "firebrick" "gray"))
      @item{Temporal reasoning through contracts})))

  (define/staged (essence-slogan stage) #:stages [items slogan slogan-zoom]
    (define frame-margin 20)
    (define frame-sep 5)
    (define slogan-pict
      (inset (shadow (big (colorize @t{Slogan} "steel blue")) 10 5) 10))
    (define white-bg
      (cc-superimpose
       (colorize (filled-rectangle (pict-width slogan-pict) (pict-height slogan-pict)) "white")
       slogan-pict))
    (define slogan-frame
      (cc-superimpose white-bg
                      (colorize (frame-without-bottom (ghost white-bg)) "gray")))
    (define the-slogan
      (shadow-frame (hc-append
                     (t "Summarization is ")
                     (tag-pict
                      (colorize-if (>= stage slogan-zoom) (t "context-sensitive") "medium forest green")
                      'context)
                     (t " memoization"))
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
       (pin-over
        (pin-over
         the-slogan
         (sub1 (+ frame-sep frame-margin))
         (+ (- (pict-height slogan-frame)) frame-margin 1) slogan-frame)
        490 200
        (show framed-contexts (>= stage slogan-zoom)))
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
      (vc-append gap-size
                 (big (hc-append (t "Share ") @Mtt{M} (t " and ") @Ξtt{Ξ} (t ",")))
                 (big (t "Get CFA2 without stack allocation"))    
                 (blank 50)
                 (show
                  (big (hc-append(t "Stack inspection: include more in ") @ctxtt{ctx}))
                  (= stage stack-inspection))))
     (colorize (text "[Essence of summarization]" null 20) "gray")))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; When do I propose to do all this crap?

  (define (timeline)
    (slide (big @t{When will this happen?})
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
           (small @t{Committee reads for a while})
           (blank 30)
           @t{Defend in September 2014}))

  (define (run-talk [sections '(intro/why useful wrong done timeline)])
    (when (memv 'intro/why sections)
      (title)
      (why-hopa))
    (when (memv 'useful sections) (run-stages useful))
    (when (memv 'wrong sections) (whats-wrong))
    (when (memv 'done sections) (what-done))
    (when (memv 'timeline sections) (timeline))))

(module+ main
  (require (submod ".." slide-deck))
  (run-talk '(done)))