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
     (small (t "2013 November 18"))))))

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
   (for ([i num]) (do i))])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Why HOPA?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions
(define/staged (tail-jail stage) #:num-stages 3
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

(define ((scale-to-0/1 from to) n)
  (cond [(<= n from) 0]
        [(>= n to) 1]
        [else (define scale (/ (- to from)))
              (+ (* n to) (* (- 1 n) from))]))

(define (roll-and-fade-in from to picts)
  (match picts
    [(cons fst rest)
     (define N (add1 (length rest)))
     (pin-over (blank)
               0 0 (fade-pict (ghost fst) fst))]
    [_ (λ (n) (blank))]))

(define/staged (useful stage) #:stages [base anim to-name-a-few] #:anim-at anim #:steps 30
  (define big-font 28)
  (define base-pict (big @t{HOPA is useful}))
  (define (vlappend-vec v) (apply vl-append 3 (vector->list v)))
  (define (anim-fn n)
    (with-size 16
     (define superβ @t{(Super-beta) inlining})
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
     (define opts (vector superβ safety global arity unbox future uve ive vect fusion strict lazy strength))
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
     (cc-superimpose base-pict
      (hc-append 20
                 ((slide-and-compose opt-base opts superβ fader)
                  (scale0 n))
                 ((slide-and-compose (fader understanding-base (scale0 n)) understandings race fader)
                  (scale1 n))
                 ((slide-and-compose (fader verification-base (scale1 n)) verifications temporal fader)
                  (scale2 n))))))
  
  (match stage
    [(== base) base-pict]
    [(== anim) anim-fn]
    [(== to-name-a-few)
     (cc-superimpose (anim-fn 1.0) 
                     (shadow-frame (big @t{To name a few})))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slides
(slide (big @t{Why care about HOPA?}))
(run-stages tail-jail)
(run-stages universal)
(run-stages useful)


;; TODO
#| Example clients
Optimization:
 - future optimization
 - (super-β) inlining
 - safety test removal (types, vector accesses, nullability)
 - globalization
 - arity-raising
 - useless variable elimination
 - unboxing
 - loop fusion 
 - vectorization
 - induction variable elimination
 - strictness promotion
 - strength reduction (call/cc → abort)
Program understanding:
 - data race detection
 - information flow for security
 - laziness analysis
 - termination analysis
 - complexity analysis
 - dependence analysis
 - type inference
 - shape analysis
Verification:
 - temporal logic model-checking
 - behavioral contract validity
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What's wrong with HOPA?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slides

(slide (big (t "What's wrong with HOPA?")))
(slide #:title "Intrinsically"
       (vc-append @t{Soundness}
                  (hc-append @t{Speed} (blank 400) @t{Precision})))
(slide #:title "Extrinsically"
       (vc-append @t{Maintainability}
                  (hc-append @t{Designability} (blank 400) @t{Grokability})))
(run-stages temporal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What have I done for HOPA?

(slide (big (t "What have I done for HOPA?")))
(slide @item{Systematic optimizations}
       @item{Systematic summarization}
       @item{Abstract model of stack introspection}
       @item{Concrete summarization of introspection}
       @item{Temporal reasoning through contracts})

(slide (big (t "What do I propose to do for HOPA?")))
