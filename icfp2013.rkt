#lang at-exp slideshow

(require unstable/gui/slideshow
         rsvg
         "poppler-main.rkt"
         file/convertible
         unstable/gui/ppict
         net/sendurl
         slideshow/code
         slideshow/flash
         slideshow/play
         racket/gui/base
         scheme/runtime-path
         slideshow/balloon slideshow/face
         (rename-in "pict-helpers.rkt" [addr paddr])
         "color-scheme.rkt"
         (submod "semantics.rkt" slide-deck))
(set-page-numbers-visible! #f)

(define SCREEN-HEIGHT 768)

;; Should be in sync with texpict/code (used by pict/code)
(define comma @tt{, })
(paren-color "brown")

(define bg-slide-assembler
  (lambda (title sep content)
    (inset content (- margin) (- margin) 0 0)))

(define-syntax-rule (scode e ...)
  (parameterize ([current-font-size 28])
    (code e ...)))

(current-main-font "LModern")
;; Inconsolata for code
;; Use thicker arrows
;; Use more curvy arrows
;; Inset frames. Borders rectangle/border
;; For AAM vs OAAM, keep them together, group with {
;; Group faster/smaller better. Don't cover with popups

(define-runtime-path grinder-path "grinder.png")
(define-runtime-path logo-path "prl-logo.png")

(define-runtime-path aam-path "vanhorn2010abstract.pdf")
(define-runtime-path speedup-path "all-relative-time.pdf")
(define-runtime-path fanout-path "fanoutdot2.pdf")
(define-runtime-path lazy-path "lazydot.pdf")
(define-runtime-path graph0-path "introspective-base.pdf")
(define-runtime-path graph1-path "introspective-lazy.pdf")
(define-runtime-path graph2-path "introspective-lazyc.pdf")
(define-runtime-path comp-store-path "comp-store.pdf")

(define-runtime-path aam-png "vanhorn2010abstract.png")
(define-runtime-path speedup-png "all-relative-time.png")
(define-runtime-path fanout-png "fanoutdot2.png")
(define-runtime-path lazy-png "lazydot.png")
(define-runtime-path graph0-png "introspective-base.png")
(define-runtime-path graph1-png "introspective-lazy.png")
(define-runtime-path graph2-png "introspective-lazyc.png")
(define-runtime-path comp-store-png "comp-store.png")

(define-runtime-path curly-svg "curly.svg")

(define use-pdf? #t)
(define-syntax-rule (error-bitmap e) (error 'todo "Render bitmap ~a" 'e))
(define (pdf-scale-or-bitmap pdf-path png-path [scale-factor #f])
  (cond [use-pdf?
         (define pict
           (if scale-factor
             (scale (page->pict pdf-path) scale-factor)
             (page->pict pdf-path)))
         ;; dump rendering to png for later use.
         (with-output-to-file png-path #:exists 'replace
           (λ () (write-bytes (convert (pict->bitmap pict) 'png-bytes))))
         pict]
        [else (bitmap png-path)]))
(define aam-pict (delay (pdf-scale-or-bitmap aam-path aam-png 0.7)))
(define speedup-pict (delay (pdf-scale-or-bitmap speedup-path speedup-png)))
(define fanout-pict (delay (pdf-scale-or-bitmap fanout-path fanout-png)))
(define lazy-pict (delay (pdf-scale-or-bitmap lazy-path lazy-png)))
(define graph0-pict (delay (pdf-scale-or-bitmap graph0-path graph0-png 0.15)))
(define graph1-pict (delay (pdf-scale-or-bitmap graph1-path graph1-png 0.15)))
(define graph2-pict (delay (pdf-scale-or-bitmap graph2-path graph2-png 0.15)))
(define comp-store-pict (delay (pdf-scale-or-bitmap comp-store-path comp-store-png 0.4642)))

(define (title)
 (parameterize ([current-slide-assembler bg-slide-assembler])
   (slide
    (cc-superimpose
     (bitmap logo-path)
     (vc-append
      (with-size 45 (bold
                      (vc-append gap-size (colorize @it{Optimizing} '(30 30 110))
                                 (colorize @t{Abstract Abstract Machines} '(15 15 74)))))
      (blank 40)
      (with-size 24
       (ht-append gap-size
        (vc-append
              (hc-append (colorize @bt{J. Ian Johnson} "navy") @t{,})
              (t "Nicholas Labich, David Van Horn")
              (small (tt "{ianj,labichn,dvanhorn}@ccs.neu.edu"))
              (blank-line)
              (vc-append @t{Northeastern University}
                         @t{Boston, MA, USA}))
        (vc-append
         @t{Matt Might}
         (ghost (t "David Van Horn"))
         (small (tt "matt@might.net"))
         (blank-line)
         (vc-append @t{University of Utah}
                    @t{Salt Lake City, UT, USA})))))))))

(define (greent txt) (colorize (t txt) grn))
(define (oranget txt) (colorize (t txt) "orange"))
(define (light-gray txt) (colorize (t txt) (list 200 200 200)))
(define tyellow (make-object color% #xFF #xFF #x00 0.3))

(define (intro stage)
 (slide (cc-superimpose
         (with-size 60 (para #:align 'center (colorize @it{Abstract} '(15 15 74)) @t{abstract machines?}))
         (show (shadow-frame (force aam-pict)) (>= stage 1))
         (show (shadow-frame (with-size 60 (with-font "DejaVu Serif" (colorize @t{x 1000} "midnightblue"))))
               (= stage 2)))))

(define (grinder stage)
  (λ (n)
     (define in-anchor (blank 0))
     (define out-anchor (blank 0))
     (define analysis (let ([p (inset (with-size 48 @t{Analysis}) 3)])
                        (pict-if (= stage 0) p (cc-superimpose (thick-filled-rounded-rectangle
                                                                (pict-width p)
                                                                (pict-height p)
                                                                0
                                                                #:border-color "black"
                                                                #:color "slategray")
                                                               (colorize p "white")))))
     (define program (show @t{Program} (= stage 1)))
     (define report (show @t{Report} (= stage 1)))
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
            (pin-arrow-line 15 analysis-box analysis cb-find report ct-find
                            #:line-width 3
                            #:under? #t)
            #:line-width 3
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
     (define overall
       (pin-arrow-line 15
                     (pin-arrow-line 15 anchored-grinder semantics cb-find
                                     in-anchor ct-find
                                     #:line-width 3)
                     out-anchor rc-find
                     analysis lc-find
                     #:line-width 3))
     (pin-over (ghost overall) -200 0 overall)))

(define ((bracket superimpose) width height brack-height)
  (define brack (filled-rectangle width brack-height))
  (rb-superimpose
   (superimpose brack
                (filled-rectangle (/ width 2) height))
   brack))
(define bracket-right (bracket rt-superimpose))
(define bracket-left (bracket lt-superimpose))

(define (analysis-qualities stage)
  (define-values (intro show-qualities aam oaam) (apply values (range 4)))
  (define-values (sound prec perf maint)
    (with-size 40
     (values @t{Soundness}
             (if (= stage aam) (colorize @it{Precision} "dimgray") @t{Precision})
             @t{Performance}
             @t{Maintainability})))

  (define aamcite (with-size 24 (colorize-if (>= stage oaam) 
                                             (vc-append @t{[Van Horn and Might ICFP 2010]} @t{(AAM)})
                                             "gray")))
  (define oaamcite (with-size 24 (colorize (vc-append @t{[Johnson, et al. ICFP 2013]} @t{(OAAM)}) "navy")))
  (define base
    (vc-append gap-size
               (show sound (> stage intro))
               (show maint (> stage intro))
               (show prec (> stage intro))
               (if (= stage aam) (colorize perf "gray") perf)))
  (define cites
    (pin-over
      (pin-over base (* -0.8 (pict-width aamcite)) (- (pict-height aamcite)) (show aamcite (>= stage aam)))
      (- (pict-width base) 25) (+ (pict-height base) (pict-height oaamcite))
      (show oaamcite (>= stage oaam))))
  (define-values (x y) (lc-find cites prec))
  (slide #:title "An ideal static analysis has"
         (ht-append
          (show (colorize-if (>= stage oaam) (bracket-left 20 y 10) "gray") (>= stage aam))
          cites
          (show (bracket-right 20 (pict-height cites) 10)
                (>= stage oaam)))))

(define (chop-at min max i)
  (cond [(< i min) min]
        [(> i max) max]
        [else i]))

(define (talk-focus stage)
  (define-values (anim dec fast focus goal revisit) (apply values (range 6)))
  (define fewer-col "medium forest green")
  (define faster-col "slateblue")
  (define count (box 0))
  (define (numbers)
    (for/vector #:length 3 ([i (in-range 3)])
      (t (format "~a." (add1 i)))))
  (define (item-space last . s)
    (keyword-apply item '(#:bullet) (list (apply
                                           cc-superimpose
                                           (append (cons (ghost (scale bullet 2))
                                                         (for/list ([i (in-vector (numbers))]) (ghost i)))
                                                   (list last))))
                   s))
  (define (bullet-item . s) (apply item-space (scale bullet 2) s))
  (define in-talk (if (= stage anim)
                      (λ (n . s) (bullet-item s))
                      (λ (n . s) (apply item-space (vector-ref (numbers) n) s))))
  (define in-paper (if (>= stage focus)
                       (λ (p) (colorize (bullet-item p) "gray"))
                       bullet-item))
  (define (if-paper paper-only? p)
    (if (and (number? paper-only?) (> stage fast))
        (in-talk paper-only? p)
        (in-paper p)))
  (define (decrease paper-only? p)
    (colorize-if
     (or (and (>= stage dec) (number? paper-only?) (< stage revisit))
         (= stage dec)
         (= stage fast))
     (if-paper paper-only? p) fewer-col))
  (define (faster paper-only? p)
    (colorize-if
     (or (and (>= stage fast) (number? paper-only?) (< stage revisit))
         (= stage fast))
     (if-paper paper-only? p)
     faster-col))
  (with-size 44
    (define-values (i0 i1 i2 i3 i4 i5 i6)
      (values (decrease 'paper @t{Store-allocate values})
              (decrease 'paper @t{Frontier-based semantics})
              (decrease 0 @t{Lazy non-determinism})
              (decrease 1 @t{Abstract compilation})
              (faster 2 @t{Locally log-based store-deltas})
              (faster 'paper @t{Store-counting})
              (faster 'paper @t{Mutable frontier and store})))
    (define is (vector i0 i1 i2 i3 i4 i5 i6))
    (define off-screen (blank 0))
    (define placement*
      (parameterize ([current-gap-size (* 2 gap-size)])
        (apply vl-append (for/list ([i (in-vector is)]) (ghost i)))))
    (define bars (let-values ([(x0 y0) (lt-find placement* i0)]
                              [(x1 y1) (lb-find placement* i3)])
                   (define base
                     (ht-append
                      (show (colorize
                             (hc-append gap-size
                                        (rotate (with-size 28 @t{Decrease state space}) (/ pi 2))
                                        (bracket-left 20 (add1 (- y1 y0)) 10))
                             fewer-col)
                            (and (>= stage dec) (< stage goal)))
                      placement*))
                   (define-values (x2 y2) (rt-find base i4))
                   (define-values (x3 y3) (rb-find base i6))
                   (pin-over base x2 y2
                             (show (colorize
                                    (hc-append gap-size
                                     (bracket-right 20 (add1 (- y3 y2)) 10)
                                     (rotate (with-size 28 @t{Explore faster}) (/ pi -2))) faster-col)
                                   (and (>= stage fast) (< stage goal))))))
    (define-values (x y) (lt-find bars i0))
    (define placement
      (pin-over bars x SCREEN-HEIGHT off-screen))
    ;; unit-interval ∈ [0, 1].
    ;; When unit-interval ∈ [min, max], uniformly scale from 0 to 1 as min approaches max.
    ;; When unit-interval < min. 0
    ;; When unit-interval > max. 1
    (define (chopped-interval-scale min max)
      (define 1/distance (/ 1 (- max min)))
      (λ (unit-interval)
         (cond [(< unit-interval min) 0]
               [(> unit-interval max) 1]
               [else (* 1/distance (- unit-interval min))])))
    (λ (n)
       (define num (vector-length is))
       (cc-superimpose
        (for/fold ([p* placement]) ([i (in-range num)])
          (define ipict (vector-ref is i))
          (slide-pict p* ipict off-screen ipict
                      (fast-start ((chopped-interval-scale (/ i num) (min 1 (/ (add1 i) (- num 2)))) n))))
        (show (with-size 48 (shadow-frame @t{Goal: Directly implementable math}))
              (= stage goal))))))
(define focus-title (with-size 50 @t{OAAM outline}))

(define (bench-overview stage)
  (define AAM-point (blank 0))
  (define OAAM-point (blank 0))
  (define AAM-text (with-size 48 @t{AAM}))
  (define OAAM-text (with-size 48 @t{OAAM}))
  (define-values (AAM-x AAM-y) (values 40 245))
  (define-values (OAAM-x0 OAAM-y0) (values 742 0))
  (define-values (OAAM-x1 OAAM-y1) (values 750 175))
  (define speedup
    (pin-over
     (pin-over
      (pin-over
       (pin-over
        (pin-over
         (force speedup-pict) OAAM-x0 OAAM-y0 (ellipse 15 OAAM-y1))
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
              (show (shadow-frame (with-size 48 (colorize @t{minimal effort,}
                                                          "medium forest green")))
                    (> stage 1))
              (show (shadow-frame (with-size 48 (colorize @t{simple proofs}
                                                          "medium forest green")))
                    (> stage 2)))))

(define (drama stage)
  (define hipster "Aliqua put a bird on it pour-over DIY cupidatat Truffaut American Apparel paleo et sed Pariatur leggings photo booth beard Craft beer sed brunch eiusmod readymade Anim put a bird on it odio Cosby sweater Cupidatat raw denim placeat selvage bicycle rights gentrify Brooklyn dolor roof party Excepteur laborum fingerstache dreamcatcher artisan")
  (define words (string-split hipster))
  (define junk (for/vector ([w (in-list words)])
                 (typeset-code (datum->syntax #f (string->symbol w)))))
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
                   ([(indent) 8]
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

(define fake-match (with-size 3 (drama 1)))
(define (matches show?)
  (slide (cc-superimpose
          fake-match
          (show (shadow-frame (with-size 42 @t{Compile away dispatch overhead})) show?))))

(define (boucher-feeley p)
  (rt-superimpose
   p
   (shadow-frame (with-size 24 (colorize @t{[Boucher & Feeley CC 1996]} id-color)))))

(define aam->oaam
  (let ()
    (define-values (graph slazy scomp sdelt hlazy hcomp hdelt) (apply values (range 7)))
    (define (hilight b t)
      (define t* (colorize-if b t "white"))
      (pict-if b
               (filled-rounded-rectangle-frame t* #:color "slategray" #:scale 1.02)
               t*))
   (define (pin-many-ellipses-over base xyxys show?)
     (for/fold ([p base]) ([xyxy (in-list xyxys)])
       (match-define (list x0 y0 x1 y1) xyxy)
       (define ew (add1 (- x1 x0)))
       (define eh (add1 (- y1 y0)))
       (pin-over p x0 y0 (show (thick-ellipse ew eh 3 "medium forest green") show?))))
   (define-values (lazy comp delt)
     (with-size 60
       (values (item #:bullet @t{1.} @t{Lazy non-determinism})
               (item #:bullet @t{2.} @t{Abstract compilation})
               (item #:bullet @t{3.} @t{Store deltas}))))
   (λ (stage)
      (define (surround-stuff base)
        (hc-append
         (pin-many-ellipses-over
          base
          '((40 198 68 213)
            (40 294 66 310)
            (68 294 104 310)
            (86 389 120 404)
            (37 421 63 434)
            (41 446 64 458)
            (85 478 119 491)
            (41 555 65 565))
          (or (= stage slazy) (= stage hlazy)))
         (blank 100)
         (para (show (hilight (= stage hlazy) lazy) (>= stage slazy))
               (show (hilight (= stage hcomp) comp) (>= stage scomp))
               (show (hilight (= stage hdelt) delt) (>= stage sdelt)))))
      (λ (n)
         (cond [(= stage sdelt)
                (define starting (force graph0-pict))
                (define template (surround-stuff (ghost starting)))
                (define-values (x y) (lt-find template starting))
                (define nf (fast-start n))
                (define dy
                  (if (>= nf 1/2)
                      ;; halfway through, jump to top of the screen and slide back to original position.
                      ;; dy ∈ [-height, y], so (2nf - 1)*y + (1 - (2nf - 1))*-height
                      (+ (* y (sub1 (* nf 2)))
                         (* (pict-height starting) (- (* 2 nf) 2)))
                      ;; First half, the picture shoots off the screen, dy ∈ [y, SCREEN-HEIGHT]
                      (+ (* SCREEN-HEIGHT 2 nf) (* y (- 1 (* 2 nf))))))
                (pin-over template x dy starting)]
               [else
                (surround-stuff (force graph0-pict))])))))
(define aam-title (with-size 42 @t{Finite reduction relation = graph}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ((proof-discussion stage) n)
  (define base
    (vc-append gap-size
               @t{The proof is interesting (a simple use of WEBs).}
               @t{It is a complete abstraction,}
               @t{but not if the store is global.}
               (show
                @t{Roughly, stores don't line up} (> stage 0))
               (show (pin-over
                      (pin-over (blank 300)
                                (* n 125) 0 (rectangle 100 200))
                      (- 300 (* n 175)) 0 (force comp-store-pict))
                     (> stage 0))))
  (cond
   [(> stage 0)
    (define origin (blank 0))
    (define y-axis (blank 0))
    (define x-axis (blank 0))
    (define bottom-distance 200)
    (define origin-x 0)
    (define origin-y (- SCREEN-HEIGHT bottom-distance))
    (define y-length 120)
    (define x-length 150)
    (define x-axis-x (+ origin-x x-length))
    (define y-axis-y (- origin-y y-length))
    ;; Draw a legend for addresses × abstraction
    (define anchors
      (pin-over
       (pin-over
        (pin-over base origin-x origin-y origin)
        origin-x y-axis-y y-axis)
       x-axis-x origin-y x-axis))
    (define ytext (colorize (rotate (with-size 24 @t{Addresses}) (/ pi 2)) "slateblue"))
    (define xtext (colorize (with-size 24 @t{Abstraction}) "slateblue"))
    (pin-over
     (pin-over
      (pin-arrow-line
       10
       (pin-arrow-line 10 anchors origin cc-find x-axis cc-find
                       #:line-width 3 #:color "slategray")
       origin cc-find y-axis cc-find
       #:line-width 3 #:color "slategray")
      (- origin-x (pict-width ytext) 5)
      ;; halfway between origin and y-axis
      (+ y-axis-y (/ y-length 2) (* -1/2 (pict-height ytext)))
      ytext)
     (+ origin-x (/ x-length 2) (* -1/2 (pict-width xtext)))
     (+ origin-y 5)
     xtext)]
   [else base]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Store deltas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (dinky)
  (code (code:line (define (dinky-helper-function x lst)
                     (map (λ (y) (- y x)) lst)))))
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
         (apply vl-append
          (for/fold ([acc '()]) ([form (in-list all-forms-rev)]
                                 [i (in-naturals)])
            (define (itag p) (tag-pict (typeset-code p) (formi i)))
            (cond
             [(= i (quotient len 2))
              (list* (itag form) (tag-pict (dinky) 'helper) acc)]
             [else (list* (itag form) acc)])))
         len)))))

(define context
  (let ()
    (define-values (scene recall #;point sstore schange scascade) (apply values (range 5)))
    (define-values (p* len) (with-size 3 (a-program)))
    (define p
      (pin-under-all p* 'helper
                     (filled-flash-frame
                      (ghost (with-size 3 (dinky)))
                      #:color (make-object color% #xFF #xFF #x00 0.8))))
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
       (cond [(= stage scene) p]
             [(= stage recall)
              (define ballooned (wrap-balloon (dinky) 'n 5 -120 "white" 32))
              (pin-balloon ballooned p (find-tag p 'helper) cb-find)]
#;
             [(= stage point) ;; point at helper
              (define helper-path (find-tag p 'helper))
              (define-values (x y) (rt-find p helper-path))
              (define impact @t{Small impact on σ})
              (pin-arrow-line
               15
               (pin-over p (+ x (/ (pict-width impact) 2))
                         (- y (* 2 (pict-height impact)))
                         impact)
               impact lb-find (car helper-path) rt-find)]
             [(= stage sstore) ;; show "store"
              (pin-under p 0 0 (rectangle (pict-width p) (pict-height p)))]
             [(= stage schange) ;; show small change to store
              (pin-under (bump p (find-tag p 'helper))
                         0 0 (rectangle (pict-width p) (pict-height p)))]
             [(= stage scascade) ;; show cascaded changes
              (for/fold ([p* p]) ([i (in-range (- len 2))])
                (pin-under (bump p* (find-tag p (formi i)) (random 150))
                           0 0 (rectangle (pict-width p) (pict-height p))))]))))

(define (run-talk [sections '(intro slow motivation recap
                              outline lazy abscomp abscomp-proof deltas
                              evaluation wrapup)])
  (define-syntax-rule (section name body ...) (when (memv 'name sections) body ...))
  (section intro
   (title)

   (for ([i 3]) (intro i))

   (slide ((grinder 0) 0)))

  (section slow
   (play (grinder 1) #:steps 100)
   (slide ((grinder 1) 1)))

  (section motivation
   (for ([i 4]) (analysis-qualities i))
   (play (talk-focus 0) #:steps 60 #:title focus-title)
   (for ([i (in-range 1 5)]) (slide #:title focus-title ((talk-focus i) 1))))

  (section recap
    (slide (with-size 60 @t{First: recap of AAM})))

  (parameterize ([pushdown? #f]
                 [use-color? #f])
    (section recap
     (slide (CESK-table sCESK))
     (slide (CESK-table sCESK*))
     (slide (CESK-table saCESK)))

    ;; AAM produces graphs
    (section outline
     (for ([i 3]) (slide ((aam->oaam i) 0) #:title aam-title))
     (play (aam->oaam 3) #:title aam-title #:skip-first? #t)
     (slide ((aam->oaam 3) 1) #:title aam-title))

    (section lazy
      (slide ((aam->oaam 4) 0) #:title (ghost aam-title)) ;; highlight lazy
      (slide (CESK-table lazy))

      (slide (code (f x y))
             (force fanout-pict)
             'next
             (vc-append (arrow 20 (/ pi -2))
                        (force lazy-pict)))
      (slide (hc-append (* 2.5 gap-size)
                        (force graph0-pict)
                        (arrow 20 0)
                        (force graph1-pict))))

    (section abscomp

     (slide ((aam->oaam 5) 0) #:title (ghost aam-title)) ;; highlight comp
     (slide (boucher-feeley (CESK-table abscomp)))

     (slide (with-size 26 (drama 0)))
     (both matches)

     (slide (vc-append
             (hc-append (t "\u27e6\u2022\u27e7") @tt{:} (t "Expr \u2192 Env \u00d7 Store \u00d7 Kont \u2192 State"))
             (parameterize ([abs-comp? #t]) (CESK-table saCESK))))
     (slide (hc-append (* 2.5 gap-size)
                       (force graph1-pict)
                       (arrow 20 0)
                       (force graph2-pict))))

    (section abscomp-proof
     (slide ((proof-discussion 0) 0))
     (play (proof-discussion 1) #:steps 15)
     (slide ((proof-discussion 1) 1)))

    (section deltas

     (slide ((aam->oaam 6) 0) #:title (ghost aam-title)) ;; highlight deltas
     (slide (CESK-table σ-big)))
    )

  (section deltas
    (slide #:title (with-size 40 (hc-append @t{Store widening } (bold @t{(WIP slide)})))
           (item @t{All explored states share one store})
           (item @t{Reusing ↦ produces full stores})
           (item @t{The next store is the ⨆ of those}))

    (slide (code ...
                 #,(dinky)
                 ...))

    (for ([i 5]) (slide (context i))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Evaluation section
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (section evaluation
   (for ([i 4]) (slide (bench-overview i))))


  (section wrapup
   (slide ((talk-focus 5) 1) #:title focus-title)
   (slide ((talk-focus 0) 1) #:title focus-title)
   (slide #:title (with-size 50 @t{Before we part})
          (with-size 42
            (vc-append gap-size
                       @t{A few guidelines for implementing analyses}
                       @t{Intuitive, simple, and competitive}))
          'next
          (blank 70)
          (with-size 42
            (vc-append gap-size
                       @t{A simple matter of engineering?}
                       @t{Build your tricks into the semantics}))
          'next
          (blank 70)
          (with-size 72 (colorize @t{Thank you} "dark slate blue")))))

(module+ main
 (define dummy (run-talk '(recap)))
  '|That's all, folks!|
)