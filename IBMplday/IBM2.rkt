#lang at-exp slideshow

(require unstable/gui/slideshow
         unstable/gui/ppict
         slideshow/code
         slideshow/flash
         racket/gui
         scheme/runtime-path
         slideshow/balloon slideshow/face
         "pict-helpers.rkt")

(define bg-slide-assembler
  (lambda (title sep content)
    (inset content (- margin) (- margin) 0 0)))

(define curlyT "\U1d4af")
(define curlyE "\u2130")
(define curlyO "\U1d4aa")
(define curlyR "\u211b")

(define-syntax-rule (scode e ...)
  (parameterize ([current-font-size 28])
    (code e ...)))

(define-runtime-path logo-path "../utils/prl-logo.png")
(define-runtime-path pdcfa-path "pdcfafig-small.png")
(define-runtime-path pdcfa-path2 "sprout.png")
(define-runtime-path pdcfa-path3 "addpush.png")
(define-runtime-path pdcfa-path4 "addempty.png")
(define-runtime-path pdcfa-path5 "epsilon-closure.png")
(define-runtime-path pdcfa-path6 "addpop.png")

(define-runtime-path up-path "../utils/checkmark.jpg")
(define-runtime-path down-path "../utils/xmark.png")
(define-runtime-path meh-path "Question_Mark.jpg")
(define up (scale (bitmap up-path) 1/6))
(define down (scale (bitmap down-path) 1/6))
(define meh (scale (bitmap meh-path) 1/8))

(define-runtime-path cfa2-path "cfa2fig-small.png")
(define-runtime-path scratch-path "headscratcher-small.png")
(parameterize ([current-slide-assembler bg-slide-assembler])
  (slide
   (cc-superimpose
    (bitmap logo-path)
    (vc-append
     (big (bold (para #:align 'center "Designing Precise Pushdown Higher-Order Flow Analyses")))
     (blank-line)
     (para #:align 'center (bt "J. Ian Johnson") ", David Van Horn and Olin Shivers")
     (para #:align 'center (tt "{ianj,dvanhorn,shivers}@ccs.neu.edu"))
     (t "Northeastern University")
     (t "Boston, MA, USA")))))

@slide[@big{@(t "Flow analysis is indispensible")}
       'next
       (t "Every language should have one")]

(slide #:title "Analysis is great"
       @item{Compiler optimizations}
       'next
       @item{Type reconstruction}
       'next
       @item{Deadlock detection}
       'next
       @item{Data race detection}
       'next
       @item{Code quality enforcement}
       'next
       @item{@it{Hundreds} more})

(define (greent txt) (colorize (t txt) grn))
(define (oranget txt) (colorize (t txt) "orange"))

(define (mystrike pict)  
  (cc-superimpose pict (filled-rectangle (pict-width pict) 5)))

(define (terrible txt stage fpos time impl correct fo)
  (define (good x) (cond [(not x) down]
                         [(eq? x 'blank) (blank)]
                         [(eq? x 'meh) meh]
                         [else up]))
  (define (struck x why)
    (cond [(equal? #t why) (mystrike x)]
          [else x]))
  (slide #:title txt
         (vc-append 
          (progressive-table stage (list 0 1 2 3 4) 2
                             (list (struck @t{• Annoying false positives} fpos) (good fpos)
                                   (struck @t{• Takes too long} time)           (good time)
                                   (struck @t{• Hard to implement} impl)        (good impl)
                                   (struck @t{• Hard to show correct} correct)  (good correct)
                                   (struck @t{• The good ones inherently first-order} fo) (good fo))
                             lc-superimpose cc-superimpose gap-size 10)
;; I'm going to show you a way to solve all these problems
;; but first, some history.
          (show @big{@t{We can fix this}} (< 4 stage 7))
          (show @t{First some history} (= stage 6)))))
(let ([b 'blank])
  (for ([stage 7]) (terrible "Analysis is terrible" stage b b b b b)))

;; It's important to understand the starting ideas behind flow analysis
;; and how approaches to it have evolved over the decades.

(define (regular txt) (colorize (t txt) "red"))
(define (pushdown txt) (colorize (t txt) grn))

(define takeaway 6)
(define derive! 7)
(define huh 8)
(define today 9) 
(define sparse 10)
(define (foho stage)
  (vc-append
   (small
    (columns (vl-append 15
                        @t{First-order}
                        @regular{(Hecht 77) CFG + lattice}
                        (tag-pict (show @pushdown{(Sharir & Pnueli 81) CFG + lattice + summaries} (>= stage 1)) 'sharir)
                        (tag-pict (show @pushdown{(Reps 95) PDS + idempotent semiring} (>= stage 2)) 'reps)
                        ;; show at the end
                        (tag-pict (show @regular{(Oh et al. 12) CFG + lattice + bypassing} (>= stage sparse)) 'oh))
             (vl-append 15
                        @t{Higher-order}
                        (show @regular{(Jones & Muchnick 82) Program + set constraints} (>= stage 1))
                        (show @regular{(Shivers 91) CPS + set constraints} (>= stage 2))
                        (show @regular{(Might 07) CPS + abstract small step semantics} (>= stage 3))
                        (tag-pict (pin-under (show @regular{(Might & Van Horn 10) Derived from concrete} (>= stage 4))
                                             210 -20
                                             (show (colorize (filled-flash 100 75) "yellow") (= stage derive!)))
                                  'aam)
                        (tag-pict (show @pushdown{(Vardoulakis & Shivers 10) CPS + summaries} (>= stage 5)) 'cfa2)
                        (tag-pict (show @pushdown{(Earl et al. 10) ANF + Dyck state graph} (>= stage 5)) 'pdcfa)
                        (tag-pict (cond [(= stage huh) @pushdown{?}]
                                        [(>= stage today) @pushdown{Today's talk}]
                                        [else (ghost (t "?"))])
                                  'ours)
                        (tag-pict (show @pushdown{?} (>= stage today)) 'future))))
   (show @big{@t{Graphs are a bad abstraction}} (= stage takeaway))))
(for ([i derive!])
  (slide #:title "Flow Graphs vs. Programs"
         (foho i)))
(define (influence scene)
  (let* ([sharir (find-tag scene 'sharir)]
         [reps (find-tag scene 'reps)]
         [cfa2 (find-tag scene 'cfa2)]
         [pdcfa (find-tag scene 'pdcfa)]
         [sharir-arrow (pin-arrow-line 10 scene sharir rc-find cfa2 lc-find)])
    (pin-arrow-line 10 sharir-arrow reps rc-find pdcfa lc-find)))
;; show influence
(slide #:title "Flow Graphs vs. Programs" (influence (foho takeaway)))
(slide #:title "Flow Graphs vs. Programs" (influence (foho derive!))) ;; flash!

(define (potential scene)
  (let* ([ours (find-tag scene 'ours)]
         [aam (find-tag scene 'aam)]
         [cfa2 (find-tag scene 'cfa2)]
         [pdcfa (find-tag scene 'pdcfa)]
         [cfa2-arrow
          (pin-arrow-line 10 scene cfa2 lc-find ours lc-find
                          #:start-pull 0.5 #:end-pull 0.5
                          #:start-angle (* 4/3 pi) #:end-angle (* 1/4 pi))]
         [pdcfa-arrow
          (pin-arrow-line 10 cfa2-arrow pdcfa lc-find ours lc-find
                          #:start-pull 0.5 #:end-pull 0.5
                          #:start-angle (* 4/3 pi) #:end-angle (* 1/4 pi))]
         [aam-arrow
          (pin-arrow-line 10 pdcfa-arrow aam lc-find ours lc-find
                          #:start-pull 0.5 #:end-pull 0.5
                          #:start-angle (* 4/3 pi) #:end-angle (* 1/4 pi))])
    aam-arrow))
;; influence with derivation?
(slide #:title "Flow Graphs vs. Programs" (potential (influence (foho huh))))

;; take a closer look - derivable... nice
(slide #:title "Analyses are derivable [Might & Van Horn 2010]"
       @item{Start: concrete machine semantics}
       'next
       @item{Simple transforms: put semantics in right form}
       'next
       @item{Analysis: pointwise-abstraction})

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

;; why go pushdown?
;; problem with 0CFA
(define (why? stage)
  (let* ([prog
         (vl-append 70
           (code (define (sqr x) (* x x)))
           (code (sqrt (+ #,(tag-pict (code (sqr y)) 'y) #,(tag-pict (code (sqr z)) 'z)))))]
         [arrows (arrows-back-and-forth prog (find-tag prog 'y) (find-tag prog 'z))])
    (slide #:title "Functions Without Stack"
           (vc-append
            (cond [(= stage 0) prog]
                  [else arrows])
            (show @big{@t{0CFA: Not even terminating!}} (= stage 2))))))
(for ([stage 3]) (why? stage))

(define tyellow (make-object color% #xFF #xFF #x00 0.3))
;; Shows the fixpointing algorithms for current pushdown analyses
;; Then a head-scratching clipart
;; then highlights common aspects
(slide/staged
 [first second sprout addpush addempty eps addpop scratch highlight]
 (cc-superimpose
  (vl-append
    (titlet "Pushdown Higher-Order Flow Analysis")
    (hc-append
     (pin-over
      (bitmap cfa2-path)
      220 138
      ;; highlight
      (show (colorize (filled-ellipse 90 50) tyellow)
            (= stage highlight)))
     ;; PDCFA
     (show (ct-superimpose
             (bitmap pdcfa-path)
             (vl-append
              (show (frame (bitmap pdcfa-path2)) (>= stage sprout))
              (show (frame (bitmap pdcfa-path3)) (>= stage addpush))
              (show (frame (bitmap pdcfa-path4)) (>= stage addempty)))
             (vl-append
              (show (pin-over (frame (bitmap pdcfa-path5))
                              15 110
                              (show (colorize (filled-ellipse 300 100) tyellow) (= stage highlight)))
                    (>= stage eps))
              ;; highlight pop
              (show (pin-over (frame (bitmap pdcfa-path6))
                              190 30
                              (show (colorize (filled-ellipse 50 30) tyellow) (= stage highlight)))
                    (>= stage addpop))))
           (>= stage second))))
  (show (bitmap scratch-path) (= stage scratch))))

(define struck @hc-append[@t{Analysis is } @mystrike{@t{terrible}} @t{ salvagable}])

;; Previous approaches hurt
(terrible struck 4
          'meh ;; false positives
          #t ;; time
          #f ;; implementation
          #f ;; correctness
          #t) ;; first-order

(slide #:title "Deriving Pushdown Analyses"
       @item{Start: Concrete machine semantics}
       'next
       @item{Simple transform: memoize functions}
       'next
       @item{Simple transform: store functions' calling contexts}
       'next
       @item{Simple transform: heap-allocate data}
       'next
       @item{Analysis: pointwise-abstraction}
       'next
       @big{@t{Gives semantic account of summaries}})

(terrible struck 4 'meh #t #t #t #t)

(define example
  (scode (let* ([id (λ (x) #,(tag-pict (scode x) 'idbody))]
                [app (λ (f y) #,(tag-pict (scode (f y)) 'appbody))]
                [n1 #,(tag-pict (scode (app id 1)) 'app1)]
                [n2 #,(tag-pict (scode (app id 2)) 'app2)])
           #,(tag-pict (scode (+ n1 n2)) 'body))))
(define (highlight-tagged whole tag)
  (pin-under-tag whole lt-find tag 
                 (λ (tagged) (colorize (filled-rounded-rectangle-frame tagged) "yellow"))))


(define (concrete stage)
  (define-values (app1 id1 id1memo app1memo bindn1 app2 id2 id2memo app2memo bindn2)
    (apply values (stream->list (in-range 1 11))))
  (define memo-table
    (progressive-table stage (list id1memo app1memo id2memo app2memo) 2
                       (list (scode 〈id 1〉) (scode 1)
                             (scode 〈app id 1〉) (scode 1)
                             (scode 〈id 2〉) (scode 2)
                             (scode 〈app id 2〉) (scode 2))
                       lc-superimpose cc-superimpose gap-size 5))
  (define cont-table
    (progressive-table stage (list app1 id1 app2 id2) 2
                       (list (scode 〈app id 1〉) (scode (let* (... [n1 •] ...) ...))
                             (scode 〈id 1〉) (scode (let* (... [app (λ (f y) •)] ...) ...))
                             (scode 〈app id 2〉) (scode (let* (... [n2 •]) ...))
                             (scode 〈id 2〉) (scode (let* (... [app (λ (f y) •)] ...) ...)))
                       lc-superimpose cc-superimpose gap-size 5))
  (define memo-table-placement
    (pin-over titleless-page 20 210 (vl-append (bt "Memo") memo-table)))
  (define cont-table-placement
    (let* ([gray (make-object color% #xBB #xBB #xBB)]
           [bar1 (colorize (filled-rounded-rectangle 700 38) gray)]
           [bar2 (colorize (filled-rounded-rectangle 890 38) gray)]
           [bar3 (colorize (filled-rounded-rectangle 650 38) gray)]
           [at (λ (where) (* where 41))]
           [id1lookup
            (cond [(= stage id1memo) (pin-under cont-table 0 (at 1) bar2)]
                  [else cont-table])]
           [app1lookup
            (cond [(= stage app1memo) (pin-under cont-table 0 (at 0) bar1)]
                  [else id1lookup])]
           [id2lookup
            (cond [(= stage id2memo) (pin-under cont-table 0 (at 3) bar2)]
                  [else app1lookup])]
           [app2lookup
            (cond [(= stage app2memo) (pin-under cont-table 0 (at 2) bar3)]
                  [else id2lookup])])
      (pin-over memo-table-placement 20 430 (vl-append (bt "Contexts") app2lookup))))
  (define env-table
    (let ([tab (λ rows (table 2 rows lc-superimpose cc-superimpose gap-size 5))])
      (cond [(= stage 0)
             (tab (scode id) (scode ...)
                  (scode app) (scode ...))]
            [(= stage app1)
             (tab (scode id) (scode ...)
                  (scode f) (scode id)
                  (scode y) (scode 1))]
            [(= stage id1)
             (tab (scode x) (scode 1))]
            [(= stage bindn1)
             (tab (scode id) (scode ...)
                  (scode app) (scode ...)
                  (scode n1) (scode 1))]
            [(= stage app2)
             (tab (scode id) (scode ...)
                  (scode f) (scode id)
                  (scode y) (scode 2))]
            [(= stage id2)
             (tab (scode x) (scode 2))]
            [(>= stage bindn2)
             (tab (scode id) (scode ...)
                  (scode app) (scode ...)
                  (scode n1) (scode 1)
                  (scode n2) (scode 2))]
            ;; pops have no environment
            [else (blank)])))
  (define env-table-placement
    (pin-over cont-table-placement 575 20 (vl-append (bt "Environment:") env-table)))
  (define whole (pin-over env-table-placement 20 20 example))
  (cond [(= stage 0) (highlight-tagged whole 'app1)]
        [(= stage app1) (highlight-tagged whole 'appbody)]
        [(< app1 stage app1memo) (highlight-tagged whole 'idbody)]
        [(= stage app1memo) (highlight-tagged whole 'appbody)]
        [(= stage bindn1) (highlight-tagged whole 'app2)]
        [(= stage app2) (highlight-tagged whole 'appbody)]
        [(< app2 stage app2memo) (highlight-tagged whole 'idbody)]
        [(= stage app2memo) (highlight-tagged whole 'appbody)]
        [(>= stage bindn2) (highlight-tagged whole 'body)]
        [else whole]))
(for ([stage (in-range 11)]) (slide (concrete stage)))

(define num-abstract-slides 10)
(define (abstracted stage)
  (define-values (app1 id1 id1memo app1memo bindn1 app2 id2 id2memo app2memo bindn2)
    (apply values (stream->list (in-range 1 (add1 num-abstract-slides)))))
  (define memo-table
    (progressive-table stage (list id1memo app1memo id2memo app2memo) 2
                       (list (scode 〈id σ₂〉) (scode (1 σ₂))
                             (scode 〈app σ₁〉) (scode (1 σ₂))
                             (scode 〈id σ₅〉) (scode (2 σ₅))
                             (scode 〈app σ₄〉) (scode (2 σ₅)))
                       lc-superimpose cc-superimpose gap-size 5))
  (define cont-table
    (progressive-table stage (list app1 id1 app2 id2) 2
                       (list (scode 〈app #,(tag-pict (scode σ₁) 'ctxσ1)〉)
                             (scode ((let* (... [n1 •] ...) ...) #,(tag-pict (scode σ₀) 'app1ctx)))
                             (scode 〈id #,(tag-pict (scode σ₂) 'ctxσ2)〉)
                             (scode ((let* (... [app (λ (f y) •)] ...) ...) #,(tag-pict (scode σ₁) 'id1ctx)))
                             (scode 〈app #,(tag-pict (scode σ₄) 'ctxσ3)〉)
                             (scode ((let* (... [n2 •]) ...) #,(tag-pict (scode σ₀) 'app2ctx)))
                             (scode 〈id #,(tag-pict (scode σ₅) 'ctxσ4)〉)
                             (scode ((let* (... [app (λ (f y) •)] ...) ...) #,(tag-pict (scode σ₄) 'id2ctx))))
                       lc-superimpose cc-superimpose gap-size 5))
  (define store-table
    (progressive-table stage (list app1 app1 id1 bindn1 bindn2) 2 #:ghost? #f
                       (list (addr "f") (scode id)
                             (addr "y") (cond [(< 0 stage app2) (scode 1)]
                                              [(>= stage app2)  (scode (1 2))]
                                              [else (blank)])
                             (addr "x") (cond [(< app1 stage id2) (scode 1)]
                                              [(>= stage id2)     (scode (1 2))]
                                              [else (blank)])
                             (addr "n1") (scode 1)
                             (addr "n2") (scode (1 2)))
                       lc-superimpose
                       cc-superimpose
                       gap-size 5))
  ;; name the current store
  (define store-name
    ;; σ₀ = 0       
    ;; σ₁ = 1       [app1]
    ;; σ₂ = 2, 3, 4 [id1 id1memo app1memo]
    ;; σ₃ = 5       [bindn1]
    ;; σ₄ = 6       [app2]
    ;; σ₅ = 7, 8, 9 [id2 id2memo app2memo]
    ;; σ₆ = 10      [bindn2]
    (cond [(= stage 0) (scode σ₀)]
          [(= stage app1) (scode σ₁)]
          [(< app1 stage bindn1) (scode σ₂)]
          [(= stage bindn1) (scode σ₃)]
          [(= stage app2) (scode σ₄)]
          [(< app2 stage bindn2) (scode σ₅)]
          [(>= stage bindn2) (scode σ₆)]
          [else (blank)]))
  (define start-store
    (cond [(= stage 0) (scode σ₀)]
          [(= stage app1) (scode σ₁)]
          [(memv stage (list id1 id1memo)) (scode σ₂)]
          [(= stage app1memo) (scode σ₁)]
          [(= stage bindn1) (scode σ₀)]
          [(= stage app2) (scode σ₄)]
          [(memv stage (list id2 id2memo)) (scode σ₅)]
          [(= stage app2memo) (scode σ₄)]
          [(>= stage bindn2) (scode σ₀)]
          [else (blank)]))
  (define memo-table-placement
    (pin-over titleless-page 20 210 (vl-append (bt "Memo") memo-table)))
  (define cont-table-placement
    (let* ([gray (make-object color% #xBB #xBB #xBB)]
           [bar1 (colorize (filled-rounded-rectangle 700 38) gray)]
           [bar2 (colorize (filled-rounded-rectangle 890 38) gray)]
           [bar3 (colorize (filled-rounded-rectangle 650 38) gray)]
           [at (λ (where) (* where 41))]
           [id1lookup
            (cond [(= stage id1memo) (pin-under cont-table 0 (at 1) bar2)]
                  [else cont-table])]
           [app1lookup
            (cond [(= stage app1memo) (pin-under cont-table 0 (at 0) bar1)]
                  [else id1lookup])]
           [id2lookup
            (cond [(= stage id2memo) (pin-under cont-table 0 (at 3) bar2)]
                  [else app1lookup])]
           [app2lookup
            (cond [(= stage app2memo) (pin-under cont-table 0 (at 2) bar3)]
                  [else id2lookup])])
    (pin-over memo-table-placement 20 430 (vl-append (bt "Contexts") app2lookup))))
  (define store-table-placement
    (pin-over cont-table-placement
                575 20
                (vl-append (hc-append (bt "Store: ") store-name)
                           ;; frame the table on updates
                           (cond [(memv stage (list app1 id1 bindn1 app2 id2 bindn2))
                                  (frame store-table)]                                 
                                 [else store-table]))))
  (define start-store-placement
    (pin-over store-table-placement
              575 310 (hc-append (bt "Start Store: ") start-store)))
  (define whole (pin-over start-store-placement 20 20 example))
  (cond [(= stage 0) (highlight-tagged whole 'app1)]
        [(= stage app1) (highlight-tagged whole 'appbody)]
        [(< app1 stage app1memo) (highlight-tagged whole 'idbody)]
        [(= stage app1memo) (highlight-tagged whole 'appbody)]
        [(= stage bindn1) (highlight-tagged whole 'app2)]
        [(= stage app2) (highlight-tagged whole 'appbody)]
        [(< app2 stage app2memo) (highlight-tagged whole 'idbody)]
        [(= stage app2memo) (highlight-tagged whole 'appbody)]
        [(= stage bindn2) (highlight-tagged whole 'body)]
        [else whole]))

(for ([stage (add1 num-abstract-slides)]) (slide #:title (ghost (titlet "Heaps are imprecise")) (abstracted stage)))
(slide #:title "Heaps are imprecise" (abstracted num-abstract-slides))

(slide @(hc-append @big{@t{Fixed}} @small{@t{[Vardoulakis & Shivers 10]}})
       @t{Local storage (stack frames)})

(define (stack-problem stage)
  (define whole
    (code (define (foldr #,(tag-pict (code f) 'f) #,(tag-pict (code b) 'b) lst)
            (letrec 
              ([loop
                (#,(tag-pict (code λ) 'λ) (lst)
                 (cond [(empty? lst) #,(tag-pict (code b) 'bref)]
                       [else (cons (#,(tag-pict (code f) 'fref) (car lst)) 
                                   (loop (cdr lst)))]))])
              (loop lst)))))
  (slide (cond [(= stage 0) whole]
               [(= stage 1) (encircle-tagged whole 'λ)]
               [(= stage 2) (encircle-tagged whole 'b 'f 'bref 'fref)])))
(for ([stage 3]) (stack-problem stage))

(slide @big{@t{Fixed}}
       @t{Instrument your escape analysis})

(slide #:title "Escape Analysis"
       (para "Which addresses outlive their creator's context?")
       'next
       (para "We introduce three components:")
       (item "Escaped addresses " curlyE)
       (item "Addresses owned by current function " curlyO)
       (item "The random access stack Ξ")
       'next
       (para "Lookup is now defined with")
       (para (it "ℒ(a,σ,Ξ,\u2130) = a ∈ \u2130 → σ(a), Ξ(a)")))

(define CESΞK-state
  (case-lambda
    [(c e s Ξ k) (state (closure c e) (store s) (stack Ξ) (icons k))]
    [(v s Ξ k) (state (val v) (store s) (stack Ξ) (icons k))]))
(define CESΞK-state+
  (case-lambda
    [(c e s Ξ k) (state (closure c e) s Ξ k)]
    [(v s Ξ k) (state v s Ξ k)]))
(slide
 ((CESΞK-state "x" (env "ρ") "σ" "Ξ" "κ") . ⟼ . (CESΞK-state "v" "σ" "Ξ" "κ"))
 (para (t "  ")
       (where-clause (val "v") (it " ∈ ") (use-map (it "ℒ") (use-env "ρ" "x") (store "σ") (stack "Ξ") (it curlyE))))
 'next
 ((CESΞK-state+ (val "v") (store "σ") (stack "Ξ") (fn (iclosure "λ x.e" "ρ") "κ")) . ⟼ .
  (CESΞK-state+ (clo "c") (store "σnext") (extend-map (stack "Ξ") (addr "a") (val "v")) (supscript-right (t "[]") "c")))
 (para                       #|(t " if ") ;; unnecessary to show here?
            (tuple (clo "c") (store "σnext")) (it " ∉ ")
            (use-map (it "dom") (memo "M")) |#
       (where-clause (clo "c") (it " = ") (closure "e" (extend-env "ρ" "x" (addr "a")))))
 (para (it "  ") (store "σnext") (it " = ") (extend-store "σ" "a" (val "v")))
 (para (it "  \u2130' = \u2130∩") (use-map (it curlyR) (use-map (it curlyT) (clo "c"))  (store "σnext")))
 'next
 (para (it "  ") (it curlyO) (it "' = ") (braces (addr "a")))
 'next
 (para (it "  ") (edge "L'") (it " = ") (join-map (edge "L")
                                                  (tuple (clo "c") (store "σnext"))
                                                  (tuple (icons "κ") (store "σcur") (it "Ξ") (it curlyE) (it curlyO)))))

;; return case
(slide
 ((CESΞK-state+ (val "v") (store "σ") (stack "Ξ") (supscript-right (t "[]") "c")) . ⟼ .
  (CESΞK-state "v" "σ" "Ξ'" "κ"))
 (para (t "  if ") (tuple (icons "κ") (store "σnext") (it "Ξ'") (it "\u2130'") (it curlyO)) (it "' ∈ ") (use-map (edge "L") (clo "c") (store "σcur")))
 (para (it "  ") (memo "M'") (it " = ") (extend-map (memo "M") (tuple (clo "c") (store "σcur"))
                                                  (tuple (val "v") (store "σ"))))
 (para (it "  \u2130'' = \u2130' ∪ ((\u2130 ∪ \U1d4aa) ∩ \u211b(\U1d4af(v),σ))")))

(terrible struck 4 #t 'meh #t #t #t)

;; context problem
(slide @big{@t{We're not done yet}})

(slide #:title "Too context-sensitive"
 (let* ([abstract-example (abstracted 11)]
        [σ₁ (find-tag abstract-example 'ctxσ1)]
        [σ₂ (find-tag abstract-example 'ctxσ2)]
        [σ₃ (find-tag abstract-example 'ctxσ3)]
        [σ₄ (find-tag abstract-example 'ctxσ4)]
        [arrow-outwards-from 
         (λ (path finder length θ)
            (define-values (x1 y1) (finder abstract-example path))
            (define-values (x2 y2) (values (+ x1 (* length (cos θ)))
                                           (+ y1 (* length (sin θ)))))
            (define point1 (blank))
            (define point2 (blank))
            (define with-point1 (pin-over titleless-page x1 y1 point1))
            (define with-point2 (pin-over with-point1 x2 y2 point2))
            (colorize (pin-arrow-line 20 with-point2 point2 finder point1 cc-find
                            #:line-width 5)
                      "red"))]
        [arrow1 (arrow-outwards-from σ₁ lc-find 100 (* -3/4 pi))]
        [arrow2 (arrow-outwards-from σ₂ rt-find 100 (* -1/6 pi))]
        [arrow3 (arrow-outwards-from σ₃ rc-find 100 0)]
        [arrow4 (arrow-outwards-from σ₄ lt-find 100 (* 4/3 pi))])
   (cc-superimpose abstract-example
                   arrow1 arrow2 arrow3 arrow4)))

;; sparse analysis in the future
(slide #:title "First-Order Influence" (potential (foho today)))
(slide #:title "First-Order Influence" (foho sparse))
(slide
 (let* ([whole (foho sparse)]
        [oh (find-tag whole 'oh)]
        [future (find-tag whole 'future)])
   (pin-arrow-line 10 whole oh lc-find future lc-find
                   #:start-pull 0.5 #:end-pull 0.5
                   #:start-angle (* 4/3 pi) #:end-angle (* 1/4 pi))))

(slide #:title "To Conclude"
       (item "Design: Model abstract mechanisms concretely")
       'next
       (item "Pushdown: Memo and local continuation tables")
       'next
       (item "Precise analyses: Stack allocation ⟹ precision")
       'next
       (item "(Not shown) works for control operators / GC")
       'next
       (item "Future work: sparse analysis via time travel")
       'next
       (para #:align 'center (big "Thank you")))

