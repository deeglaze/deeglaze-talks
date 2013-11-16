#lang at-exp slideshow

(require (except-in unstable/gui/slideshow stage)
         unstable/gui/ppict
         net/sendurl
         slideshow/code
         slideshow/flash
         slideshow/play
         racket/gui/base
         scheme/runtime-path
         slideshow/balloon slideshow/face
         slideshow-helpers/slide
         (rename-in "pict-helpers.rkt" [addr paddr])
         "color-scheme.rkt"
         (submod "semantics.rkt" slide-deck)
         (submod "historical.rkt" slide-deck)
         (submod "hammer-slide.rkt" slide-deck)
         (submod "proof-diagram.rkt" slide-deck)
         (submod "call-ret.rkt" slide-deck))
(set-page-numbers-visible! #f)
#|
Outline:
Two things I want to communicate: PD easy, concrete models
What's easy? Systematic construction [with structural proofs].
What isn't easy? CFA2 and PDCFA.
But if you squint, they do the same thing. Memoization and saving calling contexts.
What does memoization look like in the CESK machine?
Calling contexts fit right in.
The store-widening also widens the memo CC table.
Okay, soo first-class control?
Continuations as values must be abstracted.
Second part of the conversation: concrete makes this easy [demo?]
|#

;; Should be in sync with texpict/code (used by pict/code)
(paren-color "brown")

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

(define logo-path (collection-file-path "prl-logo.png" "talk-utils"))
(define-runtime-path pdcfa-path "pdcfafig-small.png")
(define-runtime-path pdcfa-path2 "sprout.png")
(define-runtime-path pdcfa-path3 "addpush.png")
(define-runtime-path pdcfa-path4 "addempty.png")
(define-runtime-path pdcfa-path5 "epsilon-closure.png")
(define-runtime-path pdcfa-path6 "addpop.png")

(define-runtime-path cfa2-path "cfa2fig-small.png")
(define-runtime-path cfa2-1st-path "cfa2-1st-class-small.png")
(define-runtime-path scratch-path "headscratcher-small.png")
(define-runtime-path square-circle-path "square-peg-round-hole.jpg")

(define tyellow (make-object color% #xFF #xFF #x00 0.3))

(module+ utils
  (provide txt-overlay)
  (define (txt-overlay p txt)
    (let ([txt (text txt null 48)])
      (cc-superimpose p
                      (rotate (shadow-frame
                               (cc-superimpose
                                (colorize
                                 (scale
                                  (filled-rounded-rectangle
                                   (pict-width txt)
                                   (pict-height txt))
                                  1.3)
                                 "white")
                                txt))
                              (* pi 1/16))))))

(module+ slide-deck
  (provide convince concrete)
  
  (define/staged convince #:num-stages 4
    (define (light-on txt b)
      (if b (colorize (t txt) (list 200 200 200)) (t txt)))
    (parameterize ([current-font-size 48])
      (vl-append 15
                 (light-on "Two things:" (>= stage 3))
                 (show (light-on "Pushdown analysis is easy" (= stage 4)) (>= stage 1))
                 (show (light-on "You should model your analyses concretely" (= stage 3)) (>= stage 2)))))


  (define/staged concrete #:stages [base app1 id1 id1memo app1memo bindn1 app2 id2 id2memo app2memo bindn2]
    (define example
      (scode (let* ([id (λ (x) #,(tag-pict (scode x) 'idbody))]
                    [app (λ (f y) #,(tag-pict (scode (f y)) 'appbody))]
                    [n1 #,(tag-pict (scode (app id 1)) 'app1)]
                    [n2 #,(tag-pict (scode (app id 2)) 'app2)])
               #,(tag-pict (scode (+ n1 n2)) 'body))))
    (define (highlight-tagged whole tag)
      (pin-under-tag whole lt-find tag 
                     (λ (tagged) (colorize (filled-rounded-rectangle-frame tagged) "yellow"))))


    (define memo-table
      (progressive-table stage (list id1memo app1memo id2memo app2memo) 2
                         (list (angles (scode x ρ₁ σ₂)) (angles (scode 1 σ₂))
                               (angles (scode (f y) ρ₁ σ₁)) (angles (scode 1 σ₂))
                               (angles (scode x ρ₅ σ₅)) (angles (scode 2 σ₅))
                               (angles (scode (f y) ρ₄ σ₄)) (angles (scode 2 σ₅)))
                         lc-superimpose cc-superimpose gap-size 5))
  
    (define cont-table
      (progressive-table stage (list app1 id1 app2 id2) 2
                         (list (angles (scode (f y) ρ₁ σ₁)) (scode (let* (... [n1 •] ...) ...))
                               (angles (scode x ρ₁ σ₂)) (scode (let* (... [app (λ (f y) •)] ...) ...))
                               (angles (scode (f y) ρ₄ σ₄)) (scode (let* (... [n2 •]) ...))
                               (angles (scode x ρ₅ σ₅)) (scode (let* (... [app (λ (f y) •)] ...) ...)))
                         lc-superimpose cc-superimpose gap-size 5))
  
    (define store-table
      (progressive-table stage (list app1 app1 id1 bindn1 app2 id2 bindn2) 2 #:ghost? #f
                         (list (cond [(< base stage app2) (paddr "f₀")]
                                     [(>= stage app2) (paddr "f₀,f₁")]
                                     [else (blank)]) (scode id)
                                     (paddr "y₀") (cond [(< base stage) (scode 1)]
                                                        [else (blank)])
                                     (paddr "x₀") (cond [(< app1 stage) (scode 1)]
                                                        [else (blank)])
                                     (paddr "n1₀") (scode 1)
                                     (paddr "y₁") (cond [(>= stage app2)  (scode 2)]
                                                        [else (blank)])
                                     (paddr "x₁") (cond [(>= stage id2) (scode 2)]
                                                        [else (blank)])
                                     (paddr "n2₀") (scode 2))
                         lc-superimpose
                         cc-superimpose
                         gap-size 7))
  
    (define store-name
      ;; σ₀ = 0       
      ;; σ₁ = 1       [app1]
      ;; σ₂ = 2, 3, 4 [id1 id1memo app1memo]
      ;; σ₃ = 5       [bindn1]
      ;; σ₄ = 6       [app2]
      ;; σ₅ = 7, 8, 9 [id2 id2memo app2memo]
      ;; σ₆ = 10      [bindn2]
      (cond [(= stage base) (scode σ₀)]
            [(= stage app1) (scode σ₁)]
            [(< app1 stage bindn1) (scode σ₂)]
            [(= stage bindn1) (scode σ₃)]
            [(= stage app2) (scode σ₄)]
            [(< app2 stage bindn2) (scode σ₅)]
            [(>= stage bindn2) (scode σ₆)]
            [else (blank)]))
  
    (define start-store
      (cond [(= stage base) (t "N/A")]
            [(= stage app1) (scode σ₁)]
            [(memv stage (list id1 id1memo)) (scode σ₂)]
            [(= stage app1memo) (scode σ₁)]
            [(= stage bindn1) (t "N/A")]
            [(= stage app2) (scode σ₄)]
            [(memv stage (list id2 id2memo)) (scode σ₅)]
            [(= stage app2memo) (scode σ₄)]
            [(>= stage bindn2) (t "N/A")]
            [else (blank)]))
  
    (define memo-table-placement
      (pin-over titleless-page 20 210 (vl-append (bt "Memo") memo-table)))
    (define cont-table-placement
      (let* ([gray (make-object color% #xBB #xBB #xBB)]
             [bar1 (colorize (filled-rounded-rectangle 715 38) gray)]
             [bar2 (colorize (filled-rounded-rectangle 904 38) gray)]
             [bar3 (colorize (filled-rounded-rectangle 650 38) gray)]
             [at (λ (where) (* where 39))]
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
                (vl-append (hc-append (bt "Store:") store-name)
                           ;; frame the table on updates
                           (cond [(memv stage (list app1 id1 bindn1 app2 id2 bindn2))
                                  (frame store-table)]                                 
                                 [else store-table]))))
    (define start-store-placement
      (pin-over store-table-placement
                575 400 (hc-append (bt "Store in rt:") start-store)))
    (define whole (pin-over start-store-placement 20 20 example))
    (cond [(= stage 0) (highlight-tagged whole 'app1)]
          [(= stage app1) (highlight-tagged whole 'appbody)]
          [(< app1 stage app1memo) (highlight-tagged whole 'idbody)]
          [(= stage app1memo) (highlight-tagged whole 'appbody)]
          [(= stage bindn1) (highlight-tagged whole 'app2)]
          [(= stage app2) (highlight-tagged whole 'appbody)]
          [(< app2 stage app2memo) (highlight-tagged whole 'idbody)]
          [(= stage app2memo) (highlight-tagged whole 'appbody)]
          [(>= stage bindn2) (highlight-tagged whole 'body)]
          [else whole])))
(module+ main
  (require (submod ".." slide-deck)
           (submod ".." utils))

  #;
  (define (cfa2-1st highlight?)
  (define box1y1 84)
  (define box2y1 258)
  (define box1 (colorize (filled-rounded-rectangle (- 390 20) (- 123 box1y1)) tyellow))
  (define box2 (colorize (filled-rounded-rectangle (- 400 20) (- 350 box2y1)) tyellow))
  (define pic (bitmap cfa2-1st-path))
  (slide (if highlight?
  (pin-over (pin-over pic 25 box1y1 box1) 25 box2y1 box2)
  pic)))

  (parameterize ([current-slide-assembler bg-slide-assembler])
    (slide
     (cc-superimpose
      (bitmap logo-path)
      (vc-append
       (big (bold (para #:align 'center "Concrete Semantics for Pushdown Analysis")))
       (bold (colorize (para #:align 'center "The Essence of Summarization") "brown"))
       (blank-line)
       (para #:align 'center (colorize @bt{J. Ian Johnson} "navy") " and David Van Horn")
       (small (para #:align 'center (tt "{ianj,dvanhorn}@ccs.neu.edu")))
       (t "Northeastern University")
       (t "Boston, MA, USA")))))

  #|
  I'm here to convince you of two things.
  First that pushdown analysis is easy,
  and secondly as a corollary that we should be focusing on concrete
  semantics when modeling future analyses.
  |#
(run-stages convince)
#|
  Why do we even care about pushdown analysis? Well, a classic analysis
  like 0CFA grows return points monotonically, and thus we get spurious
  predictions that lead to future spurious predictions in a vicious
  cycle.
  Let's step through this tiny program to exhibit the problem.
  |#
(slide (parameterize ([current-font-size 48]
                      [current-main-font (make-object font% 48 "Century Schoolbook L Roman" 'default)])
         (t "Regular v Pushdown")))


(run-stages (call/ret #f))

;; What do we actually expect for this program's execution?
(run-stages (call/ret #t))

(run-stages history)

(slide #:title "Deriving Pushdown Analyses"
       @item{Transform: memoize functions}
       'next
       @item{Transform: store return points for ENTIRE states}
       'next
       @item{Analysis: bound store})

#|
  Pushdown analysis gives us that.
  You might guess that the "pushdown" here is referring to a pushdown
  automaton / pushdown system abstraction of a program's semantics.
  And you'd be right, but what I'm going to show you is an effective
  operational analogy that works pre-abstraction, and has a dead-simple
  abstraction that results in the existing (difficult) literature for
  doing pushdown analysis for higher-order languages. They did the
  proofs of pushdown-ness, and I leverage that to say that since we end
  at the same endpoints, what I'm going to show you is also pushdown.
  |#

(both diagram)
(hammer #t)

#|
  There are opposing views on this. One is we can use existing
  technology for pushdown systems to analyze our languages. My
  counter-argument is that many languages have control structure that
  does not follow the stack discipline, making the cast into a pushdown
  automaton a difficult venture. Second, these tools are not amenable to
  online calculation of the pushdown system (PDSolver)
  |#

(slide (semantics))
(slide (the-shift))
(slide (CESK-table sCEK))
(slide (CESK-table sCESK))
(slide (CESK-table saCESK))
(slide (txt-overlay (CESK-table saCESK)
                    "(1) memoize functions"))
(play (λ (t) (CESK-table sfade t)) #:skip-first? #t)
(play (λ (t) (CESK-table smove t)))
(slide (CESK-table smove 1.0))
(slide (CESK-table sCESKM))
(slide (CESK-table sCESKM-ret))
(slide (txt-overlay
        (CESK-table sCESKM-ret)
        "(2) store return points"))
(slide (CESK-table sCESKMΞ))

(slide (text "How does this look?" null 50))
(run-stages concrete)

(slide (a-state))

(run-stages convince #:stage 4)
(slide (with-size 48 (vc-append (plug @ctt{E} (code (reset #,(plug @ctt{F} (code (shift k e))))))
                   @tt{ ↦ }
                   (plug @ctt{E} (subst @idtt{e} @idtt{k} (code (λ (x) #,(plug @ctt{F} @idtt{x})))))))
       'next
       (hc-append @ctt{F} @t{ doesn't contain any } @idtt{reset} @t{s}))
(slide #:title "Deriving Pushdown Analyses"
       @item[@t{Transform: memoize functions } (colorize @t{/ continuations} "red")]
       @item{Transform: store return points for ENTIRE states}
       @item{Analysis: bound store})

;; Shows the fixpointing algorithms for current pushdown analyses
;; Then a head-scratching clipart
;; then highlights common aspects

(slide #:title "To Conclude"
       (item "Design: Model abstract mechanisms concretely")
       'next
       (item "Pushdown: Memo and local continuation tables")
       'next
       (item "Works for control operators / GC (not shown)")
       'next
       (clickback (colorize @t{https://github.com/ianj/pushdown-shift-reset} "navy")
                  (λ () (send-url "https://github.com/ianj/pushdown-shift-reset")))
       (para #:align 'center (big (t "Thank you"))))

(slide #:title "Garbage collection"
       @t{Read root addresses of κ through Ξ}
       (hc-append @idtt{𝒯}
                  (parens @idtt{rt} (parens @idtt{e} comma @idtt{ρ} comma @idtt{σ}))
                  @tt{ = ⋃}
                  (braces @idtt{𝒯} (parens @idtt{κ}) @tt{ : }
                              @idtt{κ} @tt{ ∈ } @idtt{Ξ} (parens @idtt{e} comma @idtt{ρ} comma @idtt{σ})))))