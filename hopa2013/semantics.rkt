#lang at-exp slideshow
(require unstable/gui/slideshow
         unstable/gui/ppict
         slideshow/play
         unstable/gui/pict
         racket/gui/base
         slideshow/code
         (except-in "pict-helpers.rkt" addr)
         "color-scheme.rkt")

(module+ slide-deck
  (provide semantics the-shift CESK-table a-state pushdown? abs-comp?
           sCEK sCESK saCESK sfade smove sCESKM sCESKM-ret sCESKMΞ
           lazy σ-big abscomp)
  (require racket/trace)
  (define comma @tt{,})
  (define old-main-font (current-main-font))
  (define want (make-object font% 28 "Comic Sans" 'default))
  (current-main-font want)
  (define-syntax-rule (lam) (hc-append 3 @tt{λ} @idtt{x} @tt{.} @idtt{e}))
  (define-syntax-rule (do-want . body) (parameterize ([current-main-font want]) . body))
  (define pushdown? (make-parameter #t))
  (define abs-comp? (make-parameter #f))
  (define (showP p b) (and (pushdown?) (show p b))) ;; ntuple treats #f different from ghosts

  (define Ee (code (#,(values @ctt{E}) e)))
  (define vE (code (v #,(values @ctt{E}))))

  (define rcl (list rc-superimpose cc-superimpose lc-superimpose))

  (define (fcons e lst) (do-want (hc-append e @tt{:} lst)))
  (define (a-state)
    (vc-append
     (with-size 48
                (ntuple @idtt{e} @idtt{ρ} (σtt @tt{σ}) @ctt{κ} (showP (Mtt @tt{M}) #t) (showP (Ξtt @tt{Ξ}) #t)))
     (blank 50)
     (with-size 30
                (table 3
                       (list (hc-append @idtt{ρ} @tt{ ∈ } @t{Env})
                             @tt{ = }
                             (hc-append @t{Var → } (addr @t{Addr}))
                     
                             (hc-append (σtt @tt{σ}) @tt{ ∈ } @t{Store}) 
                             @tt{ = }
                             (hc-append (addr @t{Addr}) @t{ → ℘(Value × Env)})

                             (hc-append (Mtt @tt{M}) @tt{ ∈ } @t{Memo})
                             @tt{ = }
                             (hc-append @t{Expr × Env × Store → ℘(Value)})

                             (hc-append (Ξtt @tt{Ξ}) @tt{ ∈ } @t{KTable})
                             @tt{ = }
                             (hc-append @t{Expr × Env × Store → ℘(Kont)}))
                       (list rc-superimpose cc-superimpose lc-superimpose) cc-superimpose 30 15))
     (blank 20)
     (vl-append
      (production @ctt{κ} @tt{[]} (nstruct "rt" @idtt{e} @idtt{ρ} (σtt @tt{σ})) (fcons @idtt{φ} @ctt{κ}))
      (production @ftt{φ} (nstruct "ar" @idtt{e} @idtt{ρ}) (nstruct "fn" @idtt{v} @idtt{ρ})))))

  (define (semantics)
    ;; Pushdown is motivated. Let's talk about easy.
    (vl-append (production @idtt{e} @idtt{x} (code (e e)) (lam))
               (production @idtt{v} (lam))
               (production @ctt{E} @tt{[]} Ee vE)
               (blank 100)
               (hc-append (plug @ctt{E} (parens (lam) (blank 10) @idtt{v}))
                          @tt{ ↦} (subscript @t{βv}) @tt{ }
                          (plug @ctt{E} (hc-append @idtt{e} (braces @idtt{x} @tt{:=} @idtt{v}))))))

  (define konts
    (table 2
           (list (tag-pict @tt{[]} 'hole) (tag-pict @tt{[]} 'mt)
                 (tag-pict Ee 'Ear) (tag-pict (fcons (nstruct "ar" @idtt{e} @idtt{ρ}) @ctt{κ}) 'ar)
                 (tag-pict vE 'Efn) (tag-pict (fcons (nstruct "fn" @idtt{v} @idtt{ρ}) @ctt{κ}) 'fn))
           (list rc-superimpose lc-superimpose) cc-superimpose 100 5))
  
  (define (κ̂ σ? fresh?)
    (if (pushdown?) @ctt{κ}
        (pict-if σ? (addr (if fresh? @tt{a} @tt{b})) @ctt{κ})))
  (define (kont-items σ?)
    (cons (hc-append @ctt{κ} @tt{ ∈ } @t{Kont})
          (if (pushdown?)
              (list @tt{ = } (hc-append @t{Frame} (superscript @t{*})))
              (list (colorize @t{ ::= } "dark gray")
                    (hc-append 5.0
                               @tt{[]}
                               (colorize (t "|") "dark gray")
                               (fcons @ftt{φ} (κ̂ σ? #t)))))))

  (define (the-shift)
    (vl-append (table 3
                      (append
                       (list 
                        (hc-append @idtt{ρ} @tt{ ∈ } @t{Env})
                        @tt{ = }
                        @t{Var → (Value × Env)})
                       (kont-items #f))
                      rcl cc-superimpose gap-size 5)
               (blank 60)
               (pin-arrow-line
                10
                (pin-arrow-line
                 10
                 (pin-arrow-line 10 konts (find-tag konts 'hole) rc-find (find-tag konts 'mt) lc-find)
                 (find-tag konts 'Ear) rc-find (find-tag konts 'ar) lc-find)
                (find-tag konts 'Efn) rc-find (find-tag konts 'fn) lc-find)))

  (define (chσ*) (with24 (tag-pict (σtt @tt{σ}) 'σ)))
  (define (chσ′*) (with24 (tag-pict (σtt @tt{σ′}) 'σ′)))
  (define (ρσ σ? abs?) 
    (do-want
     (with24
      (table 3
             (append
              (list (hc-append @idtt{ρ} @tt{ ∈ } @t{Env})
                    @tt{ = }
                    (hc-append @t{Var → } (if σ? (addr @t{Addr}) @t{(Value × Env)})))
              (if (pushdown?)
                  '()
                  (kont-items σ?))
              (list (show (hc-append chσ @tt{ ∈ } @t{Store}) σ?)
                    (show @tt{ = } σ?)
                    (show (hc-append (addr @t{Addr})
                                     @t{ → } (show @t{℘} abs?) @t{(Value × Env)})
                          σ?)))
             rcl cc-superimpose gap-size 5))))
  (define ρx (with24 (call @idtt{ρ} @idtt{x})))

  (define-values (sCEK sCESK saCESK lazy σ-big abscomp sfade smove sCESKM sCESKM-ret sCESKMΞ)
    (apply values (range 11)))

  (define (CESK-table stage [time #f])
    (do-want
     (define σ? (> stage sCEK))
     (define abs? (> stage sCESK))
     (define M? (> stage smove))
     (define show-rt? (> stage sCESKM))
     (define Ξ? (> stage sCESKM-ret))
     (define fade? (= stage sfade))
     (define move? (= stage smove))
     (define lazy-problem? (= stage lazy))
     (define σ-problem? (= stage σ-big))
     (define e-problem? (= stage abscomp))

     (define OR (with24 (text "or" null 20)))
     (define stepto (lt-superimpose (ghost OR) (with24 @tt{↦})))

     ;; need for animation:
     (define (call-pict tag?)
       (tag-pict
        (with24 (ntuple (code v) @idtt{ρ} (show (if tag? (chσ*) chσ) σ?)
                        (fcons (nstruct "fn" (lam) @idtt{ρ′}) (κ̂ σ? #f))
                        (showP M M?) (showP Ξ Ξ?)))
        'call))

     (define (ev-state e . rest)
       (if (abs-comp?)
           (call e (apply hc-append 5.0 (ghost-commas rest)))
           (apply ntuple e rest)))

     (define (compile p) (hc-append (tt "\u27e6") p (tt "\u27e7")))
     (define (ev-lhs e . rest)
       (if (abs-comp?)
           (hc-append (compile e) @tt{ = λ} (apply hc-append 5.0 (ghost-commas rest)) @tt{.})
           (apply ntuple e rest)))

     (define call-elms
       (with24
        (define rhs
          (ev-state @idtt{e}
                    @idtt{ρ″}
                    (show (chσ′*) σ?)
                    (if (pushdown?)
                        (pict-cond
                         [(and M? Ξ?) (nstruct "rt" ctx)
                          #;(fcons (nstruct "rt" ctx) @tt{[]})
                          ]
                         [M? (fcons (nstruct "rt" ctx) @ctt{κ})]
                         [else @ctt{κ}])
                        @ctt{κ})
                    (showP M M?)
                    (showP Ξ′ Ξ?)))
        (append 
         (list (call-pict #t)
               stepto
               (if (pushdown?)
                   rhs
                   (hc-append rhs
                              (show (hc-append
                                     (blank 10) @ctt{κ} (pict-if abs? @tt{ ∈ } @tt{ = })
                                     (call (chσ*) (addr @tt{b})))
                                    σ?))))
         (if M?
             (list (blank 0) OR
                   (hc-append (ntuple (code v′) @idtt{ρ} @ctt{κ}
                                      (showP M #t) (showP Ξ′ Ξ?))
                              @t{ if } (code v′) @tt{ ∈ } (call M ctx)))
             (list)))))

     (define (σext elm)
       (define σ (chσ*))
       (with24
        (pict-if abs?
                 (join-one σ a (braces elm))
                 (ext-one σ a elm))))

     (define where-clause-elms
       (with24
        (append
         (list (ghost (call-pict #f))
               (ghost (lt-superimpose @t{where} OR))
               (hc-append 
                @t{where } @idtt{ρ″} @tt{ = } (ext-one @idtt{ρ′} @idtt{x}
                                                       (if σ? a (tuple @idtt{v} @idtt{ρ}))))
               (blank 0) (blank 0) (show (hc-append (ghost @t{where }) (chσ′*) @tt{ = }
                                                    (σext (tuple @idtt{v} @idtt{ρ})))
                                         σ?))
         (if M?
             (append
              (list (blank 0) (blank 0)
                    (hc-append (ghost @t{where }) ctx @tt{ = } (tuple @idtt{e} @idtt{ρ″} (chσ′*))))
              (map (λ (p) (show p Ξ?))
                   (list
                    (blank 0) (blank 0)
                    ;; XXX: Not sure whether to put truly-concrete, but not sure if right ext-one,
                    ;; or what we show in the paper, with join-one... Going with paper.
                    (hc-append (ghost @t{where }) Ξ′ @tt{ = } (join-one Ξ ctx (braces @ctt{κ}))))))
             (list)))))

     (define rt-elms
       (with24
        (map (λ (p) (show p show-rt?))
             (list ;; Return
              (ntuple (code v) @idtt{ρ} (chσ*)
                      (if (pushdown?)
                          (pict-if Ξ?
                                   (nstruct "rt" ctx)
                                   (fcons (nstruct "rt" ctx) @ctt{κ}))
                          @ctt{κ})
                      #;(fcons (nstruct "rt" ctx) (if Ξ? @tt{[]} @ctt{κ})) (showP M #t) (showP Ξ Ξ?)
                      )
              @tt{↦}
              (ntuple (code v) @idtt{ρ} (chσ*) @ctt{κ}
                      (showP M′ #t) (showP Ξ Ξ?))
              (blank 0) (blank 0)
              (show (hc-append @t{ if } @ctt{κ} @tt{ ∈ } (call Ξ ctx)) Ξ?)
              (blank 0) (blank 0)
              (hc-append @t{where } M′ @tt{ = } (join-one M ctx (braces (tuple @idtt{v} @idtt{ρ}))))))))

     (define var-condition
       (with24
        (tag-pict
         (hc-append @t{ if }
                    (tuple @idtt{v} @idtt{ρ′})
                    (pict-if abs? @tt{ ∈ } @tt{ = })
                    (if σ? (call (chσ*) ρx) ρx))
         'var-condition)))

     (define x-var
       (with24 (tag-pict @idtt{x} 'eval0)))
     (define fn-app
       (with24 (tag-pict (code (e₀ e₁)) 'eval1)))

     (define (app-rhs σid)
       (with24
        (ev-state (if (abs-comp?)
                      (compile (code e₀))
                      (code e₀)) @idtt{ρ} (show σid σ?)
                  (fcons (nstruct "ar" (if (abs-comp?)
                                           (compile @idtt{e₁})
                                           @idtt{e₁}) @idtt{ρ}) (κ̂ σ? #t))
                  (showP M M?) (showP Ξ Ξ?))))

     (define elms
       (with24
        (define app-cond
          (show (hc-append (blank 10) (chσ′*) @tt{ = } (σext @ctt{κ})) σ?))
        (append
         (list
          (ev-lhs x-var
                  @idtt{ρ} (show (chσ*) σ?) @ctt{κ} (showP M M?) (showP Ξ Ξ?))
          (if (abs-comp?) (blank 0) @tt{↦})
          (ntuple @idtt{v} @idtt{ρ′} (show (chσ*) σ?) @ctt{κ}
                  (showP M M?) (showP Ξ Ξ?)))
         (if (abs-comp?)
             '()
             (list
              ;; variable lookup
              (blank 0) (blank 0)
              var-condition))
          ;; application - eval function position
         (list
          (ev-lhs fn-app @idtt{ρ} (show (chσ*) σ?) @ctt{κ}
                  (showP M M?) (showP Ξ Ξ?))
          (if (abs-comp?) (blank 0) @tt{↦})
          (cond [(pushdown?) (app-rhs (chσ*))]
                [(abs-comp?) (app-rhs (chσ′*))]
                [else (hc-append (app-rhs (chσ′*)) app-cond)]))
         (if (abs-comp?) (list (blank 0) (blank 0) app-cond) '())
         (list         
          ;; application - eval argument position
          (rt-superimpose
           (ghost (call-pict #f))
           (ntuple (code v) @idtt{ρ} (show (chσ*) σ?)
                   (fcons (nstruct "ar" @idtt{e} @idtt{ρ}) (κ̂ σ? #f))
                   (showP M M?) (showP Ξ Ξ?)))
          @tt{↦}
          (ev-state (code e) @idtt{ρ} (show (chσ*) σ?)
                    (fcons (nstruct "fn" @idtt{v} @idtt{ρ}) (κ̂ σ? #f))
                    (showP M M?) (showP Ξ Ξ?))))))

     (define tyellow (make-object color% #xFF #xFF #x00 0.8))

     (define non-call (table 3 elms rcl cc-superimpose gap-size 30))
     (define legend
       (cond 
        [(= stage sfade)
         (let ([ρσ (ρσ σ? abs?)]) (fade-pict time ρσ (ghost ρσ)))]
        [(>= stage smove) (blank 0)]
        [else (show (ρσ σ? abs?) (not (= stage smove)))]))
     (define call-reduction
       (table 3 call-elms rcl cc-superimpose gap-size 30))
     (define call-where
       (tag-pict (table 3 where-clause-elms rcl cc-superimpose gap-size 30) 'where-clause))
     (define call-ret
       (table 3 rt-elms rcl cc-superimpose gap-size 30))

     (define rules
       (with24 (vl-append 30
                          (if (<= stage smove)
                              (if (= stage sfade)
                                  (fade-pict time non-call (ghost non-call))
                                  (show non-call (not (= stage smove))))
                              (blank 0))
                          (tag-pict call-reduction 'reduction)
                          call-where
                          (if M? call-ret (blank 0)))))
     (define a-fresh (hc-append a (with24 @t{ fresh})))
     (define a-alloc (hc-append a (with24 @tt{ = alloc(ς)})))

     (define side-condition
       (cond
        [(= stage sCESK) (lt-superimpose (ghost a-alloc) a-fresh)]
        [(<= saCESK stage sfade) (lt-superimpose (ghost a-fresh) a-alloc)]
        [else (lt-superimpose (ghost a-alloc) (ghost a-fresh))]))

     (define pict* (vc-append 0 (if (abs-comp?) (blank 0) legend) rules
                              (if (= stage sfade)
                                  (fade-pict time side-condition (ghost side-condition))
                                  side-condition)))

     (define pict
       (cond
        [lazy-problem?
         (define path (find-tag pict* 'var-condition))
         (unless path (error 'wat))
         (define frame (filled-flash-frame (ghost var-condition) #:bgcolor tyellow))
         (define-values (dx dy) (lt-find pict* path))
         (define-values (dx* dy*) (mk-center dx dy var-condition frame))
         (pin-under pict* dx* dy* frame)]
        [σ-problem?
         (pin-under-all
          (pin-under-all pict* 'σ (filled-flash-frame (ghost (chσ*)) #:bgcolor tyellow))
          'σ′ (filled-flash-frame (ghost (chσ′*)) #:bgcolor tyellow))]
        [e-problem?
         (pin-under-all
          (pin-under-all pict* 'eval0 (filled-flash-frame (ghost x-var) #:bgcolor tyellow))
          'eval1 (filled-flash-frame (ghost fn-app) #:bgcolor tyellow))]
        [else pict*]))

     ;; stupid pixel-pushing
     (define starty 341)
     (define endy -45)
     (define-values (rd-x rd-y) (lt-find pict (find-tag pict 'reduction)))
     (define-values (sc-x sc-y) (lt-find pict (find-tag pict 'where-clause)))
     (define diff (- sc-y rd-y))
     (define interpy (and time (+ (* (- 1.0 time) starty) (* time endy))))
     (define interpsc-y (and time (+ (* (- 1.0 time) (+ starty diff)) (* time (+ endy (* 2 diff))))))
     (if (= stage smove)
         (pin-over (pin-over (ghost pict)
                             sc-x interpsc-y
                             call-where)
                   0 interpy
                   call-reduction)
         pict)))
  (current-main-font old-main-font))

(module+ main
 (require (submod ".." slide-deck))
; (slide semantics)
; (slide the-shift)
; (slide (CESK-table sCEK))
; (slide (CESK-table sCESK))
; (slide (CESK-table saCESK))
 (slide a-state)
 (play (λ (t) (CESK-table sfade t)))
 (play (λ (t) (CESK-table smove t)))
 (slide (CESK-table smove 1.0))
#; (play (λ (t) (CESK-table smove t)))
 (slide (CESK-table sCESKM))
 (slide (CESK-table sCESKM-ret))
 (slide (CESK-table sCESKMΞ)))