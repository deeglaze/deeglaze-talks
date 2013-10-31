#lang at-exp slideshow

(require unstable/gui/slideshow
         unstable/gui/ppict
         net/sendurl
         slideshow/code
         slideshow/flash
         racket/gui/base
         scheme/runtime-path
         slideshow/balloon slideshow/face
         "pict-helpers.rkt"
         (submod "hammer-slide.rkt" slide-deck)
         (submod "proof-diagram.rkt" slide-deck)
         (submod "call-ret.rkt" slide-deck))

(define (naive arrows?)  
  (slide #:title "Naive doesn't cut it"
         (if arrows?
        (define kappa "\u03ba")
(define abscont (abstt kappa))

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

(define store-pict
  (big (join-one (tag-pict (idtt "σ") 'first)
                 @idtt{a}
                 (braces
                  (nstruct "rt" (idtt "e") (idtt "ρ") (tag-pict (idtt "σ′") 'second))))))

(define alloc-pict
  (big (join-one (idtt "σ") @idtt{a}
                 (braces
                  (nstruct "rt" (idtt "e") (idtt "ρ") (tag-pict (idtt "a") 'fix))))))

(define (naive->fix stage)
  (define base-pict
    (vc-append (vl-append 100
                          store-pict
                          (show (vl-append
                                 alloc-pict
                                 (big (join-one @idtt{Ξ} @idtt{a} (braces (idtt "σ′")))))
                                (> stage 0)))
               (show (parameterize ([current-font-size 80]) (abstt kappa)) (> stage 1))))
  (slide #:title (hc-append (t "Approximation tuning: ") (tt "alloc"))
         (if (> stage 0)
             (pin-arrow-line 30 base-pict
                             (find-tag base-pict 'second) cb-find
                             (find-tag base-pict 'fix) ct-find
                             #:line-width 10)
             base-pict)))     (arrows-back-and-forth store-pict (find-tag store-pict 'first) (find-tag store-pict 'second))
             store-pict)))



(define sqcup "\u2294")
(define (fixup stage)
  (slide #:title "Fix up memoization and returns"
         (hc-append @t{Approximate contexts mean all instantiations from } @idtt{Ξ})
         (show
          (vl-append 50
                     (hc-append (angles @idtt{rt} (parens @idtt{e} comma @idtt{ρ} comma @idtt{a}) comma
                                           @idtt{C} comma
                                           @idtt{v})
                                (show (hc-append @tt{ ↦ }
                                                 (angles @idtt{κ} comma @idtt{C} comma @idtt{v}))
                                      (> stage 1)))
                     (show (hc-append @t{where }
                                      @idtt{κ} @tt{ ∈ ⋃}
                                      (braces @idtt{Ξ}
                                                  (parens @idtt{e} comma
                                                             @idtt{ρ} comma
                                                             @idtt{σ}) @tt{ : }
                                                                       @idtt{σ} @tt{ ∈ } @idtt{Ξ} (parens @idtt{a})))
                           (> stage 1))
                     (show (hc-append
                            @idtt{M′} @tt{ = }
                            @idtt{M} @tt{ ⊔ }
                            (vc-append 1 (big (tt sqcup)) ;; bigsqcup
                                       (small (hc-append @idtt{σ} @tt{ ∈ } @idtt{Ξ} (parens @idtt{a}))))
                            (brackets (parens @idtt{e} comma @idtt{ρ} comma @idtt{σ})
                                         @tt{ ↦ }
                                         (braces @idtt{v})))
                           (> stage 2)))
          (> stage 0))))

(slide (big (t "We can extend the analogy"))
       'next
       (t "Everyone's favorite: delimited composable control"))

(slide (big (hc-append @idtt{E} (brackets (parens @idtt{reset } @idtt{F} (brackets (code (shift k e)))))))
       @tt{ ↦ }
       (big (hc-append @idtt{E} (brackets @idtt{e} (braces @idtt{k} @tt{:=} (code (λ (x) #,(parens @idtt{reset } (hc-append 2 (idtt "F") (brackets @idtt{x})))))))))
       'next
       @t{(F doesn't contain a reset)})

(define ttcirc (tt "\u2218"))
(define (meta-cont stage)
  (define base
    (vc-append
     (big (angles (code (reset e)) comma @idtt{ρ} comma @idtt{σ} comma @idtt{κ} comma @idtt{C}))
         @tt{ ↦ }
         (big (angles @idtt{e} comma @idtt{ρ} comma @idtt{σ} comma @tt{end} comma (hc-append 3 @idtt{κ} ttcirc  @idtt{C})))
         (show (vc-append
                (big (angles (code (shift k e)) comma @idtt{ρ} comma @idtt{σ} comma @idtt{κ} comma @idtt{C}))
                @tt{ ↦ }
                (big (angles @idtt{e} comma
                                (ext-one @idtt{ρ} @idtt{k} @idtt{a}) comma
                                (join-one @idtt{σ} @idtt{a} (braces (tag-pict @idtt{κ} 'problem)))
                                @tt{end} comma @idtt{C})))
               (> stage 0))))
  (slide #:title "Metacontinuations"
         (if (> stage 1)
             (pin-over base (find-tag base 'problem) rb-find (colorize (arrow 40 (* 5/8 pi)) "red"))
             base)))
(for ([i 3]) (meta-cont i))

(both naive)

;; Gwarg at the old. Highlight and say, "eh.. not so much."
;;(both cfa2-1st)

(slide #:title "The AAM way"
       (with-size 44 @t{Break circularity with indirection}))

(for ([i 3]) (naive->fix i))

(define alt (tt " | "))
(define sharp "\u266f")
(slide #:title "Metacontinuations"
       (hc-append 5 @idtt{C} @tt{ ::= } @tt{halt} alt (hc-append 3 @idtt{κ} ttcirc  @idtt{C}))
       'next
       (arrow 30 (* 3/2 pi))
       (hc-append 5 @idtt{C} @tt{ ::= } @tt{halt} alt (idtt sharp) (parens @idtt{ctx}))
       'next
       (hc-append 5 @idtt{ctx} @tt{ ::= }
                  (angles @idtt{e} comma @idtt{ρ} comma @idtt{σ}) alt
                  (angles abscont comma @idtt{v} comma @idtt{σ})))

(slide #:title "The new context"
       @item{Prompts treated just like function calls}
       'next
       (hc-append @tt{call} (angles abscont comma @idtt{v} comma @idtt{σ} comma @idtt{κ} comma @idtt{C}))
       (arrow 30 (* 3/2 pi))
       (hc-append (angles abscont comma (hc-append 2 (idtt sharp) (parens @idtt{ctx})) comma @idtt{v} comma @idtt{σ}))
       (ht-append @t{with } (vl-append 
                             (hc-append @idtt{ctx} @tt{ = } (parens abscont comma @idtt{v} comma @idtt{σ}))
                             (hc-append @tt{Ξ′ = Ξ⊔} (brackets @idtt{ctx} @tt{↦} (braces (angles @idtt{κ} comma @idtt{C})))))))

(for ([i 4]) (fixup i))
