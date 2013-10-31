#lang racket
(require redex "util.rkt")

(define primops '(+ - add1 sub1 zero? * /))
(define-language Ξ
  [pre-e x (e e ...) (shift x e) (reset e) lam pre-v (if e e e) (let ([x e] ...) e)]
  [e (ℓ pre-e)]
  [pre-v primops number boolean]
  [lam (λ-sym (x ...) e)]
  [λ-sym λ lambda]
  [primops + - add1 sub1 zero? * /]
  [ℓ natural]
  [(x y z) variable-not-otherwise-mentioned])

(define-extended-language CESKMΞ Ξ
  [v pre-v (clo (x ...) e ρ) (comp κ̂) number]
  [vs (side-condition (name vs any) (set? (term vs)))]
  [f (ev ℓ (e ...) (vs ...) ρ)
     (ifk e e ρ)
     (letk x (x ...) (e ...) (x ...) (vs ...) e ρ)]
  [κ (f κ) (rt ctx) mt]
  [κ̂ (f κ̂) (rt sctx) mt]
  [C mt (♯ ctx)]
  [ctx (e ρ σ-name) (κ vs σ-name) sctx]
  [sctx (approx e ρ a)]
  [(σ σ-name ρ M Ξ a σs) any]
  [δσ ((a vs) ...) same]
  [δM ((ctx vs) ...) same]
  [δΞ (ctx κ C) (a σs) same]
  [pre-ς (eval e ρ κ C)
         (continue κ C vs)
         (apply v (vs ...) κ C)]
  [ς (in pre-ς σ σ-name M Ξ)
     (out-kind pre-ς σ M Ξ δσ δM δΞ)]
  [out-kind out out/ctx])

(define-metafunction CESKMΞ
  [(inject e) (in (eval e #hash() mt mt) #hash() #hash() #hash() #hash())])
(define-metafunction CESKMΞ
  [(one any) ,(set (term any))])

(define-metafunction CESKMΞ
  [(extend* ρ (x ..._1) (a ..._1))
   ,(for/fold ([ρ* (term ρ)]) ([y (in-list (term (x ...)))]
                               [b (in-list (term (a ...)))])
      (hash-set ρ* y b))])
(define-metafunction CESKMΞ
  [(extend ρ x a) (extend* ρ (x) (a))])

(define-metafunction CESKMΞ
  [(approximate κ Ξ a) ((replaceσ κ a) (addσ κ Ξ a))])

(define-metafunction CESKMΞ
  [(replaceσ mt a) mt]
  [(replaceσ (rt (e ρ any)) a) (rt (approx e ρ a))]
  [(replaceσ (rt (approx e ρ any)) a) (rt (approx e ρ a))]
  [(replaceσ (f κ) a) (f (replaceσ κ a))])

(define-metafunction CESKMΞ
  [(addσ mt Ξ a) same]
  [(addσ (f κ) Ξ a) (addσ κ Ξ a)]
  [(addσ (rt (e ρ σ-name)) Ξ a) (a (one σ-name))]
  [(addσ (rt (e ρ a_prime)) Ξ a) (a ,(hash-ref (term Ξ) (term a_prime)))])

(define-metafunction CESKMΞ
  [(returns Ξ (e ρ σ-name)) ,(set->list (hash-ref (term Ξ) (term (e ρ σ-name))))]
  [(returns Ξ (approx e ρ a)) ,(set->list (for/union ([σ-name (in-set (hash-ref (term Ξ) (term a)))])
                                  (hash-ref (term Ξ) (term (e ρ ,σ-name)))))])

(define-metafunction CESKMΞ
  [(memoize Ξ (e ρ σ-name) vs) (((e ρ σ-name) vs))]
  [(memoize Ξ (approx e ρ a) vs)
   ,(for/list ([σ-name (in-set (hash-ref (term Ξ) (term a)))])
      (term ((e ρ ,σ-name) vs)))])

(define (same->null x) (if (eq? 'same x) '() x))
(define (join-list h lst)
  (for/fold ([h* h]) ([a×b (in-list (same->null lst))])
    (hash-join h* (first a×b) (second a×b))))
(define (applyΔΞ Ξ δΞ)
  (match δΞ
    [`(,ctx ,κ ,C) (hash-join Ξ ctx (set (list κ C)))]
    [`(,a ,σs) (hash-join Ξ a σs)]
    ['same Ξ]))

(define-metafunction CESKMΞ
  [(applyΔ σ δσ M δM Ξ δΞ)
   (σ_1 σ_1 ,(join-list (term M) (term δM))
        ,(applyΔΞ (term Ξ) (term δΞ)))
   (where σ_1 ,(join-list (term σ) (term δσ)))])

(define (safe-apply fn args)
  (with-handlers ([exn:fail? (λ (e) (set))])
    (set (apply fn (map set-first args)))))

(define-metafunction CESKMΞ
  [(lookup σ ρ x) ,(hash-ref (term σ) (hash-ref (term ρ) (term x)) (set))])

;; Concrete δ-axioms expect singleton sets
(define-metafunction CESKMΞ
  [(δ-axioms + vs ...) ,(safe-apply + (term (vs ...)))]
  [(δ-axioms - vs ...) ,(safe-apply - (term (vs ...)))]
  [(δ-axioms * vs ...) ,(safe-apply * (term (vs ...)))]
  [(δ-axioms / vs ...) ,(safe-apply / (term (vs ...)))]
  [(δ-axioms zero? vs) ,(safe-apply zero? (term (vs)))]
  [(δ-axioms add1 vs) ,(safe-apply add1 (term (vs)))]
  [(δ-axioms sub1 vs) ,(safe-apply sub1 (term (vs)))])
(define-metafunction CESKMΞ
  [(alloc (x ...) ς) ,(gensym)])

(define (number^? x) (or (eq? x 'number) (number? x)))
(define (only-if-non-empty f b lst)
  (match lst
    ['() b]
    [(cons vs vss) (if (set-empty? (set-filter f vs))
                       (set)
                       (only-if-non-empty f b vss))]))
#;#;
(define-metafunction CESKMΞ
  [(δ-axioms (side-condition (name p primops) (memq (term p) '(+ - * / add1 sub1)))
             vs ...)
   ,(only-if-non-empty number^? (set 'number) (term (vs ...)))]
  [(δ-axioms zero? vs) ,(cond [(equal? (term vs) (set 0)) (set #t)]
                              [else
                               (define not-constant-numbers
                                 (set-filter (compose not number?) (term vs)))
                               (if (set-member? not-constant-numbers 'number)
                                   (set #t #f)
                                   (set #f))])])
(define-metafunction CESKMΞ
  [(alloc (x ...) ς) (x ...)])

(define R
  (reduction-relation CESKMΞ
    [--> (in (eval (ℓ x) ρ κ C) σ σ-name M Ξ)
         (out (continue κ C (lookup σ ρ x)) σ M Ξ same same same)]
    [~~> (eval (ℓ (e_0 e ...)) ρ κ C)
         (eval e_0 ρ ((ev ℓ (e ...) () ρ) κ) C)]
    [~~> (eval (ℓ (if e_g e_t e_e)) ρ κ C)
         (eval e_g ρ ((ifk e_t e_e) κ) C)]
    [~~> (eval (ℓ (let () e_body)) ρ κ C)
         (eval e_body ρ κ C)]
    [~~> (eval (ℓ (let ([y e_0] [x e] ...) e_body)) ρ κ C)
         (eval e_0 ρ ((letk y (x ...) (e ...) () () e_body ρ) κ) C)]
    [--> (in (eval (ℓ (reset e)) ρ κ C) σ σ-name M Ξ)
         (out (eval e ρ mt (♯ ctx)) σ M Ξ same same (ctx κ C))
         (where ctx (e ρ σ-name))
         (side-condition/hidden (not (hash-has-key? (term M) (term ctx))))]
    ;; Memoized reset
    [--> (in (eval (ℓ (reset e)) ρ κ C) σ σ-name M Ξ)
         (out (continue κ C vs) σ M Ξ same same (ctx κ C))
         (where ctx (e ρ σ-name))
         (where vs ,(hash-ref (term M) (term ctx) #f))
         (side-condition/hidden (term vs))]
    [--> (name ς (in (eval (ℓ (shift x e)) ρ κ C) σ σ-name M Ξ))
         (out (eval e ρ_1 mt C) σ M Ξ ((a (one (comp κ̂)))) same δΞ)
         (where a (alloc (x) ς))
         (where ρ_1 (extend ρ x a))
         (where (κ̂ δΞ) (approximate κ Ξ a))]
    ;; Value forms
    [~~> (eval (ℓ (λ-sym (x ...) e)) ρ κ C)
         (continue κ C (one (clo (x ...) e ρ)))]
    [~~> (eval (ℓ pre-v) ρ κ C)
         (continue κ C (one pre-v))]
    ;; Continues
    [~~> (continue ((ifk e_t e_e ρ) κ) C vs)
         (eval e_t ρ κ C)
         ;; Non-false is a truish value.
         (side-condition (not (equal? (term vs) (set #f))))]
    [~~> (continue ((ifk e_t e_e ρ) κ) C vs)
         (eval e_e ρ κ C)
         (side-condition (set-member? (term vs) #f))]
    [~~> (continue ((ev ℓ (e_0 e ...) (vs_done ...) ρ) κ) C vs)
         (eval e_0 ρ ((ev ℓ (e ...) (vs_done ... vs) ρ) κ) C)]
    [~~> (continue ((letk y (z x ..._1) (e_0 e ..._1) (x_done ..._2) (vs_done ..._2) e_body ρ) κ) C vs)
         (eval e_0 ρ ((letk z (x ...) (e ...) (x_done ... y) (vs_done ... vs) e_body ρ) κ) C)]
    ;; Thunk application
    [~~> (continue ((ev ℓ () () ρ) κ) C vs)
         (apply v () κ C)
         (where (v_any0 ... v v_any1 ...) ,(set->list (term vs)))]
    [~~> (continue ((ev ℓ () (vs_fn vs_done ...) ρ) κ) C vs)
         (apply v (vs_done ... vs) κ C)
         (where (v_any0 ... v v_any1 ...) ,(set->list (term vs_fn)))]
    ;; Binding let
    [--> (name ς (in (continue ((letk y () () (x ...) (vs_done ...) e_body ρ) κ) C vs) σ σ-name M Ξ))
         (out (eval e_body ρ_1 κ C) σ M Ξ ((a vs_done) ... (a_last vs)) same same)
         (where (a ... a_last) (alloc (x ... y) ς))
         (where ρ_1 (extend* (x ... y) (a ... a_last)))]
    ;; Function application
    ;; Funky out state because in wide semantics the store might update afterwards,
    ;; meaning the name we used in the context was wrong and thus we must
    ;; rewrite the state to use an updated context if it turns out the name changed.
    [--> (name ς (in (apply (clo (x ..._1) e ρ) (vs ..._1) κ C) σ σ-name M Ξ))
         (out/ctx (eval e ρ_1 (rt ctx) C) σ M Ξ ((a vs) ...) same (ctx κ C))
         (where (a ...) (alloc (x ...) ς))
         (where ρ_1 (extend* ρ (x ...) (a ...)))
         (where ctx (e ρ_1 σ-name))
         (side-condition/hidden (not (hash-has-key? (term M) (term ctx))))]
    ;; Memoized function application
    [--> (name ς (in (apply (clo (x ..._1) e ρ) (vs ..._1) κ C) σ σ-name M Ξ))
         (out/ctx (continue κ C vs_memo) σ M Ξ same same (ctx κ C))
         (where (a ...) (alloc (x ...) ς))
         (where ρ_1 (extend* ρ (x ...) (a ...)))
         (where ctx (e ρ_1 σ-name))
         (where vs_memo ,(hash-ref (term M) (term ctx) #f))
         (side-condition/hidden (term vs_memo))]
    ;; Continuation application
    [--> (in (apply (comp κ̂) (vs) κ C) σ σ-name M Ξ)
         (out (continue κ̂ (♯ ctx) vs) σ M Ξ same same (ctx κ C))
         (where ctx (κ̂ vs σ-name))
         (side-condition/hidden (not (hash-has-key? (term M) (term ctx))))]
    ;; Memoized continuation application
    [--> (in (apply (comp κ̂) (vs) κ C) σ σ-name M Ξ)
         (out (continue κ C vs_memo) σ M Ξ same same (ctx κ C))
         (where ctx (κ̂ vs σ-name))
         (where vs_memo ,(hash-ref (term M) (term ctx) #f))
         (side-condition/hidden (term vs_memo))]
    ;; Memoize function call
    [--> (in (continue (rt ctx) C vs) σ σ-name M Ξ)
         (out (continue κ C vs) σ M Ξ same (memoize Ξ ctx vs) same)
         (where (any_0 ... κ any_1 ...) (returns Ξ ctx))]
    ;; Memoize prompt
    [--> (in (continue mt (♯ ctx) vs) σ σ-name M Ξ)
         (out (continue κ C vs) σ M Ξ same ((ctx vs)) same)
         (where (any_0 ... (κ C) any_1 ...) ,(set->list (hash-ref (term Ξ) (term ctx))))]
    [~~> (apply primops (vs ...) κ C)
         (continue κ C vs_result)
         (where vs_result (δ-axioms primops vs ...))
         (side-condition (not (set-empty? (term vs_result))))]
    ;; Apply changes as a standalone step.
    [--> (out pre-ς σ_in M_in Ξ_in δσ δM δΞ)
         (in pre-ς σ σ-name M Ξ)
         (where (σ σ-name M Ξ) (applyΔ σ_in δσ M_in δM Ξ_in δΞ))]
    with
    [(--> (in pre-ς_0 σ σ-name M Ξ) (out pre-ς_1 σ M Ξ same same same))
     (~~> pre-ς_0 pre-ς_1)]))

;; Widened semantics
(define (step-all F σ σ-name S M Ξ)
  (define-values (F* σ* δ? σ-name* S* M* Ξ* S/ctx M/ctx Ξ/ctx)
    (for/fold ([F* '()] [σ* σ] [δ? #f] [σ-name* σ-name]
               [S* S] [M* M] [Ξ* Ξ]
               [S/ctx (hash)] [M/ctx (hash)] [Ξ/ctx (hash)])
        ([ς̂ (in-list F)])
      (define stepped (apply-reduction-relation R (list 'in ς̂ σ σ-name* M* Ξ*)))
      (for/fold ([F* F*] [σ* σ*] [δ? δ?] [σ-name* σ-name*]
                 [S* S*] [M* M*] [Ξ* Ξ*]
                 [S/ctx S/ctx] [M/ctx M/ctx] [Ξ/ctx Ξ/ctx])
          ([ς (in-list stepped)])
        (define (witness-step ς̂* δσ δM δΞ on-new-unchanged)
          (define-values (σ** δ?*) (applyΔσ/δ? σ* δσ))
          (define M** (join-list M* δM))
          (define Ξ** (applyΔΞ Ξ* δΞ))
          (cond
           [δ? ;; already saw a change?
            (values (cons ς̂* F*)
                    σ** #t σ-name*
                    (hash-set S* ς̂* σ-name*)
                    M** Ξ** S/ctx M/ctx Ξ/ctx)]
           [δ?* ;; Just witnessed a change? Gotta change contexts :(
            (define σ-name* (add1 σ-name))
            (values (cons ς̂* F*)
                    σ** #t σ-name*
                    (hash-set S* ς̂* σ-name*)
                    M** Ξ**
                    (rewriteS S/ctx σ-name*)
                    (rewrite-ctx-table M/ctx σ-name*)
                    (rewrite-ctx-table Ξ σ-name*))]
           [(= (hash-ref S* ς̂* -1) σ-name)
            (values F* σ* δ? σ-name* S* M** Ξ** S/ctx M/ctx Ξ/ctx)]
           [else
            (on-new-unchanged M** Ξ**)]))
        (match ς
          [`(out ,ς̂* ,_ ,_ ,_ ,δσ ,δM ,δΞ)
           (witness-step
            ς̂* δσ δM δΞ
            (λ (M** Ξ**)
               (values (cons ς̂* F*)
                       σ* #f σ-name* (hash-set S* ς̂* σ-name*)
                       M** Ξ** S/ctx M/ctx Ξ/ctx)))]
          [`(out/ctx ,ς̂* ,_ ,_ ,_ ,δσ ,δM ,δΞ)
           (witness-step
            ς̂* δσ δM δΞ
            (λ (M** Ξ**)
               (values (cons ς̂* F*)
                       σ* #f σ-name* S* M* Ξ*
                       (hash-set S/ctx ς̂* σ-name*)
                       (join-list M/ctx δM)
                       (applyΔΞ Ξ/ctx δΞ))))]
          [_ (error 'witness-step "Bad state ~a" ς)]))))
  (values F* σ* σ-name*
          (hash-extend S* S/ctx)
          (hashes-join M* M/ctx)
          (hashes-join Ξ* Ξ/ctx)))
(define (iter e)
  (define ς̂₀ `(eval ,e #hash() mt mt))
  (let loop ([F (list ς̂₀)] [σ #hash()] [σ-name 0] [S (hash ς̂₀ 0)] [M #hash()] [Ξ #hash()])
    (cond
     [(null? F) (list S M Ξ σ)]
     [else
      (define-values (F* σ* σ-name* S* M* Ξ*) (step-all F σ σ-name S M Ξ))
      (loop F* σ* σ-name* S* M* Ξ*)])))

(define (rewriteS S/ctx σ-name)
  (for/hash ([(ς̂ old) (in-hash S/ctx)]
             #:unless (eq? (first ς̂) 'continue))
    (match ς̂
      ;; false memo lookup. No way we have an entry at the new context.
      [`(eval ,e ,ρ_1 (rt ,_) ,C)
       (values `(,e ,ρ_1 (rt (,e ,ρ_1 ,σ-name)) ,C) σ-name)]
      [_ (error 'rewriteS "Bad state ~a" ς̂)])))

(define (rewrite-ctx-table h/ctx σ-name)
  (for/hash ([(ctx vs) (in-hash h/ctx)])
    (match ctx
      [`(,e ,ρ ,_) (values `(,e ,ρ ,σ-name) vs)] ;; or κ vs. Matches all the same.
      [other (values other vs)])))

(define (join/δ? σ a vs)
  (define old (hash-ref σ a (set)))
  (define new (set-union old vs))
  (if (= (set-count old) (set-count new))
      (values σ #f)
      (values (hash-set σ a new) #t)))
(define (applyΔσ/δ? σ δσ)
  (for/fold ([σ* σ] [δ? #f]) ([a×b (in-list (same->null δσ))])
    (define-values (σ** δ?*) (join/δ? σ* (first a×b) (second a×b)))
    (values σ** (or δ? δ?*))))

;; No one wants to give example programs with labelling. Go add labels.
(define (add-labels e)
  (define ℓ 0) (define (ℓ!) (begin0 ℓ (set! ℓ (add1 ℓ))))
  (let loop ([e e])
    (match e
      [(or (? (λ (v) (memq v primops))) (? number?) (? boolean?) (? symbol?))
       (list (ℓ!) e)]
      [`(shift ,x ,e) (list (ℓ!) `(shift ,x ,(loop e)))]
      [`(if ,eg ,et ,ee) (list (ℓ!) `(if ,(loop eg) ,(loop et) ,(loop ee)))]
      [`(reset ,e) (list (ℓ!) `(reset ,(loop e)))]
      [`(,(or 'λ 'lambda) (,x ...) ,e)
       (list (ℓ!) `(λ ,x ,(loop e)))]
      [`(let ([,x ,e] ...) ,ebody)
       (list (ℓ!) `(let ,(for/list ([y (in-list x)]
                                    [e0 (in-list e)])
                           (list y (loop e0)))
                     ,(loop ebody)))]
      [`(,e0 ,es ...) (list (ℓ!) (map loop e))]
      [_ (error 'add-labels "Bad expr ~a" e)])))

(define example
  (add-labels `(* 2 (reset (+ 1 (shift k (k 5)))))))

(apply-reduction-relation* R (term (inject ,example)))
(iter example)