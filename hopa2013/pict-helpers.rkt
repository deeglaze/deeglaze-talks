#lang racket
(require slideshow/pict
         slideshow/code
         slideshow/flash
         slideshow/base
         unstable/gui/slideshow
         racket/draw
         slideshow-helpers/picts
         (except-in "color-scheme.rkt" addr))
(provide (all-from-out slideshow-helpers/picts)
         subscript-right
         paren-color
         supscript-right
         extend-map join-map
         expr iexpr
         plug call join-one ext-one subst
         extend-env extend-store join-store
         ntuple nstruct
         pin-under-all mk-center
         ⟼ 
         comma
         use-store use-env use-map
         production
         where-clause)
;; different constructors for coloring stuff
(provide/contract [memo (string? . -> . pict?)]
                  [edge (string? . -> . pict?)]
                  [metacons (string? string? . -> . pict?)]
                  [metac (string? . -> . pict?)]
                  [addr (string? . -> . pict?)]
                  [store (string? . -> . pict?)]
                  [stack (string? . -> . pict?)]
                  [clo (string? . -> . pict?)]
                  [val (string? . -> . pict?)]
                  [env (string? . -> . pict?)]
                  [ibrack (string? . -> . pict?)]
                  [angled (string? . -> . pict?)]
                  [ghost-commas ((listof (or/c pict? #f)) . -> . (listof pict?))]
                  [state (->* () #:rest (listof pict?) pict?)]
                  [angles (->* () #:rest (listof pict?) pict?)]
                  [parens (->* () #:rest (listof pict?) pict?)]
                  [tuple (->* () #:rest (listof pict?) pict?)]
                  [brackets (->* () #:rest (listof pict?) pict?)]
                  [braces (->* () #:rest (listof pict?) pict?)]
                  [ar (string? string? string? . -> . pict?)]
                  [fn (pict? string? . -> . pict?)]
                  [ar* (string? string? string? . -> . pict?)]
                  [fn* (pict? string? . -> . pict?)]
                  [icons (->* (string?) #:rest (listof string?) pict?)]
                  [closure (string? pict? . -> . pict?)]
                  [iclosure (string? string? . -> . pict?)]
                  [encircle-tagged (->* (pict?) (#:border-width real?
                                                 #:color color/c) 
                                        #:rest (listof symbol?) pict?)]
                  [grn color/c]
                  [constructor (->* (string?) #:rest (listof pict?) pict?)])


(define comma (tt ","))
(define (is-ghost? pict)
  (match (pict-draw pict)
    [`(picture ,w ,h) #t] [_ #f]))
(define (ghost-commas lst)
  (define-values (p dummy)
    (let ghost-commas ([lst (add-between lst comma)])
      (match lst
        ['() (values '() #t)]
        [(list p)
         (if p
             (values lst (is-ghost? p))
             (values '() #t))]
        [(list-rest pict comma rest)
         (define-values (ghosted all-ghosts?) (ghost-commas rest))
         (cond
          [(not pict)
           (values ghosted all-ghosts?)]
          [(is-ghost? pict)
           (values (list* pict (ghost comma) ghosted) all-ghosts?)]
          [else
           (values (list* pict (show comma (not all-ghosts?)) ghosted) #f)])])))
  p)
(define (opname-comma-separated wrap args)
  (match args
    [(cons (? string? s) rest) (hc-append (tt s) (apply wrap (ghost-commas rest) ))]
    [_ (apply angles (ghost-commas args))]))
(define (ntuple . args) (opname-comma-separated angles args))
(define (nstruct . args) (opname-comma-separated parens args))

(define (plug ctx hole) (hc-append 2 ctx (brackets hole)))
(define (call ctx hole) (hc-append 2 ctx (parens hole)))
(define (subst ctx id v) (hc-append 2 ctx (braces id (tt ":=") v)))
(define (ext-one m a v) (hc-append m (brackets a (tt " ↦ ") v)))
(define (join-one m a v) (hc-append m (tt "⊔") (brackets a (tt " ↦ ") v)))

(define grn (make-object color% #x10 #xBB #x10))

(define (surround d left right picts)
  (apply hc-append d left (append picts (list right))))

(define paren-color (make-parameter "black"))
(define (pcolor p) (colorize p (paren-color)))
(define (angled inner)
  (surround 0 (pcolor (t "〈")) (pcolor (t "〉"))
            (list (it inner))))
(define (ibrack inner)
  (surround 0 (pcolor (t "[")) (pcolor (t "]")) (list (it inner))))
(define (angles . picts)
  (surround 0 (pcolor (t "〈")) (pcolor (t "〉")) picts))
(define (brackets . picts)
  (surround 0 (pcolor (t "[")) (pcolor (t "]")) picts))
(define (braces . picts)
  (surround 0 (pcolor (t "{")) (pcolor (t "}")) picts))
(define (parens . picts)
  (surround 0 (pcolor (t "(")) (pcolor (t ")")) (list (apply hc-append 5.0 picts))))

(define (subscript-right t sub)
  (hc-append (it t) (subscript (it sub))))
(define (supscript-right pict sub)
  (hc-append pict (superscript (clo sub))))
(define (production left . rights)
  (apply hc-append 5.0 left (colorize (t "::=") "darkgray") (list-join rights (colorize (t "|") "darkgray"))))

(define (list-join lst sep)
  (let loop ([lst lst])
    (cond [(empty? lst) lst]
          [(empty? (rest lst)) lst]
          [else (list* (first lst) sep (loop (rest lst)))])))

(define (constructor name . args)
  (define colored (colorize (t name) "blue"))
  (cond [(empty? args) colored]
        [else (apply hc-append
                     colored 
                     (it "(")
                     (append (list-join args (it ","))
                             (list (it ")"))))]))

(define (icons name . args)
  (apply constructor name (map it args)))

(define (expr p) (colorize p (make-object color% #x00 #x66 #x00)))
(define (iexpr text) (expr (it text)))

(define (iclosure e ρ) (tuple (iexpr e) (env ρ)))
(define (closure e ρ) (tuple (iexpr e) ρ))

(define (extend-map m x v) (hc-append m (brackets x (it " ↦ ") v)))
(define (join-map m x v) (hc-append m (it "⊔") (brackets x (it " ↦ ") v)))
(define (use-map m . args) (hc-append m (apply tuple args)))
(define (extend-env ρ x v) (extend-map (env ρ) (iexpr x) v))
(define (use-env ρ x) (use-map (env ρ) (iexpr x)))
(define (extend-store σ a v) (extend-map (store σ) (addr a) v))
(define (join-store σ a v) (join-map (store σ) (addr a) v))
(define (use-store σ a) (use-map (store σ) (addr a)))

(define (env ρ) (it ρ))
(define (val v) (it v))
(define (store s) (it s))
(define (stack Ξ) (it Ξ))
(define (addr a) (colorize (it a) (make-object color% #x66 #x00 #xFF)))
(define (clo c) (it c))
(define (metac C) (it C))
(define (metacons κ C) (hc-append (icons κ) (it "·") (metac C)))
(define (memo M) (it M))
(define (edge L) (it L))

(define (⟼ lhs rhs)
  (para lhs (it "⟼") rhs))

(define (ar e ρ κ) (constructor "ar" (iexpr e) (env ρ) (icons κ)))
(define (fn v κ) (constructor "fn" v (icons κ)))
(define (ar* e ρ a) (constructor "ar" (iexpr e) (env ρ) (addr a)))
(define (fn* v a) (constructor "fn" v (addr a)))

(define (where-clause . picts)
  (colorize (apply hc-append (t "where ") picts)
            (make-object color% #x66 #x33 #x33)))

(define (state . args) (apply angled (list-join args (it ","))))
(define (tuple . args) (apply parens (list-join args (it ",")))) 

(define (pin-at-tag pin base finder tag pict-fn)
  (define path (find-tag base tag))
  (define-values (x y) (finder base path))
  (pin-under base x y (pict-fn (last path))))


(define (encircle-tagged whole #:color [color grn] #:border-width [border-width 4] . tags)
  (define (encircle tagged)
    (ellipse/border (pict-width tagged) (pict-height tagged)
                    #:border-width border-width #:border-color color))
  (let loop ([tags tags] [img whole])
    (cond [(empty? tags) img]
          [else (loop (rest tags)
                      (pin-over-tag img lt-find (first tags) encircle))])))