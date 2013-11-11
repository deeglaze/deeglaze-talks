#lang racket
(require slideshow/pict
         slideshow/code
         slideshow/flash
         slideshow/base
         slideshow-helpers/picts
         unstable/gui/slideshow
         racket/draw
         (for-syntax syntax/parse)
         syntax/parse/define
         (except-in "color-scheme.rkt" addr))
(provide subscript-right
         paren-color
         supscript-right
         extend-map join-map
         expr iexpr
         plug call join-one ext-one subst
         extend-env extend-store join-store
         ntuple nstruct
         ⟼
         use-store use-env use-map
         production
         where-clause
         pcolor)
(define (nonneg-real? x) (and (real? x) (>= x 0)))
(define style/c
  (one-of/c 'transparent 'solid 'xor 'hilite
            'dot 'long-dash 'short-dash 'dot-dash
            'xor-dot 'xor-long-dash 'xor-short-dash
            'xor-dot-dash))
(define (within-width-and-height w h)
  (make-contract #:name (format "within width and height ~a ~a" w h)
                 #:first-order
                 (λ (rw)
                    (define 2v (* 2 rw))
                    (and (positive? (- w 2v))
                         (positive? (- h 2v))))))
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

;; brush/pen not parameters, unfortunately.
;; Imperative save-restore to the "rescue."
(begin-for-syntax
 (define-syntax-class (gsr dc-stx)
   #:attributes (g s do)
   (pattern [g:id s:id (~optional (~seq #:if guard:expr)) r:expr ...]
            #:with do (if (attribute guard)
                          #`(unless guard (send #,dc-stx s r ...))
                          #`(send #,dc-stx s r ...)))))
(define-simple-macro (with-save dc (~var p (gsr #'dc)) body ...)
  (let* ([dcv dc]
         [v (send dcv p.g)])
    p.do
    body ...
    (send dcv p.s v)))
(define-syntax (with-save* stx)
  (syntax-parse stx
    [(_ dc () body ...) (syntax/loc stx (let () body ...))]
    [(_ dc (~and (give gives ...)
                 ((~var p (gsr #'dcv)) (~var ps (gsr #'dcv)) ...))
         body ...)
     (syntax/loc stx (let ([dcv dc])
                       (with-save dcv give
                                  (with-save* dcv (gives ...) body ...))))]))

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
(define (pcolor p) (colorize* p (paren-color)))
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
  (apply hc-append 5.0 left (colorize (t "::=") "darkgray") (add-between rights (colorize (t "|") "darkgray"))))

(define (constructor name . args)
  (define colored (colorize* (t name) "blue"))
  (cond [(empty? args) colored]
        [else (apply hc-append
                     colored
                     (it "(")
                     (append (add-between args (it ","))
                             (list (it ")"))))]))

(define (icons name . args)
  (apply constructor name (map it args)))

(define (expr p) (colorize* p (make-object color% #x00 #x66 #x00)))
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
(define (addr a) (colorize* (it a) (make-object color% #x66 #x00 #xFF)))
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
  (colorize* (apply hc-append (t "where ") picts)
             (make-object color% #x66 #x33 #x33)))

(define (state . args) (apply angled (add-between args (it ","))))
(define (tuple . args) (apply parens (add-between args (it ","))))

(define (encircle-tagged whole #:color [color grn] #:border-width [border-width 4] . tags)
  (define (encircle tagged)
    (thick-ellipse (pict-width tagged) (pict-height tagged)
                   border-width color))
  (let loop ([tags tags] [img whole])
    (cond [(empty? tags) img]
          [else (loop (rest tags)
                      (pin-over-tag img lt-find (first tags) encircle))])))