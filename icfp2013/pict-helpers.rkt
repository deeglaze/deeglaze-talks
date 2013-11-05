#lang racket
(require slideshow/pict
         slideshow/code
         slideshow/flash
         slideshow/base
         unstable/gui/slideshow
         racket/draw
         (for-syntax syntax/parse)
         syntax/parse/define
         (except-in "color-scheme.rkt" addr))
(provide filled-rounded-rectangle-frame
         filled-flash-frame
         subscript-right
         paren-color
         supscript-right
         extend-map join-map
         expr iexpr
         plug call join-one ext-one subst
         extend-env extend-store join-store
         ntuple nstruct pin-under-all mk-center
         ⟼
         use-store use-env use-map
         production
         where-clause
         pin-over-tag pcolor
         pin-under-tag
         progressive-table)
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
                  [colorize-if (any/c pict? color/c . -> . pict?)]
                  [metacons (string? string? . -> . pict?)]
                  [metac (string? . -> . pict?)]
                  [addr (string? . -> . pict?)]
                  [store (string? . -> . pict?)]
                  [pin-over-center (pict? real? real? pict? . -> . pict?)]
                  [pin-over-vcenter (->* (pict? (or/c pict? real?) (or/c procedure? real?) pict?)
                                         [#:x-translate real?]
                                         pict?)]
                  [pin-over-hcenter (->* (pict? (or/c pict? real?) (or/c procedure? real?) pict?)
                                         [#:y-translate real?]
                                         pict?)]
                  [stack (string? . -> . pict?)]
                  [clo (string? . -> . pict?)]
                  [val (string? . -> . pict?)]
                  [env (string? . -> . pict?)]
                  [ibrack (string? . -> . pict?)]
                  [angled (string? . -> . pict?)]
                  [ghost-commas ((listof (or/c pict? #f)) . -> . (listof pict?))]
                  [both ((boolean? . -> . void?) . -> . void?)]
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
                  [thick-ellipse (->* (nonneg-real? nonneg-real?
                                       (real-in 0 255)
                                       color/c)
                                      (#:fill-color (or/c #f color/c))
                                      pict?)]
                  [thick-filled-rounded-rectangle
                   (->* (nonneg-real? nonneg-real?)
                        (real?
                         #:color color/c
                         #:style style/c
                         #:angle real?
                         #:border-width nonneg-real?
                         #:border-color (or/c #f color/c)
                         #:border-style style/c)
                        pict?)]
                  [annulus
                   (->i ([w nonneg-real?]
                         [h nonneg-real?]
                         [rw (w h) (and/c nonneg-real? (within-width-and-height w h))])
                        [#:color [color (or/c #f color/c)]
                         #:style [style style/c]
                         #:border-width [border-width nonneg-real?]
                         #:border-color [border-color (or/c #f color/c)]
                         #:border-style [border-style style/c]]
                        [result pict?])]
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

;; ellipse/border does the wrong thing.
(define (thick-ellipse ew eh thickness color #:fill-color [fill-color #f])
  (define-values (fill-color* style)
    (if fill-color
        (values fill-color 'solid)
        (values "white" 'transparent)))
  (dc (λ (dc dx dy)
         (with-save* dc ([get-brush set-brush (send the-brush-list find-or-create-brush fill-color* style)]
                         [get-pen set-pen color thickness 'solid])
           (send dc draw-ellipse dx dy ew eh)))
      ew eh))

(define (annulus w h rw
                 #:color [color #f]
                 #:style [style 'solid]
                 #:border-color [border-color #f]
                 #:border-width [border-width 1]
                 #:border-style [border-style 'solid])
  (dc (lambda (dc x y)
        (define p (new dc-path%))
        (define w2 (/ w 2))
        (define h2 (/ h 2))
        (define 2rw (* 2 rw))
        
        (send p move-to (- w rw) h2)
        (send p arc rw rw (- w 2rw) (- h 2rw) 0 (* 2 pi))
        (send p move-to w h2)
        (send p arc 0 0 w h 0 (* 2 pi))
        (send p translate x y)
        (send p close)
        
        (define brush (if color
                          (send the-brush-list find-or-create-brush color style)
                          (send the-brush-list find-or-create-brush "white" 'transparent)))
        (define pen (if border-color
                        (send the-pen-list find-or-create-pen border-color border-width border-style)
                        (send the-pen-list find-or-create-pen "black" 1 'transparent)))
        (with-save* dc ([get-brush set-brush brush]
                        [get-pen set-pen pen])
          (send dc draw-path p)))
      w h))

(define (mk-center x y base top)
  (values (+ x (/ (pict-width base) 2)
             (- (/ (pict-width top) 2)))
          (+ y (/ (pict-height base) 2) (- (/ (pict-height top) 2)))))

(define (pin-under-all base tag pict)
  (define pw (pict-width pict))
  (define ph (pict-height pict))
  (define last #f)
  (define paths (find-tag* base tag))
  (for/fold ([pict* base]) ([path (in-set (list->set paths))]
                            #:unless (and (pair? path) (is-ghost? (car path))))
    (define-values (dx dy) (lt-find pict* path))
    (define p (first path))
    (when (and last (equal? path last)) (error 'wat))
    (set! last path)
    (define-values (dx* dy*) (mk-center dx dy p pict))
    (pin-under pict* dx* dy* pict)))
;; Pin the center of pict at dx dy offset from base's top left corner.
(define (pin-over-center base dx dy pict)
  (pin-over base
            (- dx (/ (pict-width pict) 2))
            (- dy (/ (pict-height pict) 2))
            pict))

(define (pin-over-vcenter base dx dy pict #:x-translate [x-translate 0])
  (define-values (x y)
    (if (procedure? dy)
        (dy base dx)
        (values dx dy)))
  (pin-over base
            (+ x x-translate)
            (- y (/ (pict-height pict) 2))
            pict))

(define (pin-over-hcenter base dx dy pict #:y-translate [y-translate 0])
  (define-values (x y)
    (if (procedure? dy)
        (dy base dx)
        (values dx dy)))
  (pin-over base
            (- x (/ (pict-width pict) 2))
            (+ y y-translate)
            pict))

(define (both f) (f #f) (f #t))

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

(define (colorize-if b p c) (if b (colorize p c) p))

(define (thick-filled-rounded-rectangle w h [corner-radius -0.25]
                                        #:color [color "black"]
                                        #:style [style 'solid]
                                        #:angle [angle 0]
                                        #:border-width [border-width 1]
                                        #:border-color [border-color #f]
                                        #:border-style [border-style 'solid])
    (let ([dc-path (new dc-path%)])
      (send dc-path rounded-rectangle 0 0 w h corner-radius)
      (send dc-path rotate angle)
      (let-values ([(x y w h) (send dc-path get-bounding-box)])
        (dc (λ (dc dx dy) 
              (with-save* dc ([get-brush set-brush
                                         (send the-brush-list find-or-create-brush color style)]
                              [get-pen set-pen #:if border-color border-color border-width border-style])
                (send dc draw-path dc-path (- dx x) (- dy y))))
            w h))))

(define (filled-rounded-rectangle-frame pict
                                        #:color [color "white"]
                                        #:scale [scale 1]
                                        #:x-scale [x-scale 1]
                                        #:y-scale [y-scale 1]
                                        #:corner-radius [corner-radius -0.25]
                                        #:angle [angle 0]
                                        #:border-width [border-width 1]
                                        #:border-color [border-color "black"])
  (define dx (* x-scale scale (pict-width pict)))
  (define dy (* y-scale scale (pict-height pict)))
  (define rect
    (thick-filled-rounded-rectangle dx dy
                                    corner-radius
                                    #:color color
                                    #:angle angle
                                    #:border-width border-width
                                    #:border-color border-color))
  (cc-superimpose rect pict))

(define (filled-flash-frame pict
                            #:scale [scale 3/2]
                            #:color [color #f]
                            #:outline [outline #f]
                            #:n-points [n-points 10]
                            #:spike-fraction [spike-fraction 0.25]
                            #:rotation [rotation 0])
  (define flash
    (filled-flash (* scale (pict-width pict)) (* scale (pict-height pict))
                  n-points spike-fraction rotation))

  (cc-superimpose
   (if outline
       (colorize (outline-flash (* scale (pict-width pict)) (* scale (pict-height pict))
                                n-points spike-fraction rotation)
                 outline)
       (blank))
   (colorize-if color flash color)
   pict))

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

;; n stage : natural
;; stages: monotonically non-decreasing list of naturals
;; lst: list
(define (play-n-at n stage stages lst ghost?)
  (let loop ([stages stages] [lst lst])
    (cond [(empty? stages)
           (if ghost?
               (map ghost lst)
               '())]
          [(<= (first stages) stage)
           (define-values (to-show the-rest) (split-at lst n))
           (append to-show (loop (rest stages) the-rest))]
          [else (loop (rest stages) lst)])))
;; Given the current stage and the "rollout" stages, show the rows
;; up to the current stage.
;; there should be as many stages as there are rows.
(define (progressive-table stage stages ncols picts col-aligns row-aligns col-seps row-seps
                           #:ghost? [ghost? #t])
  (cond [(or (zero? ncols) (empty? stages) (empty? picts)) (blank)]
        [else
         (define rows (/ (length picts) (length stages)))
         (define prog-picts (play-n-at rows stage stages picts ghost?))
         (cond [(empty? prog-picts) (blank)]
               [else (table ncols prog-picts col-aligns row-aligns col-seps row-seps)])]))

(define (pin-at-tag pin base finder tag pict-fn)
  (define path (find-tag base tag))
  (define-values (x y) (finder base path))
  (pin-under base x y (pict-fn (last path))))

(define (pin-under-tag base finder tag pict-fn)
  (pin-at-tag pin-under base finder tag pict-fn))
(define (pin-over-tag base finder tag pict-fn)
  (pin-at-tag pin-over base finder tag pict-fn))

(define (encircle-tagged whole #:color [color grn] #:border-width [border-width 4] . tags)
  (define (encircle tagged)
    (thick-ellipse (pict-width tagged) (pict-height tagged)
                   border-width color))
  (let loop ([tags tags] [img whole])
    (cond [(empty? tags) img]
          [else (loop (rest tags)
                      (pin-over-tag img lt-find (first tags) encircle))])))