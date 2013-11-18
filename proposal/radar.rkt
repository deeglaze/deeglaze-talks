#lang racket
(require plot plot/utils pict slideshow slideshow-helpers/picts)
(provide radar-plot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bleh
(define vector-range
  (case-lambda [(end) (vector-range 0 end 1)]
               [(start end) (vector-range start end 1)]
               [(start end step) (for/vector #:length (ceiling (/ (- end start 1) step))
                                             ([i (in-range start end step)]) i)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (polar-line θ0 r0 θ1 r1
                    #:color [color 3]
                    #:style [style 'solid])
  (lines (list (polar->cartesian θ0 r0)
               (polar->cartesian θ1 r1))
         #:color color #:style style))

(define (polar-line-interval θ0 r00 r01 θ1 r10 r11
                              #:color [color 3]
                              #:line1-color [line1-color 3]
                              #:line1-style [line1-style 'solid]
                              #:line2-color [line2-color 3]
                              #:line2-style [line2-style 'solid])
   (lines-interval (list (polar->cartesian θ0 r00)
                         (polar->cartesian θ1 r10))
                   (list (polar->cartesian θ0 r01)
                         (polar->cartesian θ1 r11))
          #:color color
          #:line1-color line1-color
          #:line1-style line1-style
          #:line2-color line2-color
          #:line2-style line2-style))

(define (radar-plot vs/s #:dimensions [dimensions #f] #:fill-colors [colors #f] #:border-colors [bcolors #f]
 #:plot-scale [plot-scale 1])
  (cond [(vector? vs/s) ;; colors and bcolors should be (or/c #f color/c)
         (radar-plot* (list vs/s)
                      (or dimensions (for/list ([coord (in-vector vs/s)]) (car coord)))
                      (if colors (list colors) (list 1))
                      (if bcolors (list bcolors) (list 1))
                      plot-scale)]
        [else
         (define (default-colors) (range 1 (add1 (length vs/s))))
         (unless dimensions (error 'radar-plot "Multiple radar plots need an explicit list of dimensions"))
         (define colors* (or colors (default-colors)))
         (define bcolors* (or bcolors colors colors*))
         (radar-plot* vs/s dimensions colors* bcolors* plot-scale)]))

;; List_m[Vector_n[Pair[Any,Real]]] List List_m[Color] List_m[Color]
;; Precondition: n > 1

(define (radar-axes number extent)
  (define δ (/ (* 2 pi) number))
  (let build ([i 0])
    (cond [(= i number) '()]
          [else
           (cons (polar-line (* δ i) 0 (* δ i) extent
                             #:color "gray")
                 (build (add1 i)))])))
                   
(define (radar-plot* vss dimensions fill-colors border-colors plot-scale)
  (define num-dimensions (length dimensions))
  (define δ (/ (* 2 pi) num-dimensions))
  (define dimension↦angle
    (for/hash ([dim (in-list dimensions)]
               [i (in-naturals)])
      (values dim (* i δ))))
  (define (dim-lookup dim)
    (hash-ref dimension↦angle dim
              (λ () (error 'radar-plot* "Expected dimension in given dimensions ~a" dim))))

  (define (plot-tick value)
    (for/list ([i num-dimensions])
      (polar-line (* i δ) value (* (modulo (add1 i) num-dimensions) δ) value #:color "gray")))
  (define (subplot vs fill-color border-color)
    (define (slice θ0 r0 θ1 r1)
      (polar-line-interval θ0 0 r0 θ1 0 r1
                           #:color fill-color
                           #:line2-color border-color
                           #:line1-style 'transparent))

    (match-define (cons dim0 r0) (vector-ref vs 0))
    (define θ0 (dim-lookup dim0))
    (let build ([i 1]
                [last-θ θ0]
                [last-r r0])
      (cond
       [(= i (vector-length vs))
        ;; close the interval by going back to the first point.
        (list (slice last-θ last-r θ0 r0))]
       [else
        (match-define (cons dim current-r) (vector-ref vs i))
        (define current-θ (dim-lookup dim))
        (cons (slice last-θ last-r current-θ current-r)
              (build (add1 i) current-θ current-r))])))

  (define max-radius (for*/fold ([r -inf.0]) ([vs (in-list vss)]
                                              [coord (in-vector vs)])
                       (max r (cdr coord))))
  (define-values (minx maxx miny maxy)
    (for*/fold ([minx +inf.0] [maxx -inf.0] [miny +inf.0] [maxy -inf.0])
      ([vs (in-list vss)]
       [coord (in-vector vs)])
      (match (polar->cartesian (dim-lookup (car coord)) (cdr coord))
        [(vector x y) (values (min x minx) (max x maxx) (min y miny) (max y maxy))])))
  ;; Calculate best square fit
  (define mx
    (for*/fold ([mx -inf.0]) ([vs (in-list vss)]
                              [coord (in-vector vs)])
      (match-define (cons dim r) coord)
      (define θ (dim-lookup dim))
      (match-define (vector x y) (polar->cartesian θ (* r plot-scale)))
      (max mx (abs x) (abs y))))
  
  (define subplots (map subplot vss fill-colors border-colors))
  (define dimstrings (for/list ([dim (in-list dimensions)]) (format "~a" dim)))
  (parameterize ([plot-r-ticks no-ticks]
                 [plot-x-axis?  #f]
                 [plot-x-far-axis?  #f]
                 [plot-y-axis?  #f]
                 [plot-y-far-axis?  #f])
    (plot (append
           (radar-axes num-dimensions max-radius)
           ;; synthetic ticks
           (for/list ([v (in-range 1 (add1 max-radius))]) (plot-tick v))
           subplots)
          #:x-min (- mx) #:x-max mx #:y-min (- mx) #:y-max mx
          ;#:x-min minx #:x-max maxx #:y-min miny #:y-max maxy
          #:x-label #f
          #:y-label #f)))

(module+ test
 (let ([plt
        (radar-plot (vector '(a . 4) '(b . 2) '(c . 3) '(d . 2.5) '(e . 1) '(f . 5))
                    #:plot-scale 1.25)])
   (pin-over
    (bitmap (send plt get-bitmap))
    50 0 (t "Dimension 1"))))
