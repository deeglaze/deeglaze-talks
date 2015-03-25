#lang racket/base
(provide wtable)
(require (except-in pict/private/pict drop)
         (only-in pict clip)
         (only-in slideshow current-gap-size make-slide-inset)
         foracc
         racket/list
         slideshow-helpers/slide)

;; Windowed table module
;; A table pict where after every k rows, slide up, showing last row before staging more.
;; To show this adequately, we create a staged-slide

(define (imp-list->vector imp n)
  (define out (make-vector n))
  (let loop ([l imp] [p 0])
    (unless (= n p)
      (vector-set! out
                   p
                   (if (pair? l) (car l) l))
      (loop (if (pair? l) (cdr l) l) (add1 p))))
  out)

;; Precondition: stage is not an animation stage.
;; Example:
;; For keep = 1, threshold = 4
;; 0-3 fill threshold
;; 4 animates to show only row 3
;; 5 shows 3-4
;; 6 shows 3-5
;; 7 shows 3-6
;; 8 animates to show only 6.
;; 9,10,11 show 6-9
;; 12 animates to show only 9.
;;-----------------------------
;; keep = 2, threshold = 4
;; 0-3 fill threshold
;; 4 animates to show 2,3
;; 5 shows 2-4
;; 6 shows 2-5
;; 7 animates to show 4,5
;; 8 shows 4-6
;; 9 shows 4-7
;; 10 animates to show 6,7

;; slower version that is more obviously correct
(define (stage->row-start/num-integrate keep threshold stage)
  (let int ([start 0] [num-rows 1] [s 0])
    (cond [(>= s stage) (values start num-rows)]
          [(= num-rows threshold)
           ;; just about to animate. The animation slide shows the kept rows
           ;; and the following slide adds a row.
           (int (+ start (- threshold keep)) keep (add1 s))]
          [else (int start (add1 num-rows) (add1 s))])))

;; We don't want a transition at the end if there are no more rows to show afterwards.
(define (num-transitions keep threshold rows)
  (if (<= rows threshold)
      0
      (ceiling (/ (- rows threshold) (- threshold keep)))))

(define (animation-stage? keep threshold stage)
  (and (>= stage threshold)
       (zero? (remainder (- stage threshold) (add1 (- threshold keep))))))


;; Precondition: (zero? (remainder (length picts) ncols))
;; Precondition: keep < threshold
;; Number rows = (quotient (length picts) ncols)
;; Number transitions = 1 + ((rows - threshold) / (threshold - keep))
;; Number stages = rows + transitions
(define (wtable ncols picts col-aligns row-aligns col-seps row-seps
                ;; Since we have to build a whole slide, allow
                ;; the user to do post-processing on the picts we create.
                #:post [post (λ (stage->pict/proc stage->anim? stages)
                                stage->pict/proc)]
                #:title [title #f]
                #:num-keep [keep 1] ;; how many rows do we keep after hitting the threshold?
                #:threshold [threshold 2]
                #:steps [steps 10]
                #:dilate [dilate values] ;; manipulate animation
                #:delay [delay 0.05])
  (define count (length picts))
  (unless (zero? (remainder count ncols))
    (error 'wtable "cell count isn't divisble by the provided column count"))
  (unless (< keep threshold)
    (error 'wtable "Can't keep >= the threshold of rows"))
  (define nrows (/ count ncols))
  (define ntransitions (num-transitions keep threshold nrows))
  (define cells
    (for/acc ([#:type vector]
              [picts #:drop picts])
             ([r (in-range nrows)])
             (define-values (row rest) (split-at picts ncols))
             (values (list->vector row) rest)))
  (define ralign (imp-list->vector row-aligns nrows))
  (define calign (imp-list->vector col-aligns ncols))
  (define rsep (imp-list->vector row-seps nrows))
  (define csep (imp-list->vector col-seps ncols))
  (define (get-cell c r) (vector-ref (vector-ref cells r) c))
  (define (rowmap f) (build-list nrows f))
  (define (colmap f) (build-list ncols f))
  (define superimposed-rows
    (list->vector
     (rowmap (lambda (r)
               (apply (vector-ref ralign r)
                      (colmap (lambda (c) (get-cell c r))))))))
  (define superimposed-cols
    (list->vector
     (colmap (lambda (c)
               (apply
                (vector-ref calign c)
                (rowmap (lambda (r) (get-cell c r))))))))

  (define ((render-cell r) c)
    (define cell (get-cell c r))
    (define sc (vector-ref superimposed-cols c))
    (define sr (vector-ref superimposed-rows r))
    (define w (pict-width sc))
    (define h (pict-height sr))
    (ht-append
     0
     (let-values ([(x __) (find-lb sc cell)]
                  [(_  y) (find-lb sr cell)])
       (picture
        w h
        `((place ,x ,y ,cell))))
     (blank (vector-ref csep c) 0)))

  (define (render-row r)
    (vl-append
     0
     (apply ht-append 0 (colmap (render-cell r)))
     (blank 0 (vector-ref rsep r))))
  (define rendered-rows (build-list nrows render-row))
  (define max-size (panorama (ghost (apply cc-superimpose rendered-rows))))
  (define vrows (for/vector ([r (in-list rendered-rows)])
                           (cc-superimpose max-size r)))

  ;; Don't pad the last column
  (vector-set! csep (sub1 ncols) 0)
  ;; NOTE: we do want to pad the last row so that the staging is smooth.

  (apply vl-append 0 (rowmap render-row))

  ;; We want to not bounce the image, so we ghost up to the threshold.
  (define (render-rows start num [gn num])
    (apply vl-append 0
           (build-list num (λ (i) ((if (>= i gn) ghost values) (vector-ref vrows (+ i start)))))))

  (define (anim? stage) (animation-stage? keep threshold stage))
  (define (stage->pict stage)
    ;; We render up to threshold-many rows.
    ;; We start at ?? and end at ??
    ;; On stage ??, we animate the previous stage's rows to slide upward to keep
    ;; the last `keep` rows, applying `dilate` to the translation logic.
    ;; The last stage's rows form the absolute bounding box in which to clip the animation.
    (if (anim? stage)
        (let*-values
            ([(start num-rows)
              (stage->row-start/num-integrate keep threshold (sub1 stage))]
             [(last) (stage->pict (sub1 stage))]
             [(bb) (ghost last)]
             [(cut) (render-rows start (- num-rows keep))]
             [(h) (- (pict-height cut))])
          ;; render last, and scroll up (pict-height cut) many pixels
          (λ (n)
             (define n* (dilate n))
             (clip (pin-over bb 0 (* n* h) last))))
        (let*-values ([(start num-rows)
                       (stage->row-start/num-integrate keep threshold stage)]
                      [(num ghostn)
                       (if (< stage threshold)
                           (values threshold num-rows)
                           (values num-rows num-rows))])
          (render-rows start num ghostn))))

  (define stages (+ nrows ntransitions))
  (staged-slide (post stage->pict anim? stages)
                stages
                ;; no title or name
                (for/hash ([n ntransitions])
                  (define which
                    (if (= n 0)
                        threshold
                        (+ threshold (* n (add1 (- threshold keep))))))
                  ;; which stage is the animation for transition?
                  (values which
                          (anim-info #t #t steps delay #f 'auto #f)))
                (hash) ;; no options
                (slide-options title
                               #f
                               'auto
                               (current-gap-size)
                               (make-slide-inset 0 0 0 0)
                               #f
                               #f)
                (hash) ;; no names
                #f ;; no ctx
                (hash)))
