#lang at-exp slideshow

(require unstable/gui/slideshow
         slideshow/code
         scheme/runtime-path
         file/convertible
         racket/draw)

(define bg-slide-assembler
  (lambda (title sep content)
    (inset content (- margin) (- margin) 0 0)))  

(define-runtime-path logo-path "../utils/prl-logo.png")

(parameterize ([current-slide-assembler bg-slide-assembler])
  (slide
   (cc-superimpose
    (bitmap logo-path)
    (vc-append
     (big (bold (para #:align 'center "What I (am trying to) do")))
     (blank-line)
     (para #:align 'center (bt "J. Ian Johnson") ", David Van Horn and Olin Shivers")))))

(define (start stage)
  (define items0
    (vc-append @item{Conceptually}
               @item{Theoretically (if there's theory)}
               @item{Experimentally}))
  (define items1
    (vc-append @item{Simple formulation}
               @item{Slooooow or unscalable}
               @item{For impoverished languages}))
  (slide #:title "Static Analysis"       
       @para{It's too hard}
       (cond [(= stage 0) (ghost items0)]
             [else items0])
       @para{Or it's too easy}
       (cond [(<= stage 1) (ghost items1)]
             [else items1])))
(start 0)
(start 1) ;; one list
(start 2) ;; both lists

(slide #:title "Hard ≠ Bad"
       @para{The elite can do it}
       'next
       @para{But nothing is perfect}
       'next
       @para{Enter: me})

(slide #:title "Our Secret Weapon"
       @para{In a word: semantics}
       'next
       @para{Good ideas are expressed simply}
       'next
       @para{We model algorithmic ideas concretely}
       'next
       @para{Standard techniques ⇒ algorithm recovered})

(slide #:title "Concretely"
       @para{PDCFA/CFA2: Memoization + Continuation tables}
       @para{   new: escape analysis + better allocation}
       'next
       @para{Airac: Perfect hindsight + fast-forwarding}
       @para{    extend to PDA model, lessen space footprint})

(slide #:title "Lessons I've learned"
       @para{Graphs are just a projection of reality}
       @para{"Tricks" should have concrete semantics}
       @para{We have @it{X} already, so let's use it!})
