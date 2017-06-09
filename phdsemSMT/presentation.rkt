#lang slideshow

(require slideshow/code
         file/convertible
         racket/draw)

(define (load-img file-name kind)
  (let* ([in (open-input-file file-name)]
         [bmp (make-object bitmap% in kind #f)]
         [dummy (close-input-port in)])
    bmp))  

(define eugene (load-img "Goldberg.gif" 'gif))
(define circuit (load-img "adder2.gif" 'gif))
(define plane (load-img "boeing787-9dreamliner-13.jpg" 'jpeg))
(define gene (load-img "gene.png" 'png))
(define graph (load-img "graphmsgs.png" 'png)) ;; 648 x 798 /2 = 324 x 399
(define turing (load-img "turing.jpg" 'jpeg))
(define QF (load-img "QF.png" 'png))
(define incremental (load-img "steve-jobs-one-more-thing.jpg" 'jpeg))
(define socrates (load-img "socrates.png" 'png))

(slide
 #:title "SMT Solvers: Logic and Cookies"
 (t "Presented by: Dionna Glaze"))

(slide
 (para (bt "S") "atisfiability: a ∧ b ⇒ c")
 'next
 (para (bt "M") "odulo: with a, b and c interpreted in")
 'next
 (para (bt "T") "heories: [a ↦ x < 0, b ↦ y < x, c ↦ y > 0]"))

;; Introduce the motivation
(slide
 (scale (bitmap circuit) 3/8)
 'next
 (scale (bitmap plane) 1/2)
 'next
 (scale (bitmap gene) 3/4))

;; Describe the tech history
;; for lack of a better staging technique...
(slide (para "SAT is NP-complete")
       (para ""))
(slide (para "SAT is NP-complete (shocker)")
       (para ""))
(slide
 (para "SAT is NP-complete (shocker)")
 (para "Woe is not us"))

;; for lack of a better staging technique...
(slide
 (para "SAT solvers are really fast*")
 (hc-append (scale (bitmap eugene) 1/2) (blank 344 399)) ;; 1/2 + 20x0
 (para ""))
(slide
 (para "SAT solvers are really fast*")
 (hc-append (scale (bitmap eugene) 1/2)
            (blank 20 0)
            (scale (bitmap graph) 1/2))
 (para ""))
(slide
 (para "SAT solvers are really fast*")
 (hc-append (scale (bitmap eugene) 1/2)
            (blank 20 0)
            (scale (bitmap graph) 1/2))
 (para "So let's use them"))
 

;; Describe the use of tech
(slide
 (para "SMT solvers are used as many things.")
 'next
 (para "Let's talk about their use as math geniuses."))

(slide
 (hc-append (para "Theories should be decidable.") (scale (bitmap turing) 3/4))
 'next
 (hc-append (para "This tends to mean \"quanifier free.\"") (blank 10 0) (scale (bitmap QF) 1/2))
 'next
 (hc-append (para "Decision procedures should work incrementally.") (blank 10 0) (scale (bitmap incremental) 5/8))
 'next
 (hc-append (para "Decision procedures should provide explanations for their deductions.") (blank 10 0) (scale (bitmap socrates) 1)))

(slide
 (para "SMT solvers exist for undecidable theories")
 'next
 (para "They are necessarily incomplete")
 'next
 (para "These solvers involve a different workflow."))

(slide
 (para "SMT solvers are currently a hot research topic")
 'next
 (para "Researchers are looking for new theories")
 'next
 (para "I use SMT to enhance Typed Racket")
 'next
 (t "Thank you"))
