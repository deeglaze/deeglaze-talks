#lang at-exp slideshow

(require unstable/gui/slideshow
         racket/gui/base
         scheme/runtime-path
         (rename-in "pict-helpers.rkt"))

(module+ slide-deck
  (provide history)
  (define-runtime-path cfa2-path "cfa2fig-small.png")
  (define (history stage)
    (define (cite next?)
      (define-syntax-rule (wrap e)
        (if next? (big (colorize e "red")) (small e)))
      (hbl-append (small (t "[Sharir & Pnueli ")) (wrap (t "1981")) (small (t "]"))))
    (slide (cc-superimpose
            (vl-append
             100
             (hb-append
              20 (big @t{That was first-order})
              (hbl-append (lbl-superimpose (show (cite #f) (<= stage 1)) (show (cite #t) (> stage 1)))))
             (show (hbl-append
                    20 (big (hc-append @t{We } @it{can } @t{do higher-order}))
                    (small (t "[Vardoulakis & Shivers 2010]")))
                   (> stage 0)))
            (show (bitmap cfa2-path) (> stage 2))))))

(module+ main
  (require (submod ".." slide-deck))
  (for ([i 4]) (history i)))