#lang at-exp slideshow
(require unstable/gui/slideshow
         unstable/gui/ppict
         unstable/gui/pict
         racket/gui/base
         slideshow/code
         "pict-helpers.rkt")

(module+ slide-deck
  (provide call/ret)
  (define (call/ret stage pushdown?)
    ;; These have to be on the same line for formatting reasons
    (define tagged-define
      (code (define #,(tag-pict (code (id #,(tag-pict (code x) 'bind))) 'id) #,(tag-pict (code x) 'body))))
    (define tagged-le
      (code (<= #,(tag-pict (code (id #,(tag-pict (code 0) 'zero))) 'call0) #,(tag-pict (code (id #,(tag-pict (code 1) 'one))) 'call1))))
    (define staged-store
      (shadow-frame
       (progressive-table stage (list 1 3 7) 2
                          (list (code x) (cond
                                          [(< stage 5) (braces (code 0))]
                                          [(>= stage 5) (braces (code 0) @tt{, } (code 1))]
                                          [else (blank 0)])
                                (code (id 0)) (cond [(or pushdown? (< stage 7)) (braces (code 0))]
                                                    [(>= stage 7) (braces (code 0) @tt{, } (code 1))]
                                                    [else (blank 0)])
                                (code (id 1)) (braces (code 0) @tt{, } (code 1)))
                          lc-superimpose cc-superimpose gap-size 5)))
    
    (define focus
      (vector 'call0
              'id
              'body
              'call0
              'call1
              'id
              'body
              'call1
              'call1))
    
    (define prog (vl-append 100 tagged-define tagged-le))
    
    (define-values (zerot bindt bodyt call0t call1t)
      (values (find-tag prog 'zero) (find-tag prog 'bind) (find-tag prog 'body)
              (find-tag prog 'call0) (find-tag prog 'call1)))
    
    (define cfg-arrows
      (list (list zerot ct-find bindt cb-find (if pushdown?
                                                  '((#:color . "blue"))
                                                  '()))
            (list bindt ct-find bodyt ct-find `((#:start-pull . 0.3)
                                                (#:end-pull . 0.3)
                                                (#:start-angle . ,(* 1/4 pi))
                                                (#:end-angle . ,(* -1/4 pi))
                                                ,@(if pushdown?
                                                      '((#:color . "blue"))
                                                      '())))
            (list bodyt cb-find call0t ct-find (if pushdown?
                                                   '((#:color . "blue"))
                                                   (if (>= stage 7)
                                                       `((#:color . "red")
                                                         (#:line-width . 2.5))
                                                       '())))
            (list call0t cb-find call1t cb-find `((#:start-pull . 0.3)
                                                  (#:end-pull . 0.3)
                                                  (#:start-angle . ,(* -1/4 pi))
                                                  (#:end-angle . ,(* 1/4 pi))))
            (list call1t ct-find bindt cb-find (if pushdown?
                                                   `((#:color . "red"))
                                                   '()))
            (list bindt ct-find bodyt ct-find
                  (if pushdown?
                      `((#:start-pull . 0.6)
                        (#:end-pull . 0.6)
                        (#:start-angle . ,(* 1/3 pi))
                        (#:end-angle . ,(* -1/3 pi))
                        (#:color . "red"))
                      `((#:start-pull . 0.3)
                        (#:end-pull . 0.3)
                        (#:start-angle . ,(* 1/4 pi))
                        (#:end-angle . ,(* -1/4 pi)))))
            (list (find-tag prog 'body) cb-find (find-tag prog 'call1) ct-find
                  (if pushdown?
                      '((#:color . "red"))
                      '()))))
    
    (define (arrow-directive p path0 find0 path1 find1 alst)
      (pin-arrow-line 10 p path0 find0 path1 find1
                           #:start-pull (dict-ref alst '#:start-pull 1/4)
                           #:end-pull (dict-ref alst '#:end-pull 1/4)
                           #:start-angle (dict-ref alst '#:start-angle #f)
                           #:end-angle (dict-ref alst '#:end-angle #f)
                           #:color (dict-ref alst '#:color #f)
                           #:line-width 3))
    
    (define arrowed
      (for/fold ([p prog]) ([directive (in-list cfg-arrows)]
                            [i stage])
        (match directive
          [`(,path0 ,find0 ,path1 ,find1)
           (apply arrow-directive p (append directive (list '())))]
          [`(,path0 ,find0 ,path1 ,find1 ,alst)
           (apply arrow-directive p directive)])))
    
    (define focused (pin-under-tag arrowed lt-find (vector-ref focus stage)
                                   (λ (p) (filled-rounded-rectangle-frame
                                           p
                                           #:bgcolor "yellow"))))

    (slide #:title (if pushdown? "Pushdown" "Regular")
           (vl-append 100
                      (pin-over focused
                                350 -140 (vl-append @t{  Store:}
                                                    staged-store))
                      (show (if pushdown?
                                @t{Result: true}
                                @t{Result: true or false}) (> stage 7))))))

(module+ main
  (require (submod ".." slide-deck))
  (for ([i 9]) (call/ret i #f))
  (for ([i 9]) (call/ret i #t)))