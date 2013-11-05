#lang setup/infotab
(define name "ianj-talks")
(define version "1.0")
(define deps '("base" "slideshow-lib" "slideshow-helpers"))
(define setup-collects (list "boyer-moore"
                             "hopa2013"
                             "icfp2013"
                             "IBMplday"
                             "milawa"
                             "nasa-priorities"
                             "phdsemCFA"
                             "phdsemSMT"
                             "smt-pl"
                             "proposal"
                             "utils"))
(define collection 'multi)