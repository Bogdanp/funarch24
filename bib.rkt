#lang racket/base

(require scriblib/autobib)

(provide
 (all-defined-out))

(define-cite ~cite
  citet generate-bibliography
  #:cite-author cite-author
  #:cite-year cite-year
  #:style number-style)

(define b:racket
  (make-bib
   #:title "Reference: Racket"
   #:author (authors "Matthew Flatt" "PLT")
   #:date "2010"
   #:location (techrpt-location
               #:institution "PLT Design Inc."
               #:number "PLT-TR-2010-1")
   #:url "https://racket-lang.org/tr1/"))
