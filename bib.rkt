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

(define b:web-server
  (make-bib
   #:title "Implementation and use of the PLT scheme Web server"
   #:author (authors
             "Shriram Krishnamurthi"
             "Peter Walton Hopkins"
             "Jay McCarthy"
             "Paul T. Graunke"
             "Greg Pettyjohn"
             "Matthias Felleisen")
   #:location (journal-location
               "Higher-Order and Symbolic Computation"
               #:pages '(431 460)
               #:volume 20)
   #:date "2007"
   #:url "https://doi.org/10.1007/s10990-007-9008-y"
   #:doi "10.1007/s10990-007-9008-y"))
