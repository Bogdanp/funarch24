#lang racket/base

(require scriblib/autobib)

(provide
 (all-defined-out))

(define-cite ~cite
  citet generate-bibliography
  #:cite-author cite-author
  #:cite-year cite-year
  #:style number-style)

(define b:dbg
  (make-bib
   #:title "dbg"
   #:author (authors "Bogdan Popa")
   #:date "2021"
   #:url "https://github.com/Bogdanp/racket-dbg"))

(define b:racket
  (make-bib
   #:title "Reference: Racket"
   #:author (authors "Matthew Flatt" "PLT")
   #:date "2010"
   #:location (techrpt-location
               #:institution "PLT Design Inc."
               #:number "PLT-TR-2010-1")
   #:url "https://racket-lang.org/tr1/"))

(define b:delimited-composable-control
  (make-bib
   #:title "Adding delimited and composable control to a production programming environment"
   #:author (authors
             "Matthew Flatt"
             "Gang Yu"
             "Robert Bruce Findler"
             "Matthias Felleisen")
   #:location (journal-location
               "ACM SIGPLAN Notices"
               #:volume 42
               #:pages '(165 176))
   #:url "https://dl.acm.org/doi/10.1145/1291220.1291178"
   #:doi "10.1145/1291220.1291178"
   #:date "2007"))

(define b:queinnec
  (make-bib
   #:title "Inverting back the inversion of control or, continuations versus page-centric programming"
   #:author (authors "Christian Queinnec")
   #:location (journal-location
               "ACM SIGPLAN Notices"
               #:volume 38
               #:pages '(57 64))
   #:url "https://dl.acm.org/doi/10.1145/772970.772977"
   #:doi "10.1145/772970.772977"
   #:date "2003"))

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

(define b:seaside
  (make-bib
   #:title "Seaside: A flexible environment for building dynamic web applications"
   #:author (authors
             "Stéphane Ducasse"
             "Adrian Lienhard"
             "Lukas Renggli")
   #:location (journal-location
               "IEEE software"
               #:volume 24
               #:pages '(56 63))
   #:url "https://ieeexplore.ieee.org/abstract/document/4302687/"
   #:date "2007"))

(define b:oTree
  (make-bib
   #:title "oTree—An open-source platform for laboratory, online, and field experiments"
   #:author (authors
             "Daniel L. Chen"
             "Martin Schonger"
             "Chris Wickens")
   #:location (journal-location
               "Journal of Behavioral and Experimental Finance"
               #:volume 9
               #:pages '(88 97))
   #:url  "https://www.sciencedirect.com/science/article/pii/S2214635016000101"
   #:doi "10.1016/j.jbef.2015.12.001"
   #:date "2016"))

(define b:marks
  (make-bib
   #:title "Modeling an Algebraic Stepper"
   #:author (authors
             "John Clements"
             "Matthew Flatt"
             "Matthias Felleisen")
   #:location (journal-location
               "Lecture Notes in Computer Science"
               #:volume 2028
               #:pages '(320 334))
   #:doi "10.1007/3-540-45309-1_21"
   #:date "2001"))
