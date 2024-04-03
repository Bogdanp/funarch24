#lang scribble/acmart @sigplan @screen

@(require "bib.rkt")

@title{Unnamed Continuations Paper}

@author[
  "Marc Kaufmann"
  #:email (email "kaufmannm@ceu.edu")
  #:affiliation (affiliation
                 #:institution @institution{Central European University}
                 #:city "Vienna"
                 #:country "Austria")
]

@author[
  "Bogdan Popa"
  #:email (email "bogdan@defn.io")
  #:affiliation (affiliation
                 #:institution @institution{Independent}
                 #:city "Cluj-Napoca"
                 #:country "Romania")
]

@keywords{Continuations}

@abstract{Abstract goes here. A citation @~cite[b:racket].}

@section{Introduction}

@(generate-bibliography #:sec-title "References")
