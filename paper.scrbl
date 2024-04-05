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

@abstract{
  Abstract goes here. A citation @~cite[b:racket].
}

@section{Introduction}

@section{Challenges}

* Interaction between web-server continuations and parameterizations.
When the web-server continues a request, the request is launched in
a new thread so the paramz of the thread where the continuation was
captured are lost.

* Interaction between parameterizations and continuations. Extending
a parameterization can cause parameters to be kept alive that you
wouldn't expect from the lexical structure of the code when combining
delimited contiunations with parameterizations.

https://github.com/racket/racket/issues/4216

* Using composable continuations may lead to ballooning of memory
use when stack continuations that contain non-tail-recursive
functions.

For all 3, talk about how we debugged the issues and tools needed to
make debugging easier. Would be nice to have a proper debugger that
can walk the ``stack''.

@section{Positives}

* Natural to write flows without needing to worry too much about storing
intermediate state anywhere (except for ``resuming''). When talking
about this, we'll have to reference the web server paper. Developing
the ``framework'' to support the flows only involves regular Racket
code, so there's no need to worry about migrating a database or any
other external system when making changes to how the internal state is
structured.

@(generate-bibliography #:sec-title "References")
