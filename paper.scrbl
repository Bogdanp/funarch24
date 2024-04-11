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

@keywords{Continuations, Web, Racket/Scheme, State management}

@abstract{
  Surveys and experiments in economics involve stateful interactions: participants receive different messages based on earlier answers, choices, and performance, or trade across many rounds with other participants. In the design of congame, a platform for running such economic studies, we decided to use composable (?) continuations to manage the common (Marc: want to highlight that it is not how all state is managed, but default) flow of participants through a study. Here we report on the positives of this approach, as well as some challenges of using continuations, such as persisting and releasing values/bindings [at the right time/across requests], avoiding memory leaks, and the difficulty of debugging continuations. [Lessons? Discussion about patterns?]}

@section{Introduction}

A citation @~cite[b:racket]. Testing citations: @~cite[b:delimited-composable-control] and @~cite[b:seaside].

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
about this, we'll have to reference the web server paper @~cite[b:web-server]. Developing
the ``framework'' to support the flows only involves regular Racket
code, so there's no need to worry about migrating a database or any
other external system when making changes to how the internal state is
structured.

* List some/all the variables that would have to be tracked and the various
  levels/layers at which they would have to be tracked.

* Is there any aspect of the composition of studies that is easier because we
  use continuations, or would it have been equally easy with other ways of flow
  control? My guess is that it doesn't.

@(generate-bibliography #:sec-title "References")
