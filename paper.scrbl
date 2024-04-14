#lang scribble/acmart @sigplan @screen

@(require scribble/core
          scriblib/figure
          (only-in scribble/manual racket racketblock racketblock0)
          "bib.rkt")

@title{Unnamed Continuations Paper}
@subtitle{Experience Report}

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
  Surveys and experiments in economics involve stateful interactions:
  participants receive different messages based on earlier answers,
  choices, and performance, or trade across many rounds with other
  participants. In the design of congame, a platform for running such
  economic studies, we decided to use delimited continuations to
  manage the common (Marc: want to highlight that it is not how all
  state is managed, but default) flow of participants through a study.
  Here we report on the positives of this approach, as well as some
  challenges of using continuations, such as persisting and releasing
  values/bindings [at the right time/across requests], avoiding memory
  leaks, and the difficulty of debugging continuations. [Lessons?
  Discussion about patterns?]
}

@section[#:tag "intro"]{Introduction}

Continuations in a web context allow applications to be programmed in a
direct style @~cite[b:queinnec b:web-server]. In Congame, we have opted
to take advantage of this style of programming to aid our implementation
of a framework for declaratively specifying composable surveys in a way
that elides most of the details of day-to-day web programming from the
study creator.

In @secref{minimal} we show a minimal implementation of a system
similar to Congame and demonstrate how natural it is to program web
applications in this style. Then, in @secref{challenges} we talk about
some of the challenges involved in scaling such a system to include
more features and illustrate some of the debugging challenges. Finally,
in @secref{positives}, we note some of the positive experiences we've
had working on this system, and explore similarities to related work in
@secref{related}.

@section[#:tag "minimal"]{Mini Congame}

The core of Congame is a @emph{study}, represented as a tree of
@emph{steps} and other, nested, studies. Each @emph{step} in a study
is a procedure that generates a web page used to display and possibly
retrieve information to and from the participant being surveyed.
@Figure-ref{minimal-1} implements a minimal harness for constructing
and running these types of studies. A study creator uses the structures
defined in @figure-ref{minimal-1} alongside @emph{widgets} such as the
one defined in @figure-ref{minimal-2} to put together a study. The
study can then be run from within a Racket web server servlet with
@racket[run-study].

@figure-here[
  "minimal-1"
  "A mini Congame implementation."
  @racketblock0[
  (define current-embed (make-parameter #f))
  (struct step (handler))
  (struct study (steps))
  (define (run-study the-study)
    (let loop ([steps (study-steps the-study)])
      (if (null? steps)
          '(continue)
          (match (begin0 (run-step (car steps))
                   (redirect/get/forget))
            ['(retry) (loop steps)]
            ['(continue) (loop (cdr steps))]))))
  (define (run-step the-step)
    (match the-step
      [(step (? study? substudy))
       (run-study substudy)]
      [(step handler)
       (send/suspend/dispatch
        (lambda (embed)
          (parameterize ([current-embed embed])
            (response/xexpr (handler)))))]))]]

When a study is run, its steps are executed sequentially, and when a
step uses a widget, the widget reifies the current continuation of the
step and stores it in a hash table that maps URLs to continuations.
The URL of that continuation is then linked in the resulting HTML.
Once a continuation URL is visited, @racket[run-step] returns and the
study loop can continue to the next step. Once a continuation URL is
visited, the continuation is removed from the hash table to prevent
the participant from pressing the ``Back'' button in their browser and
redoing previous steps.

@figure-here[
  "minimal-2"
  @elem{A button ``widget''.}
  @racketblock0[
  (define (button label [action void])
    `(a
      ([href ,((current-embed)
               (lambda (_req)
                 (action)
                 '(continue)))])
      ,label))]]

@Figure-ref{example} shows a basic study implemented using this
framework.

@figure-here[
  "example"
  @elem{An example study.}
  @racketblock0[
  (define (hello)
    `(div
      (p "Welcome to the study.")
      ,(button "Continue")))
  (define (done)
    `(p "Thank you for participating."))
  (define example-study
    (study
     (list
      (step hello)
      (step done))))]]

@section[#:tag "challenges"]{Challenges}

@section[#:tag "positives"]{Positives} @; Needs better title

@section[#:tag "related"]{Related Work}

@; * Interaction between web-server continuations and parameterizations.
@; When the web-server continues a request, the request is launched in
@; a new thread so the paramz of the thread where the continuation was
@; captured are lost.
@;
@; * Interaction between parameterizations and continuations. Extending
@; a parameterization can cause parameters to be kept alive that you
@; wouldn't expect from the lexical structure of the code when combining
@; delimited contiunations with parameterizations.
@;
@; https://github.com/racket/racket/issues/4216
@;
@; * Using composable continuations may lead to ballooning of memory
@; use when stack continuations that contain non-tail-recursive
@; functions.
@;
@; https://github.com/MarcKaufmann/congame/commit/8cb69a0f869e0f49cd36579605326fbe00938361
@;
@; For all 3, talk about how we debugged the issues and tools needed to
@; make debugging easier. Would be nice to have a proper debugger that
@; can walk the ``stack''.
@;
@; @section{Positives}
@;
@; * Natural to write flows without needing to worry too much about storing
@; intermediate state anywhere (except for ``resuming''). When talking
@; about this, we'll have to reference the web server paper @~cite[b:web-server]. Developing
@; the ``framework'' to support the flows only involves regular Racket
@; code, so there's no need to worry about migrating a database or any
@; other external system when making changes to how the internal state is
@; structured.
@;
@; * List some/all the variables that would have to be tracked and the various
@;   levels/layers at which they would have to be tracked.
@;
@; * Is there any aspect of the composition of studies that is easier because we
@;   use continuations, or would it have been equally easy with other ways of flow
@;   control? My guess is that it doesn't.

@(generate-bibliography #:sec-title "References")
