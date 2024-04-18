#lang scribble/acmart @sigplan @screen

@(require scribble/core
          scriblib/figure
          scriblib/footnote
          (only-in scribble/manual racket racketblock racketblock0)
          "bib.rkt")

@title{Continuations: what have they ever done for us?}
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
  participants. In the design of Congame, a platform for running such
  economic studies, we decided to use delimited continuations to
  manage the common flow of participants through a study.
  Here we report on the positives of this approach, as well as some
  challenges of using continuations, such as persisting and releasing
  values/bindings [at the right time/across requests], avoiding memory
  leaks, and the difficulty of debugging continuations.
}

@section[#:tag "intro"]{Introduction}

Continuations in a web context allow applications to be programmed in a
direct style @~cite[b:queinnec b:web-server]. In Congame, we have opted
to take advantage of this style of programming to implement
a framework for specifying composable surveys in a declarative way
that elides most of the details of day-to-day web programming from the
study creator.

This style provides a natural way to track and manage the state of a
participant progressing through a study. Such studies are stateful
applications where a participant's next step may depend on random
treatments --- A/B tests --- or their own or other participants' actions.
For example, a participant may only be allowed to move on if they pass a
comprehension test, and their payoff may be co-determined by other
participants with whom they interact in market games.

In @secref{minimal} we show a minimal implementation of a system similar to
Congame and demonstrate how natural it is to program web applications in this
style. Then, in @secref{challenges} we talk about some of the challenges
involved in scaling such a system to include more features and illustrate some
of the debugging challenges. Finally, in @secref{positives}, we note some of the
positive experiences we've had working on this system, and explore similarities
to related work in @secref{related}.

@section[#:tag "minimal"]{Mini Congame}

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

The core of Congame is a @emph{study}, represented as a tree of
@emph{steps} and other, nested, studies. Each @emph{step} in a study
is a procedure that generates a web page used to display and possibly
retrieve information to and from the participant being surveyed.
@Figure-ref{minimal-1} implements a minimal harness for constructing
and running these types of studies. A study creator uses the structures
defined in @figure-ref{minimal-1} alongside @emph{widgets} such as the
one defined in @figure-ref{minimal-2} to put together a study. The study
can then be run from within a Racket @~cite[b:racket] web server servlet
with @racket[run-study].

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

When a study is run, its steps are executed sequentially, and when a
step uses a widget, the widget reifies the current continuation of the
step and stores it in a hash table that maps URLs to continuations.
The URL of that continuation is then linked in the resulting HTML.
Once a continuation URL is visited, @racket[run-step] returns and the
study loop can continue to the next step. Once a continuation URL is
visited, the continuation is removed from the hash table to prevent
the participant from pressing the ``Back'' button in their browser and
redoing previous steps. @Figure-ref{example} shows a basic study
implemented using this framework.

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

@subsection{Too Few or Too Many Parameters}

In addition to the functionality presented in @secref{minimal},
Congame tracks participants' progress through each study in a database
in order to make it possible for them to resume their progress when
necessary (eg. when they close the browser tab and come back to the
website, or after their continuations expire, or after a server
re-deployment). To facilitate this, Congame keeps track of an in-memory
``study stack'' per participant that is serialized to the database
after every step. This stack is stored using dynamic variables
(@emph{parameters}@~cite[b:delimited-composable-control] in Racket
parlance). In some cases, continuations interact with parameters in
surprsing ways.

@(define issue-4216
   (note (url "https://github.com/racket/racket/issues/4216")))

When a continuation URL is visited and the continuation is restored, it
is run in a fresh Racket thread. Racket threads inherit the parameters
of their parent threads. That is, if a parameter is set to one value
in the parent, it will be set to the same value in the child thread.
When @racket[parameterize] is used to change the value of a parameter
for a particular block of code, instead of storing each new parameter
value in a thread cell individually, a parameterization object is
extended@|issue-4216| to include the new values of the changed
parameters. As a consequence, when @racket[parameterize] is used within
the dynamic extent of a continuation and that continuation is later
restored in a thread, more parameters than one might expect may end up
being restored, because the aforementioned extended parameterization
object is installed alongside it.

@Figure-ref{challenge-1} shows an example of the issue mentioned
above. When run, the program in figure 4 displays ``a b'', despite the
fact that the continuation is captured up to a prompt that resides
within the outer @racket[parameterize] form. Removing the inner use of
@racket[parameterize] would cause the program to display ``#f #f''.

@figure-here[
  "challenge-1"
  @elem{An example of the parameter revival issue.}
  @racketblock0[
  (define a (make-parameter #f))
  (define b (make-parameter #f))
  (define tag (make-continuation-prompt-tag))
  (define k
    (parameterize ([a 'a])
      (call-with-continuation-prompt
       (lambda ()
         (parameterize ([b 'b])
           ((call-with-current-continuation
             (λ (k) (thunk k))
             tag))))
       tag)))
  (call-with-continuation-prompt
   (lambda ()
     (k (lambda ()
          (printf "~s ~s~n" (a) (b)))))
   tag)]]

On the opposite side of the coin, Because the Racket web server restores
continuations in fresh threads, it is also possible to ``lose'' changes
to a parameter when using direct assignment. Directly assigning a
parameter in a thread records the change to the parameter in a thread
cell, without affecting the current parameterization. While implementing
congame, we naïvely used direct assignment to update the state of
the study stack, which lead to the parameter seemingly being reset
at certain times. To work around this issue, we modified the study
harness to explicitly pass around the current parameterization as the
participant progresses through the study.

@figure-here[
  "challenge-2"
  @elem{An example of the parameter loss issue.}
  @racketblock0[
  (define p (make-parameter #f))
  (define tag (make-continuation-prompt-tag))
  (define k-ch (make-channel))
  (void
   (thread
    (lambda ()
      (call-with-continuation-prompt
       (lambda ()
         (parameterize ([p 'p1])
           (p 'p2)
           ((call-with-current-continuation
             (lambda (k)
               (thunk (channel-put k-ch k)))
             tag))))
       tag))))
  (thread-wait
   (thread
    (lambda ()
      (define k (channel-get k-ch))
      (call-with-continuation-prompt
       (lambda ()
         (k (λ () (printf "~s~n" (p)))))
       tag))))]]

@Figure-ref{challenge-2} demonstrates the parameter loss issue. When
the continuation from the first thread is restored in the second, the
direct assignment to the parameter is lost and the program displays
``p1''.

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

@section[#:tag "positives"]{Positives} @; Needs better title

@; Positives in extending the core functionality and in building studies
@; compared to other frameworks (e.g.,@~cite[b:oTree]).
@; Automatic state management reduces the scope for errors and enables
@; composable and reusable studies.
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

@section[#:tag "related"]{Related Work}

@(generate-bibliography #:sec-title "References")
