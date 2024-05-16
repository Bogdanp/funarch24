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
to take advantage of this style of programming to implement a framework
for specifying composable surveys in a declarative way that elides most
of the details of day-to-day web programming from the study creator.

@(define greenspun-fn
   (note "To riff on Greenspun's Tenth Rule."))

In particular, Congame automatically tracks and manages much of the
state of study participants, which is a big boon, since Congame studies
are inherently stateful applications. A participant's next step may
depend on random treatments --- as in A/B tests --- or their own or other
participants' actions: they may only be allowed to move on if they pass
a comprehension test, and their payoff may be co-determined by other
participants with whom they interact in market games. Congame thus frees
the study creator from having to implement their own ad hoc bug-ridden
state management system.@|greenspun-fn|

In @secref{minimal} we show a minimal implementation of a system
similar to Congame and demonstrate how natural it is to program web
applications in this style. Then, in @secref{challenges} we talk about
some of the challenges involved in scaling such a system to include
more features and illustrate some of the debugging challenges. Finally,
in @secref{features}, we note some of the positive experiences we've
had working on this system, and explore similarities to related work in
@secref{conclusion}.

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
the participant from pressing the ``Back'' button in their browser
and redoing previous steps. @Figure-ref{example} shows a basic study
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
surprising ways.

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

@Figure-ref{challenge-2} demonstrates the parameter loss issue. When the
continuation from the first thread is restored in the second, the direct
assignment to the parameter is lost and the program displays ``p1''.

@subsection{Debugging}

Debugging memory leaks in the presence of continuations is tricky. For
a while, we had a set of small bugs in different areas of the software
that were composing together to form a larger bug which led to massive
memory leaks under load.

Our error reporting library was setting up exception handlers in its
inner data collection loop, making the loop no longer tail-recursive;
our own middleware to configure the aforementioned error reporting
library was accidentally creating a new instance of the error reporter
per request, instead of reusing a single one, meaning that for every
new request we would spin up a new data collection thread with a
non-tail-recursive inner loop. Finally, we were using composable
continuations to implement a special type of return from a sub-study
to its parent, so when a participant continued a study at the boundary
between parent and sub-study, we would see an increase in memory usage
from stacking the composable continuations on top of each other.

@figure-here[
  "remote-debugger"
  "A remote debugger for Racket."
  @elem{
    @(image "debugging-1.png" #:scale 0.33)
    @(image "debugging-2.png" #:scale 0.33)
  }]

@Figure-ref{remote-debugger} shows what this type of issue looks like
when visualized using a remote debugging tool available for Racket.
We can see memory use grow exponentially and we can see that the
main culprit appears to be the fact that we have allocated a lot of
``metacontinuation-frame'' values. This led us to our use of composable
continuations, which we promptly changed to delimited-but-not-composable
continuations, since we didn't actually need composable continuations
for our purposes. Our use of composable continuations was acting as an
amplifier for the other two bugs, and this change seemed to fix the
issue by drastically reducing the effect of the memory leak. In a way,
this fix gave us a false sense of security, since the other two problems
were still lurking, so we were surprised to later run into the same
problem again. Eventually, we were able to find the root problems and
fix them.

@section[#:tag "features"]{Features} @; Needs better title

@; Enabler, opportunities, benefits, wins, gains, features, multiplier
@; State management made easy
@; Stepping through with swagger

Using continuations allows us to progress through the study by
traversing the study tree using regular techniques without having to worry
much about the fact that we are doing web programming. While traversing
the tree, we are able to keep track of data structures that follow the
shape of the tree and, thereby, construct a ``study stack'' that allows
us to store participant data in a way that imitates lexical scope,
making it very natural for study writers to keep track of local data.

@; Counter |-> Decrement
@;         \-> Increment

Using continuations further allows us to use regular control flow
@~cite[b:queinnec b:web-server], meaning that every step of a study can decide
locally what the participant can do next. The actions in a step can close over
the step's environment and use regular functional programming techniques.
Consider a step that creates a quiz question along with the correct answer, asks
the participant to answer the question, and then gives the participant a score
based on their answer. Then the correct answer generated before displaying the
page can be stored in a local variable @racket[answer], which is avaible in the
scope of the action to be run after the page returns. The answer doesn't have to
be stored in the database or passed around explicitly, and the developer does
not have to think about the interaction with the user.

@; This ^ is a bit longish, but I wanted to clarify for myself what we
@; gain.
@; I mean the following, not sure if we can or should include a simpler
@; example.
@;
@; (defstep (quiz-step)
@;   (define options
@;     '(5 1 a 4 7))
@;   (define odd-one-out 3)
@;   (page
@;    (haml
@;     (form
@;       #:action (lambda (#:answer answer)
@;                   (cond [(= odd-one-out answer)]))
@;       @input-number[#:answer]{Tell us the number of the item that is the odd one out.}
@;       @submit-button)  )))

Since our approach is data-driven, changing our data structures requires
minor changes to our harness. For instance, adding support for view
handlers meant extending the step struct with another field and adding
one more request handler to traverse the study tree and display those
handlers as necessary. If instead we had chosen for a design where we
store a representation of steps in the database, then we would have
had to update the schema.

More generally, our design is flexible to changes. Extending studies
to be generated dynamically was as simple as adding one more case to
@racket[run-study] to handle callable study struct instances.
Furthermore, the combination the data-driven nature of the studies
and continuations that can close over arbitrary Racket objects allows us
to easily compose studies using the full suite of Racket's facilities,
including higher-order studies, just as we would any other tree-like data
structure.

@(define oTree-fn
  (note "Here we highlight purposefully dimensions in which oTree is
  lacking, even though oTree is clearly successful and superior in many
  dimensions."))

To highlight that the above benefits are in no way obvious or automatic,let us
illustrate how they are absent from oTree @~cite[b:oTree], a popular framework
for economic experiments.@|oTree-fn| oTree represents studies as apps that are
put in a linear sequence, with each app requiring its own folder with various
files. This design makes it hard to combine and reuse apps. In order to share
data between apps is only possible by storing data to a global namespace, which
is necessary whenever one app should only be run under some outcomes from a
previous app. In Congame, this type of problem is solved by transitioning to
another branch of the study and enabled by the data-driven approach. And while
this approach would be possible in Python, it is completely natural and
facilitated by continuations. While the ease of use of oTree makes developing
simple studies even simpler, its limitations on composing studies and managing
state makes developing harder studies even harder.

@section[#:tag "conclusion"]{Conclusion}

In conclusion, our main challenge consists in us using continuations so
infrequently that it's easy for us to misuse them or to doubt our own
understanding of how things are supposed to workh. The main benefit is that it
allows us to code stateful web applications while pretending that we are
coding a linear/sequential application. And while the benefits always remain,
our experience with continuations will reduce their downside, just as we hope
that our report will help others to avoid some of their pitfalls. We will continue
using continuations and reporting back on our setbacks and progress.

To be continued.







@(generate-bibliography #:sec-title "References")
