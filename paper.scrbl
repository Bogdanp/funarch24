#lang scribble/acmart @sigplan @screen

@(require scribble/core
          scriblib/figure
          scriblib/footnote
          (only-in scribble/manual racket racketblock racketblock0)
          scribble/private/lang-parameters
          scribble/latex-properties
          "bib.rkt")

@(default-figure-caption-style #f)
@(default-figure-counter-style 'bold)
@(default-figure-label-text (make-element 'bold "Figure"))
@(define acm-meta
  (make-element (make-style #f (list (make-tex-addition "acm-metadata.tex")))
                ""))

@acm-meta

@title{Continuations: What Have They Ever Done for Us? (Experience Report)}

@author[
  "Marc Kaufmann"
  #:orcid "0000-0003-0366-6329"
  #:email (email "kaufmannm@ceu.edu")
  #:affiliation (affiliation
                 #:institution @institution{Central European University}
                 #:city "Vienna"
                 #:country "Austria")
]

@author[
  "Bogdan Popa"
  #:orcid "0009-0001-7815-4742"
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
  economic studies, we decided to use delimited continuations to manage
  the common flow of participants through a study. Here we report on
  the positives of this approach, as well as some challenges of using
  continuations, such as persisting data across requests, working with
  dynamic variables, avoiding memory leaks, and the difficulty of
  debugging continuations.
}

@CCSXML|{

<ccs2012>
   <concept>
       <concept_id>10011007.10011006.10011050.10011017</concept_id>
       <concept_desc>Software and its engineering~Domain specific languages</concept_desc>
       <concept_significance>300</concept_significance>
       </concept>
   <concept>
       <concept_id>10011007.10010940.10010971.10010972.10010545</concept_id>
       <concept_desc>Software and its engineering~Data flow architectures</concept_desc>
       <concept_significance>300</concept_significance>
       </concept>
   <concept>
       <concept_id>10011007.10011074.10011092.10011096</concept_id>
       <concept_desc>Software and its engineering~Reusability</concept_desc>
       <concept_significance>500</concept_significance>
       </concept>
   <concept>
       <concept_id>10011007.10011006.10011008.10011024.10011027</concept_id>
       <concept_desc>Software and its engineering~Control structures</concept_desc>
       <concept_significance>500</concept_significance>
       </concept>
   <concept>
       <concept_id>10011007.10011006.10011008.10011009.10011012</concept_id>
       <concept_desc>Software and its engineering~Functional languages</concept_desc>
       <concept_significance>300</concept_significance>
       </concept>
 </ccs2012>

}|

@ccsdesc[#:number 500 "Software and its engineering~Reusability"]
@ccsdesc[#:number 500 "Software and its engineering~Control structures"]
@ccsdesc[#:number 300 "Software and its engineering~Domain specific languages"]
@ccsdesc[#:number 300 "Software and its engineering~Data flow architectures"]
@ccsdesc[#:number 300 "Software and its engineering~Functional languages"]


@section[#:tag "intro"]{Introduction}

Continuations in a web context allow applications to be programmed
in a direct style @~cite[b:queinnec b:web-server]. In the design of
Congame, a platform for running economics studies, we took advantage
of this style of programming to implement a framework for specifying
composable surveys in a declarative way that elides most of the details
of day-to-day web programming from the study creator.

@(define greenspun-fn
   (note "To riff on Greenspun's Tenth Rule."))

In particular, Congame automatically tracks and manages much of the
state of study participants, which is a big boon, since Congame studies
are inherently stateful applications. A participant's next step may
depend on random treatments --- as in A/B tests --- or their own or
other participants' actions: they may only be allowed to move on if
they pass a comprehension test, and their payoff may be co-determined
by other participants with whom they interact in market games. Congame
thus frees the study creator from having to implement their own ad hoc
bug-ridden state management system.@|greenspun-fn|

We report on our experience using delimited continuations to implement
Congame. In @secref{minimal} we show a minimal implementation of a
system similar to Congame and demonstrate how natural it is to program
web applications in this style. Then, in @secref{benefits} we note some
positive benefits that follow from our design and compare our system
to a popular platform for creating studies. In @secref{challenges} we
describe some challenges of managing the data flow and of debugging
in such a system. Finally, in @secref{reflections} we reflect on and
broadly recommend our approach, analyze its pros and cons, and make
suggestions targeted at other functional architects looking to implement
similar systems.

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
@emph{steps} and sub-studies. Each @emph{step} in a study is a
procedure that generates a web page used to display and possibly
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

When a study is run, its steps are executed sequentially, and when a step uses a
widget, the widget reifies the current continuation of the step and stores it in
a hash table that maps URLs to continuations. The URL of that continuation is
then linked in the resulting HTML. Once a continuation URL is visited, the
continuation is restored so that @racket[run-step] returns and the study loop
can continue to the next step. Following this visit, the continuation is removed
from the hash table to prevent the participant from pressing the ``Back'' button
in their browser and redoing previous steps. The Racket Web Server
@~cite[b:web-server] provides @racket[send/suspend/dispatch], which takes care
of all the continuation management. @Figure-ref{example} shows a basic study
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

@section[#:tag "benefits"]{Benefits of Continuations}

Using continuations allows us to progress through the study by
traversing the study tree using regular techniques without having to
worry much about the fact that we are doing web programming. While
performing the traversal, we keep track of the participant's position
in the tree which allows us to store participant data in a way that
imitates lexical scope, making it very natural for study writers to keep
track of local data.

Using continuations further allows us to use regular control flow
@~cite[b:queinnec b:web-server], meaning that every step of a study
can decide locally what the participant can do next. The actions in a
step can close over the step's environment and use regular functional
programming techniques. @Figure-ref{var-example} illustrates that we
can write a step that tosses a coin and stores the outcome in the local
variable @racket[toss], then offers a choice to the participant and,
after the participant makes their choice, checks the answer against
@racket[toss].

@figure-here[
  "var-example"
  @elem{A study showing lexical scope and data flow in Congame.}
  @racketblock0[
  (defvar* ok?)
  (defstep (intro)
    (html
     (h1 "Welcome to the study!")
     (button "Start")))
  (defstep (heads-or-tails)
    (define toss (random-ref '(h t)))
    (html
     (button "Heads" (λ () (set! ok? (eq? toss 'h))))
     " or "
     (button "Tails" (λ () (set! ok? (eq? toss 't))))))
  (defstep (result)
    (html
     (if ok?
         (p "You guessed right.")
         (p "You guessed wrong."))))
  (defstudy choices
    [heads-or-tails --> ,(λ () done)])
  (defstudy example
    [intro --> choices --> result --> ,(λ () done)])]]

Since our approach is data-driven, changing our data structures requires
only minor changes to our harness. For instance, adding support for
view handlers --- study-specific static pages --- meant extending the
@racket[step] struct with another field and adding one more request
handler to traverse the study tree and display those handlers as
necessary. If instead we had opted for a design where we store a
representation of steps in the database, then we would have had to
update the schema.

More generally, our design is flexible to changes. Extending studies
to be generated dynamically was as simple as adding one more case
to @racket[run-study] to handle callable study struct instances.
Furthermore, the combination of continuations that can close over
arbitrary Racket objects alongside the data-driven nature of the studies
allows us to easily create and compose studies using the full suite of
Racket's facilities, including higher-order studies, just as we would
any other tree-like data structure.

@(define oTree-fn
  (note "Here we highlight purposefully dimensions in which oTree is
  lacking, even though oTree is clearly successful and superior to
  Congame in many respects."))

To highlight that the above benefits are in no way obvious or automatic,
let us illustrate how they are absent from oTree @~cite[b:oTree],
a popular framework for economic experiments.@|oTree-fn| oTree
represents studies as apps that are run in a linear sequence, with
each app requiring its own folder with various files. This design
makes it hard to combine and reuse apps, not least due to difficulties
in sharing data between apps. For example, when @tt{app2} should
be run only for participants with a high score in @tt{app1}, then
@tt{app1} has to store the score in a global namespace, then @tt{app2}
looks up this score and decides whether to run or hand over to
@tt{app3}. In Congame, @tt{study1} can locally decide to transition
to @tt{study2} or @tt{study3} depending on a high or low score.
In @figure-ref{otree-example} we replicate the Congame example from
@figure-ref{var-example} using oTree, illustrating the problem of
sharing data between apps.@note{Of course, such a simple study would not
normally be split across multiple apps.} So, while the ease of use of
oTree makes developing simple studies easy, its limitations on composing
studies and managing state make developing complex studies hard.

@figure-here[
  #:style left-figure-style
  "otree-example"
  @elem{A heavily edited-for-space example of the coin toss study implemented in oTree.}
  @verbatim|{
# In settings.py:
SESSION_CONFIGS = [{"app_sequence": [
  'Intro', 'Choices', 'Result'
]}]
PARTICIPANT_FIELDS = ['ok']

# In Intro/IntroPage.html:
# ...
{{ block content }}
  <h1>Welcome to the study!</h1>
  {{ next_button }}
{{ endblock }}

# In Choices/__init__.py:
# ...
class Player(BasePlayer):
    choice = models.StringField(label='Choice:')

class ChoicePage(Page):
    form_model = 'player'
    form_fields = ['choice']

    @staticmethod
    def before_next_page(player, timedout):
        player.participant.ok = player.choice == random.choice(['heads', 'tails'])
# ...

# In Result/ResultPage.html:
# ...
{{ block content }}
  {% if player.participant.ok %}
    <p>You chose right.</p>
  {% else %}
    <p>You chose wrong.</p>
  {% endif %}
{{ endblock }}
}|]

Of course, our design could be replicated without continuations, but
continuations made this design natural and allowed us to stay flexible.

@section[#:tag "challenges"]{Challenges of Continuations}

@subsection{Too Few or Too Many Parameters}

To allow participants to resume a study when necessary (e.g., when
they close the browser tab and return to the website, after their
continuations expire, or after a server re-deployment), Congame
persists the participant's position: the fully-qualified path to the
node they reached within the study tree as represented as a list of
ids. In memory, this position is tracked using dynamic variables
(@emph{parameters}@~cite[b:delimited-composable-control] in Racket
parlance). In specific cases, continuations interact with parameters in
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
restored in a thread, more parameters than might be expected may end up
being restored, because the aforementioned extended parameterization
object is installed alongside it.

@Figure-ref{challenge-1} shows an example of this issue. When run, the program
in figure 4 displays ``a b''; but, since the continuation is captured
up to a prompt that resides within the outer @racket[parameterize] form setting
the parameter @racket[a], we had initially expected to see ``#f b''. Removing
the inner use of @racket[parameterize] causes the program to display ``#f #f''.

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

On the opposite side of the coin, because the Racket web server restores
continuations in fresh threads, it is also possible to ``lose'' changes
to a parameter when using direct assignment. Directly assigning a
parameter in a thread records the change to the parameter in a thread
cell, without affecting the current parameterization. Originally,
we had used direct assignment to update the participant's position,
which caused the parameter to reset at the boundary between steps.
Then, we switched to explicit uses of @racket[parameterize], which
extends the parameterization such that the changes are available in the
restored continuation, as in the previous example. However, this was not
foolproof since whether or not a parameterization is extended depends
on where the @racket[parameterize] is situated in the dynamic extent of
the delimited continuation: if it is before the prompt, the extension is
not visible, otherwise it is. Finally, we settled on manually passing
around the parameterization between steps to have full control over what
values the parameters we depend on have at any time. We have not yet
experimented with using continuation marks @~cite[b:marks] directly.

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

@subsection[#:tag "debugging"]{Debugging}

Debugging memory leaks in the presence of continuations is tricky. We
had a set of small bugs in different areas of the system that were
composing together to form a larger bug which led to massive memory
leaks under load.

First, our error reporting library was setting up exception
handlers in its inner data collection loop, making the loop no
longer tail-recursive. Second, our own middleware to configure the
aforementioned error reporting library was accidentally creating a new
instance of the error reporter per request, instead of reusing a single
one, meaning that for every new request we would spin up a new data
collection thread with a non-tail-recursive inner loop. Finally, we were
using composable continuations to implement a special type of return
from a sub-study to its parent, so when a participant continued a study
at this boundary between parent and sub-study, we would see an increase
in memory usage from stacking the composable continuations on top of
each other.

@figure-here[
  "remote-debugger"
  "A remote debugger for Racket showing a spike in memory usage and the values that are taking up that memory."
  @elem{
    @(image "debugging-1.png" #:scale 0.33)
    @(image "debugging-2.png" #:scale 0.33)
  }]

@Figure-ref{remote-debugger} shows what this type of issue looks like
when visualized using dbg @~cite[b:dbg], a remote debugging tool for
Racket. We can see memory use grow exponentially and that this stems
from allocating a lot of ``metacontinuation-frame'' values. This drew
our attention to our use of composable continuations, which we promptly
changed to delimited-but-not-composable continuations, since we didn't
actually need composable continuations for our purposes. Our use of
composable continuations amplified the other two bugs, and this change
seemed to fix the issue by drastically reducing the effect of the memory
leak. In a way, this fix gave us a false sense of security, since the
other two problems were still lurking, so we were surprised to later run
into the same problem again. Eventually, we were able to find the root
problems and fix them.

@section[#:tag "reflections"]{Reflections & Recommendations}

Continuations allow us to write web code in a direct style, simplifying
the job of embedding domain specific languages in a web context.
Without leveraging continuations, the study harness introduced in
@secref{minimal} would have been a lot more cumbersome to implement
than a straightforward depth-first traversal of a tree. The issues
we encountered were primarily due to unexpected interactions
between dynamic variables and delimited continuations. We recommend
that functional architects avoid combining dynamic variables and
continuations in their systems where possible, or do so with care,
while taking into account the issues presented in @secref{challenges}.
If we were to rewrite Congame today, we would explicitly pass around
a context object containing the data we need between steps instead of
using dynamic variables. We also recommend that functional architects
carefully consider whether they need composable @emph{and} delimited
continuations, or whether delimited alone will suffice.

To aid debugging, languages should provide tooling to allow
continuations to be inspected at runtime. That way, when encountering
issues such as the one presented in @secref{debugging}, programmers
would have an easier time finding the source of memory leaks. For
example, in the case of Racket's ``metacontinuation-frame'' values, it
would be helpful if those values were inspectable to determine their
source location and what other objects they hold references to.

We believe continuations are the right abstraction for implementing
interactive systems as targeted by Congame, such as surveys and market
games, as well as any other system that requires some computation to
be suspended until the user takes action (e.g. shopping carts, or
simulations where part of the computation is delegated to another black
box, etc.). For our use case, where backtracking via the browser's
``Back'' button is undesirable, multi-shot continuations are not
required but, in other applications, they may be useful. In that sense,
in a language without continuations, coroutines would likely provide
us with the same benefits, but would not be as good a fit for use
cases where the possibility of branching the user's progress through an
interaction is a desirable feature (e.g. having the ability to open
a separate tab to take a different path through a study tree). Other
approaches, such as regular web programming with manual routing or a
weaker form of ``continuations'' where URLs get mapped to closures,
would not permit us to implement the core study harness in such a direct
and simple way.

@acks{We would like to thank the anonymous reviewers for their comments
and suggestions. This work was partially funded by Central European
University Private University.}

@(generate-bibliography #:sec-title "References")
