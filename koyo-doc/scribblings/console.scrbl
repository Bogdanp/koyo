#lang scribble/doc

@(require (for-label koyo/console
                     racket/base
                     racket/contract/base
                     racket/path)
          "koyo.rkt")

@title[#:tag "console"]{Console}

@defmodule[koyo/console]

@defparam[stubbed-components components (listof symbol?) #:value '(server)]{
  The ids of components that @racket[start-console] replaces with stubs
  when starting a system.

  @history[#:added "0.28"]
}

@defproc[(start-console [dynamic-module-path path?]
                        [namespace namespace? (make-base-empty-namespace)]
                        [repl (-> any) read-eval-print-loop]) void?]{

  Loads @racket[dynamic-module-path] and starts its system, stubbing out
  any components whose ids are members of @racket[stubbed-components].

  Use this procedure instead of @racket[start-console-here] when you
  want to control the set of stubbed components. For example:

  @racketblock[
    (require (prefix-in koyo: koyo/console) racket/path)
    (define here (path-only (syntax-source #'here)))
    (define dynamic.rkt (build-path here "dynamic.rkt"))
    (define (start-console)
      (define ns (make-base-empty-namespace))
      (current-namespace ns)
      (koyo:stubbed-components '(server pubsub))
      (koyo:start-console dynamic.rkt ns))
  ]

  The @racket[repl] procedure is called after the environment is set up
  to start the @emph{REPL}.

  @history[
   #:added "0.28"
   #:changed "0.41" @elem{Added the @racket[repl] argument.}
  ]
}

@defproc[(start-console-here) void?]{
  Searches for the ``dynamic.rkt'' module in the current project
  starting from @racket[current-directory], then loads it and starts its
  system, stubbing out the @racket[server] component. The search stops
  after looking through at most 10 directories or if a ``.git'' folder
  is found before a ``dynamic.rkt'' module. This is handy when you have
  an existing REPL session (eg. while working on a module in Emacs) and
  you want to bring in the functionality of @tt{raco koyo console} into
  that session.

  Requiring @racketmodname[(submod koyo/console dev)] is a shorthand
  for requiring this function and then calling it.

  @history[#:added "0.9"]
}
