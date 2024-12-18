#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     web-server/dispatch
                     web-server/dispatchers/dispatch
                     web-server/http)
          "koyo.rkt")

@title[#:tag "dispatch"]{Dispatch}

@defmodule[koyo/dispatch]

This module provides syntax for building a role-aware dispatch
function and dispatcher combinators.

@defform[
  #:literals (else)
  (dispatch-rules+roles dispatch-clause ... maybe-else-clause)
  #:grammar
  [(dispatch-clause [dispatch-pattern maybe-method maybe-roles maybe-name dispatch-fun])
   (dispatch-pattern (code:line ())
                     (string . dispatch-pattern)
                     (bidi-match-expander ... . dispatch-pattern)
                     (bidi-match-expander . dispatch-pattern))
   (maybe-method (code:line)
                 (code:line #:method method))
   (maybe-roles (code:line)
                (code:line #:roles (role ...)))
   (maybe-name (code:line)
               (code:line #:name name))
   (maybe-else-clause (code:line)
                      [else else-fun])
   (method pat)
   (role id)
   (name symbol?)]
   #:contracts
   [(else-fun (-> request? response?))
    (dispatch-fun (-> request? any/c ... response?))]]{

  Like @racket[dispatch-rules] but each @racket[dispatch-clause] takes
  an optional list of roles and an optional name.

  Returns three values: the first being a dispatcher function like in
  @racket[dispatch-rules], the second a function that can generate
  reverse URIs and the third a function that, given a
  @racket[request], can return the required set of roles for the
  matching @racket[dispatch-fun].

  Reverse URI generation is different from @racket[dispatch-rules] in
  that the first argument is a symbol representing the name of the
  route rather than the specific @racket[dispatch-fun] that was used.
  This helps avoid deep dependency chains between routes.  The name
  for a route is either the value passed to @racket[maybe-name] or the
  name of its dispatch function.
}

@defproc[(dispatch/mount [root string?]
                         [dispatcher dispatcher/c]) dispatcher/c]{

  Returns a new dispatcher that reroots incoming requests using
  @racket[request-reroot] with the given @racket[root] before passing
  the result to @racket[dispatcher]. Calls @racket[next-dispatcher] when
  a request cannot be rerooted.

  The rerooted dispatcher is parameterized to cooperate with the
  @racket[reverse-uri] procedure, meaning that generating a reverse URL
  from within a request handler called by the dispatcher results in a
  URL with the @racket[root] prepended to it.

  @history[#:added "0.28"]
}
