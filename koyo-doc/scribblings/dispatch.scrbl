#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     web-server/dispatch
                     web-server/dispatchers/dispatch
                     web-server/http
                     web-server/servlet-dispatch)
          "koyo.rkt")

@title[#:tag "dispatch"]{Dispatch}

@defmodule[koyo/dispatch]

This module provides syntax for building a role-aware dispatch
procedure and dispatcher combinators.

@defform[
  #:literals (else)
  (dispatch-rules+roles
   dispatch-clause ...
   maybe-else-clause)
  #:grammar
  [(dispatch-clause [dispatch-pattern
                     maybe-method
                     maybe-roles
                     maybe-name
                     dispatch-proc])
   (dispatch-pattern (code:line ())
                     (string . dispatch-pattern)
                     (bidi-match-expander ... . dispatch-pattern)
                     (bidi-match-expander . dispatch-pattern))
   (maybe-method (code:line)
                 (code:line #:method method))
   (maybe-roles (code:line)
                (code:line #:roles (role-id ...)))
   (maybe-name (code:line)
               (code:line #:name name-expr))
   (maybe-else-clause (code:line)
                      [else else-proc])
   (method match-pattern)]
   #:contracts
   [(name-expr symbol?)
    (else-proc (-> request? response?))
    (dispatch-proc (-> request? any/c ... response?))]]{

  A variant of @racket[dispatch-rules] where each
  @racket[dispatch-clause] takes an optional list of roles and an
  optional name.

  Returns three values: a dispatcher procedure as in
  @racket[dispatch-rules], a procedure that can generate reverse URIs
  and a procedure that, given a @racket[request], can return the
  required set of roles for the matching @racket[dispatch-proc].

  Reverse URI generation differs from @racket[dispatch-rules] in that
  the first argument must be a symbol representing the name of the route
  rather than a @racket[dispatch-proc]. This helps avoid deep dependency
  chains between routes. The name for a route is either the value passed
  to @racket[maybe-name] or the name of its dispatch procedure.
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

  Combine @racket[dispatch-rules+roles], @racket[dispatch/servlet] and
  @racket[dispatch/mount] to embed modular sub-applications into larger
  applications where the former have no knowledge of the URL structure
  of the larger application.

  @history[#:added "0.28"]
}

@defproc[(dispatch/plain [servlet (-> request? response?)]) dispatcher/c]{
  Returns a dispatcher that servers requests using @racket[servlet],
  without installing a continuation prompt. You probably don't need to
  use this.

  @history[#:added "0.48"]
}
