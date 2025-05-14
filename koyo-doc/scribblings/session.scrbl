#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     racket/file
                     racket/serialize
                     racket/string
                     web-server/http)
          "koyo.rkt")

@title[#:tag "session"]{Sessions}

@defmodule[koyo/session]

This module exposes a component for storing and retrieving data from a
browser session.

@defparam[current-session-manager sessions (or/c #f session-manager?)]{
  This parameter holds a reference to the current session manager.

  Functions @racket[session-ref], @racket[session-set!],
  @racket[session-update!] and @racket[session-remove!] use this
  parameter under the hood to locate the manager.

  The parameter is installed by @racket[wrap-session].
}

@defparam[current-session-id session-id (or/c #f non-empty-string?)]{
  This parameter holds the session id for the current request.  If the
  current request handler is wrapped with @racket[wrap-session], then
  this is guaranteed not to be @racket[#f].
}

@defproc[(session-manager? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a session manager.
}

@defproc[(make-session-manager-factory [#:cookie-name cookie-name non-empty-string?]
                                       [#:cookie-path cookie-path path-string? "/"]
                                       [#:cookie-secure? cookie-secure? boolean? #t]
                                       [#:cookie-http-only? cookie-http-only? boolean? #t]
                                       [#:cookie-same-site cookie-same-site (or/c 'lax 'strict) 'strict]
                                       [#:shelf-life shelf-life exact-positive-integer?]
                                       [#:secret-key secret-key bytes?]
                                       [#:store store session-store?]) (-> session-manager?)]{

  Returns a function that will create a session manager in accordance
  with the given options.
}

@deftogether[(
  @defproc*[
    ([(session-ref [k symbol?]) any/c]
     [(session-ref [k symbol?] [d any/c]) any/c]
  )]
  @defproc*[
    ([(session-manager-ref [sm session-manager?] [k symbol?]) any/c]
     [(session-manager-ref [sm session-manager?] [k symbol?] [d any/c]) any/c]
  )]
)]{

  Looks up @racket[k] in the current session, returning @racket[d] if
  the key is not found.  If @racket[d] is not provided, then a user
  error is raised.
}

@deftogether[(
  @defproc[(session-set! [k symbol?] [v serializable?]) void?]
  @defproc[(session-manager-set! [sm session-manager?] [k symbol?] [v serializable?]) void?]
)]{
  Stores @racket[v] under the @racket[k] key in the current session.
}

@deftogether[(
  @defproc*[(
    [(session-update! [k symbol?]
                      [p (-> any/c serializable?)]) serializable?]
    [(session-update! [k symbol?]
                      [p (-> any/c serializable?)]
                      [d any/c]) serializable?]
  )]
  @defproc*[(
    [(session-manager-update! [sm session-manager?]
                              [k symbol?]
                              [p (-> any/c serializable?)]) serializable?]
    [(session-manager-update! [sm session-manager?]
                              [k symbol?]
                              [p (-> any/c serializable?)]
                              [d any/c]) serializable?]
  )]
)]{

  Updates @racket[k] in the current session by applying @racket[p] to
  it.  If @racket[k] is not set then @racket[d] is used as the default
  value.  If @racket[d] is not provided, then a user error is raised.
}

@deftogether[(
  @defproc[(session-remove! [k symbol?]) void?]
  @defproc[(session-manager-remove! [sm session-manager?] [k symbol?]) void?]
)]{
  Removes @racket[k] from the current session.  Does nothing if
  @racket[k] is not set in the current session.
}

@defproc[((wrap-session [sm session-manager?]) [handler procedure?]) (-> request? any/c ... response?)]{
  Wraps a handler such that the appropriate session information for
  the current visitor is loaded and, eventually, stored.

  If the current visitor doesn't have a session cookie then a new one
  is generated and added to the response.

  Each session's lifetime is extended with every page load.
}

@section{Session Stores}

@deftech{Session stores} decide how session data is stored.

@defproc[(session-store? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a session store.
}

@deftogether[
  (@defproc[(memory-session-store? [v any/c]) boolean?]
   @defproc[(make-memory-session-store [#:ttl ttl exact-positive-integer? (* 7 86400)]
                                       [#:file-path path path-string? (make-temporary-file)]) session-store?])]{

  A session store that keeps all session data in memory, persisting
  and loading it to/from disk on shutdown and startup.

  The @racket[#:ttl] argument controls how many seconds to wait before
  removing stale sessions.
}

@subsection{PostgreSQL Session Store}
@defmodule[koyo/session/postgres]

This module provides a @tech{session store} backed by PostgreSQL tables.

@deftogether[
  (@defproc[(postgres-session-store? [v any/c]) boolean?]
   @defproc[(make-postgres-session-store [database database?]
                                         [#:ttl ttl exact-positive-integer? (* 7 86400)]) session-store?])]{

  A session store that persists session data the given PostgreSQL
  @racket[database].

  The @racket[#:ttl] argument determines how long sessions are kept
  around for.

  @history[#:added "0.25"]
}
