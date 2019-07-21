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

@defparam[current-session-id session-id (or/c false/c non-empty-string?)]{
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

@defproc*[
  ([(session-manager-ref [sm session-manager?] [k symbol?]) any/c]
   [(session-manager-ref [sm session-manager?] [k symbol?] [d any/c]) any/c])]{

  Looks up @racket[k] in the current session, returning @racket[d] if
  the key is not found.  If @racket[d] is not provided, then a user
  error is raised.
}

@defproc[(session-manager-set! [sm session-manager?] [k symbol?] [v serializable?]) void?]{
  Stores @racket[v] under the @racket[k] key in the current session.
}

@defproc*[
  ([(session-manager-update! [sm session-manager?]
                             [k symbol?]
                             [p (-> any/c serializable?)]) serializable?]
   [(session-manager-update! [sm session-manager?]
                             [k symbol?]
                             [p (-> any/c serializable?)]
                             [d any/c]) serializable?])]{

  Updates @racket[k] in the current session by applying @racket[p] to
  it.  If @racket[k] is not set then @racket[d] is used as the default
  value.  If @racket[d] is not provided, then a user error is raised.
}

@defproc[(session-manager-remove! [sm session-manager?] [k symbol?]) void?]{
  Removes @racket[k] from the current session.  Does nothing if
  @racket[k] is not set in the current session.
}

@defproc[(wrap-session [sm session-manager?]) (-> (-> request? response?)
                                                  (-> request? response?))]{
  Wraps a handler such that the appropriate session information for
  the current visitor is loaded and, eventually, stored.

  If the current visitor doesn't have a session cookie then a new one
  is generated and added to the response.

  Each session's lifetime is extended with every subsequent visit.
}

@section{Session Stores}

@defproc[(session-store? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a session store.
}

@deftogether[
  (@defproc[(memory-session-store? [v any/c]) boolean?]
   @defproc[(make-memory-session-store [#:ttl ttl exact-positive-integer? (* 7 86400)]
                                       [#:file-path path path-string? (make-temporary-file)]) session-store?])]{

  A session store that keeps all session data in memory, persisting
  and loading it to/from disk on shutdown and startup.

  @racket[ttl] controls how many seconds to wait before removing stale
  sessions.
}
