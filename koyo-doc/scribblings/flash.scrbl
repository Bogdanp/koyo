#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     web-server/http
                     web-server/servlet)
          "koyo.rkt")

@title[#:tag "flash"]{Flash}

@defmodule[koyo/flash]

This module exposes a component for storing and retrieving flash
messages.  @deftech{Flash messages} which are set on one page, are
available for display on the next and then cleared as soon as the next
page is processed.  Each message is bound to a specific user session.

@defparam[current-flash-messages messages (listof (cons/c symbol? string?))]{
  This parameter holds the set of flash messages for the current
  request.
}

@defparam[current-flash-manager flashes (or/c #f flash-manager?)]{
  This parameters holds the current flash manager for a particular
  request.  It is installed by @racket[wrap-flash] and automatically
  used by @racket[flash] if an explicit manager is not passed in.
}

@defproc[(flash-manager? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a flash manager.
}

@defproc[(make-flash-manager [sessions session-manager?]) flash-manager?]{
  Creates a new flash manager which stores its messages using
  @racket[sessions].
}

@defproc[((wrap-flash [flashes flash-manager?]) [handler procedure?]) (-> request? any/c ... response?)]{
  Wraps a handler such that flash messages from the previous request
  get collected into @racket[current-flash-messages] and then cleared
  from the current session.

  This wrapper must be applied after @racket[wrap-session].
}

@defproc*[(
  [(flash [key symbol?]
          [message string?]) void?]
  [(flash [flashes flash-manager?]
          [key symbol?]
          [message string?]) void?]
)]{

  Adds a flash message to the current session.  If @racket[flashes] is
  not provided, the @racket[current-flash-manager] is used.
}
