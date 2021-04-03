#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     racket/string
                     web-server/dispatchers/dispatch
                     web-server/http
                     web-server/safety-limits
                     web-server/web-server)
          "koyo.rkt")

@title[#:tag "server"]{Server}

@defmodule[koyo/server]

This module provides a web server component.

@defproc[(server? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a server.
}

@defproc[((make-server-factory [#:unix-socket socket-path (or/c #f path-string?) #f]
                               [#:host host non-empty-string? "127.0.0.1"]
                               [#:port port (integer-in 0 65535) 8000]
                               [#:limits limits safety-limits? (make-safety-limits)]) [dispatcher dispatcher/c]) server?]{
  Creates a component that, when started, runs a web server
  bound to @racket[#:host] and @racket[#:port] using the given
  @racket[dispatcher]. When the @racket[#:unix-socket] argument is
  provided, the server is bound to a socket at that path instead and the
  @racket[#:host] and @racket[#:port] arguments are ignored. When the
  server component is stopped, the unix socket (if provided) is
  deleted.

  The @racket[#:limits] are passed directly to @racket[serve].
}
