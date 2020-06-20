#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     web-server/http
                     web-server/servlet/servlet-structs)
          "koyo.rkt")

@title[#:tag "errors"]{Errors}

@defmodule[koyo/error]

This module exposes an error handling middleware that displays stack
traces in the browser in a prettier way that the default web server
error handler:

@centered[
 (image
  #:scale 1/3
  (build-path media-path "error-example.png"))]

@defproc[((wrap-errors [debug? boolean?]) [handler (-> request? can-be-response?)]) (-> request? can-be-response?)]{
  Wraps @racket[handler] such that, when @racket[debug?] is
  @racket[#t], unhandled exceptions are caught, logged to standard
  error using @racket[error-display-handler] and formatted for display
  in the web browser like in the screenshot above.
}

@defparam[current-production-error-page page (-> request? can-be-response?)]{
  This parameter holds the procedure that is used to render error
  pages when the @racket[debug?] parameter passed to
  @racket[wrap-errors] is @racket[#f].

  The default implementation is completely unstyled and simply states
  that an error occurred and the user should try again later.
}
