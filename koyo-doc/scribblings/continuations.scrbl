#lang scribble/doc

@(require (for-label racket/base
                     racket/contract
                     web-server/http
                     web-server/servlet/web)
          "koyo.rkt")

@title[#:tag "continuations"]{Continuations}

@defmodule[koyo/continuation]

Continuations in a web context are super valuable when it comes
quickly prototyping certain functionality.  However, because it stores
continuation ids within URLs, the web server is succeptible to session
hijacking.  This is @bold{by design}.  For some use cases it makes
sense to allow users to share their URLs and have someone else be able
to pick up from where they left off.  In most web applications,
though, that's not what you want.

Let me illustrate the issue with an example.  Say you have an
e-commerce website and you implement your "add to cart" buttons using
continuations.  If you don't protect your continuations, then a user
could copy the URL for that button and give it to someone else; that
other person would then inherit all of the state associated with that
continuation.  You might think that your users are unlikely to do
that, but there are other cases in which browsers can leak information
about URLs in a web application.  One such case being @emph{Referrer}
headers.

To guard against the aforementioned issues, this module provides
variants of the web server's continuation-related functions that
protect themselves against being hijacked.  They do this by
associating a random session cookie with each continuation that is
captured.  When a continuation is called and the visitor's session
does not contain said cookie, then a "403 Forbidden" response is
returned.

@deftogether[
  (@defparam[current-continuation-key-cookie-path path path-string?
             #:value "/"]
   @defparam[current-continuation-key-cookie-secure? secure? boolean?
             #:value #t])]{

  These parameters control the @emph{Path=} attribute and the
  @emph{Secure=} attribute, respectively, of the continuation key
  cookie.

  Since @racket[current-continuation-key-cookie-secure?] is
  @racket[#t] by default, you're expected to run your server behind
  TLS even in development mode, otherwise your cookies won't get set
  and you'll encounter @emph{403 Forbidden} responses every time you
  try to call a continuation.
}

@defparam[current-continuation-mismatch-handler handler (-> request? response?)]{
  This parameter holds the handler that is run whenever a continuation
  is called with an invalid continuation key in the request.  The
  default implementation returns a @emph{403 Forbidden} response with
  the text "Forbidden" in its body.
}

@defparam[current-continuation-wrapper wrapper (-> (-> request? response?)
                                                   (-> request? response?))]{
  Since continuations are executed outside of the standard request -
  response lifecycle, this means that any middleware/wrappers you set
  up on your dispatchers will not wrap your continuations.  To get
  around this, you can register a your middleware stack with this
  parameter and all protected continuations will be wrapped
  appropriately.
}

@defproc[((wrap-protect-continuations [handler (-> request? response?)]) [req request?]) response?]{
  This function sets up the continuation key and updates all responses
  that pass through it to include the continuation key cookie.

  Without this, all protected continuations will fail out so don't
  forget to add it to your middleware stack.
}

@deftogether[
  (@defproc[(send/suspend/protect [p (-> string? can-be-response?)]) request?]
   @defproc[(send/forward/protect [p (-> string? can-be-response?)]) request?]
   @defproc[(send/suspend/dispatch/protect [p (-> (-> (-> request? any) string?) can-be-response?)]) any]
   @defproc[(redirect/get/protect [#:headers headers (listof header?) null]) request?]
   @defproc[(redirect/get/forget/protect [#:headers headers (listof header?) null]) request?])]{

  Protected variants of the continuation-related functions in
  @secref["web" #:doc '(lib "web-server/scribblings/web-server.scrbl")].
}
