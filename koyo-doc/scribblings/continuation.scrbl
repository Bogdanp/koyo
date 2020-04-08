#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     web-server/http
                     web-server/servlet
                     web-server/servlet/web)
          "koyo.rkt")

@title[#:tag "continuations"]{Continuations}

@defmodule[koyo/continuation]

Continuations in a web context are super valuable when it comes to
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
that, but there are cases, such as via @tt{Referer} headers, in which
browsers can leak information about URLs in a web application.

@(define ccmh (racket current-continuation-mismatch-handler))

To guard against the aforementioned issues, this module provides
variants of the web server's continuation-related functions that
protect themselves against being hijacked.  They do this by
associating a random session cookie with each continuation that is
captured.  When a continuation is called and the visitor's session
does not contain said cookie, @ccmh is run instead of the target
continuation function.

@deftogether[
  (@defparam[current-continuation-key-cookie-path path path-string?
             #:value "/"]
   @defparam[current-continuation-key-cookie-secure? secure? boolean?
             #:value #t])]{

  These parameters control the @emph{Path=} attribute and the
  @emph{Secure=} attribute, respectively, of the continuation key
  cookie.
}

@defparam[current-continuation-mismatch-handler handler (-> request? response?)]{
  This parameter holds the handler that is run whenever a continuation
  is called with an invalid continuation key in the request.  The
  default implementation short-circuits the request and redirects the
  user to the base path for that continuation, skipping its handler.

  If you install a custom mismatch handler, you must avoid modifying
  the user session within it.  The session that will get modified is
  the session of the original user, not of the "attacker", which is
  probably not what you want.
}

@defparam[current-continuation-wrapper wrapper (-> (-> request? response?)
                                                   (-> request? response?))]{
  Since continuations are executed outside of the standard request -
  response lifecycle, this means that any middleware you set up on
  your dispatchers will not wrap your continuation handlers.  To get
  around this, you have to register your middleware stack with this
  parameter.
}

@defproc[((wrap-protect-continuations [handler (-> request? response?)]) [req request?]) response?]{
  This function sets up the continuation key and updates all responses
  that pass through it to include the continuation key cookie.

  Without this, all protected continuations will fail out so don't
  forget to add it to your middleware stack.

  Middleware that themselves wrap @racket[wrap-protect-continuations]
  will wrap the mismatch handler.  This means you have to be careful
  not to wrap the middleware inside other middleware that modify the
  user session.  Nor should your mismatch handler modify the session,
  because the session it modifies will be the one of the original
  user, not the "attacker."
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
