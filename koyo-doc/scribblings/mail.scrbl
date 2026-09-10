#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     racket/string
                     web-server/http)
          "koyo.rkt")

@title[#:tag "mail"]{Mail}

@defmodule[koyo/mail]

This module provides functionality for sending e-mail.

@section{Mailer}

The mailer is a component that wraps a @racket[mail-adapter?]  along
with a default value for the sender and common variables for some of
the template model properties.

@defproc[(make-mailer-factory
          [#:adapter adapter mail-adapter?]
          [#:sender sender non-empty-string?]
          [#:common-variables common-variables (hash/c symbol? string?)]) (-> mailer?)]{
 Returns a function that creates a new @racket[mailer?] when called.
}

@defproc[(mailer? [v any/c]) boolean?]{
 Returns @racket[#t] when @racket[v] is a mailer.
}

@defproc[(mailer-sender [m mailer?]) non-empty-string?]{
 Returns the default sender e-mail address the mailer was created with.
}

@defproc[(mailer-send-email
          [m mailer?]
          [#:to to non-empty-string?]
          [#:from from non-empty-string? (mailer-sender m)]
          [#:subject subject non-empty-string?]
          [#:text-content text-content (or/c #f string?) #f]
          [#:html-content html-content (or/c #f string?) #f]) void?]{

 Sends an e-mail using the underlying @tech{mail adapter}.

 @history[#:added "0.8"]
}

@defproc[(mailer-send-email-with-template
          [m mailer?]
          [#:to to non-empty-string?]
          [#:from from non-empty-string? (mailer-sender m)]
          [#:template-id template-id (or/c #f exact-positive-integer?) #f]
          [#:template-alias template-alias (or/c #f symbol?) #f]
          [#:template-model template-model (hash/c symbol? string?) (hasheq)]) void?]{

 Sends a templated e-mail using the underlying @tech{mail adapter}.

 The @racket[#:template-model] hash is merged with the
 @racket[#:common-variables] the mailer was created with prior to
 being passed to @racket[mail-adapter-send-email-with-template].
}

@defproc[(mailer-send-email-with-template+extras
          [m mailer?]
          [extras any/c]
          [#:to to non-empty-string?]
          [#:from from non-empty-string? (mailer-sender m)]
          [#:template-id template-id (or/c #f exact-positive-integer?) #f]
          [#:template-alias template-alias (or/c #f symbol?) #f]
          [#:template-model template-model (hash/c symbol? string?) (hasheq)]) void?]{

 Like @racket[mailer-send-email-with-template], but accepts an opaque
 @racket[extras] argument that may be used to configure sending for a
 specific provider.

 @history[#:added "0.51"]
}

@section{Adapters}

@deftech{Mail adapters} expose a consistent interface for values that
can send e-mail.

@deftogether[(
  @defidform[#:kind "interface" gen:mail-adapter]
  @defproc[(mail-adapter? [v any/c]) boolean?]
  @defproc[(mail-adapter-send-email
            [adapter mail-adapter?]
            [#:to to non-empty-string?]
            [#:from from non-empty-string?]
            [#:subject subject non-empty-string?]
            [#:text-content text-content (or/c #f string?) #f]
            [#:html-content html-content (or/c #f string?) #f]) void?]
  @defproc[(mail-adapter-send-email-with-template
            [adapter mail-adapter?]
            [#:to to non-empty-string?]
            [#:from from non-empty-string?]
            [#:template-id template-id (or/c exact-positive-integer?) #f]
            [#:template-alias template-alias (or/c symbol?) #f]
            [#:template-model template-model (hash/c symbol? string?)]) void?]
  @defproc[(mail-adapter-send-email-with-template+extras
            [adapter mail-adapter?]
            [extras any/c]
            [#:to to non-empty-string?]
            [#:from from non-empty-string?]
            [#:template-id template-id (or/c exact-positive-integer?) #f]
            [#:template-alias template-alias (or/c symbol?) #f]
            [#:template-model template-model (hash/c symbol? string?)]) void?]
  )]{

 The generic interface for @tech{mail adapters}.  Adapters must
 implement at least one of these methods, but they don't have to
 implement all of them.

 The @racket[#:template-id] and @racket[#:template-alias] arguments
 to @racket[mail-adapter-send-email-with-template] are mutually
 exclusive.

 @history[#:changed "0.51" @elem{Added @racket[mail-adapter-send-email-with-template+extras].}]
}

@deftogether[(
  @defproc[(make-stub-mail-adapter) mail-adapter?]
  @defproc[(stub-mail-adapter? [v any/c]) boolean?]
  @defproc[(stub-mail-adapter-outbox [adapter stub-mail-adapter?]) (listof hash?)]
)]{

 A stub @tech{mail adapter}.  All emails are stored in a list called
 the outbox.  The current contents of the outbox can be retrieved
 using @racket[stub-mail-adapter-outbox].
}


@subsection{Other Adapters}

@(define koyo-postmark-url "https://pkgd.racket-lang.org/pkgn/package/koyo-postmark")
@(define postmark-url "https://postmarkapp.com")

@link[koyo-postmark-url]{koyo-postmark} provides a @tech{mail adapter}
for interfacing with the @link[postmark-url]{Postmark} API.
