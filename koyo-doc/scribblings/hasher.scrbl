#lang scribble/doc

@(require (for-label component
                     koyo
                     racket/base
                     racket/contract
                     racket/future)
          "koyo.rkt")

@title[#:tag "password-hashing"]{Password Hashing}

@defmodule[koyo/hasher]

This module provides functionality for creating and verifying
@deftech{password hashes}.


@section{Hasher Interface}

@defidform[#:kind "interface" gen:hasher]{
  The generic interface for password hashers.  Every hasher is also a
  @racket[gen:component].
}

@defproc[(hasher? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] implements the password hasher
  interface.
}

@defproc[(hasher-make-hash [h hasher?]
                           [pass string?]) string?]{

  Hashes @racket[pass] in an implementation-specific way.
}

@defproc[(hasher-hash-matches? [h hasher?]
                               [pass-hash string?]
                               [pass string?]) boolean?]{

  Returns @racket[#t] when @racket[pass-hash] is a hash of @racket[pass].
}


@section{Implementations}

@subsection{@tt{argon2id}}

@(define argon2id-anchor
  (link "https://en.wikipedia.org/wiki/Argon2" (tt "argon2") " KDF"))

This hasher is based on the hybrid variant @|argon2id-anchor|.  It is
the recommended hasher for koyo applications and it's what you
currently get when you create an application using the standard
blueprint.

@defproc[(argon2id-hasher? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is an @tt{argon2id} hasher.
}

@defproc[(make-argon2id-hasher-factory [#:parallelism parallelism (processor-count)]
                                       [#:iterations iterations 256]
                                       [#:memory memory 2048]) (-> argon2id-hasher?)]{

  Returns an @tt{argon2id}-based hasher with the given configuration.
  It's strongly recommended that you pick config values suitable to
  your own environment.

  @examples[
  #:label #f
  #:eval sandbox
  (define the-hasher
   (component-start ((make-argon2id-hasher-factory))))

  (code:line)
  (define p "supersecret")
  (define h (hasher-make-hash the-hasher p))
  h

  (code:line)
  (hasher-hash-matches? the-hasher h "nope")
  (hasher-hash-matches? the-hasher h p)
  ]
}
