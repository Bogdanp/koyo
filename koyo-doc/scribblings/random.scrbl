#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     racket/string)
          "koyo.rkt")

@title[#:tag "random"]{Random}

@defmodule[koyo/random]

This module contains utilities for generating cryptographically-secure
random strings.

@deftogether[
  (@defparam[current-random-string-generator generator (-> exact-positive-integer? non-empty-string?)
             #:value generate-random-string]
   @defproc[(generate-random-string [len (exact-nonnegative-integer?) 64]) non-empty-string?])]{
  Generates a random string of length @racket[len] in a
  cryptographically-secure manner.
}
