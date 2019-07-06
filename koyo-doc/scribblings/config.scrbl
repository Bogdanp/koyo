#lang scribble/doc

@(require "koyo.rkt")

@title[#:tag "config"]{Configuration}

@defmodule[koyo/config]

This module provides the @racket[define-option] macro which
facilitates reading environment variables, transforming them and
providing them as Racket values.

@defform[
  (define-option name maybe-default e ...)
  #:grammar
  [(maybe-default (code:line)
                  (code:line #:default default))]]{

  Defines and provides an option called @racket[name].  The name is
  uppercased, prefixed with @racket[current-option-name-prefix] and
  has its dashes replaced with underscores; the resulting value is
  then passed to @racket[getenv].  If the environment variable is not
  set, then @racket[default] is used and if @racket[default] isn't
  provided, then the value will be @racket[#f].

  @racket[name] is bound inside the body of the define so options can
  be pre-processed.  For example:

  @racketblock[
    (define-option debug
      (equal? debug "x"))

    (define-option concurrency
      #:default "4"
      (string->number concurrency))
  ]
}

@defparam[
  current-option-name-prefix prefix string?
  #:value "KOYO"]{

  Defines the prefix for option names as environment variables.
}
