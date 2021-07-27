#lang scribble/doc

@(require (for-label koyo/console
                     racket/base)
          "koyo.rkt")

@title[#:tag "console"]{Console}

@defmodule[koyo/console]

@defproc[(start-console-here) void?]{
  Searches for the ``dynamic.rkt'' module in the current project
  starting from @racket[current-directory], then loads it and starts
  its system minus its @racket['server] component.  The search stops
  after looking through at most 10 directories or if a ``.git'' folder
  is found before a ``dynamic.rkt'' module.  This is handy when you
  have an existing REPL session (eg. while working on a module in
  Emacs) and you want to bring in the functionality of @tt{raco koyo
  console} into that session.

  Requiring @racketmodname[(submod koyo/console dev)] is a shorthand
  for requiring this function and then calling it.
}
