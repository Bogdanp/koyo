#lang scribble/doc

@(require (for-label koyo)
          "koyo.rkt")

@title[#:tag "haml"]{HAML}

@defmodule[koyo/haml]

@(define haml-link
   (hyperlink "http://haml.info" "haml"))
@(define hiccup-link
   (hyperlink "https://github.com/weavejester/hiccup" "hiccup"))

@racket[haml] is a convenience macro for generating xexprs with less
boilerplate.  Its syntax is inspired by @haml-link -- whence its name
-- but if you've used Clojure's @hiccup-link or any similar templating
language (like jade or pug), then @racket[haml] will seem familiar to
you.

@section[#:tag "haml-syntax"]{Syntax}

@haml-form

Literal numbers and strings are returned unmodified:

@examples[
  #:eval sandbox
  #:label #f
  (haml 1)
  (haml "hello")
]

Identifiers that start with an & symbol are returned with said symbol
stripped off:

@examples[
  #:eval sandbox
  #:label #f
  (haml &mdash)
]

Other identifiers are evaluated from the enclosing environment:

@examples[
  #:eval sandbox
  #:label #f
  (let ([a-symbol 'mdash])
    (haml a-symbol))
]

Element tags start with a colon:

@examples[
  #:eval sandbox
  #:label #f
  (haml
   (:h1 "Hello World"))
]

Id and class attributes can be attached to a tag via a shorthand syntax:

@examples[
  #:eval sandbox
  #:label #f
  (haml
   (:h1.title#main-title "Hello World"))
]

Tag names are optional if a class name is provided:

@examples[
  #:eval sandbox
  #:label #f
  (haml
   (.content
    (:h1.title "Hello World")))
]

Lists of elements can be spliced in using the @racket[(unquote-splicing e)] syntax:

@haml-splicing-syntax-example

All expressions that don't parse as an element are evaluated in place
at runtime.
