#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     xml)
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
language (like jade or pug), then @racket[haml] will feel familiar to
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

Identifiers that start with an @tt{&} character are converted to
symbols with the initial @tt{&} removed:

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

Tag names are optional if a class name is provided, in which case the
tag defaults to @tt{div}:

@examples[
  #:eval sandbox
  #:label #f
  (haml
   (.content
    (:h1.title "Hello World")))
]

Repeated attributes are concatenated into a single value:

@examples[
  #:eval sandbox
  #:label #f
  (haml
   (.content
    (:h1.title
     ([:class "hello"]
      [:data-example "1"]
      [:data-example "2"])
     "Hello World")))
]

Lists of elements can be spliced in using the @racket[(unquote-splicing e)] syntax:

@haml-splicing-syntax-example

Expressions that don't parse as elements are evaluated in place at
runtime:

@examples[
  #:eval sandbox
  #:label #f
  (define (say-hi name)
    (format "Hi, ~a!" name))

  (haml
   (:h1 (say-hi "Bogdan")))
]

The @racket[when] and @racket[unless] forms are special-cased so that
they automatically splice their result, if any, into the enclosing
expression:

@examples[
  #:eval sandbox
  #:label #f
  (for ([v '(#t #f)])
    (println
     (haml
      (:h1 (when v "a") "title"))))
]

Passing multiple elements to the @racket[haml] macro produces a list
of @racket[xexpr?]s:

@examples[
  #:eval sandbox
  #:label #f
  (haml
   (:li "a")
   (:li "b"))
]

@history[
  #:changed "0.17" @elem{
    Element attributes may now begin with an @tt{@"@"} symbol.
  }
  #:changed "0.10" @elem{
    @racket[when], @racket[unless] and @racket[unquote-splicing] are
    now recognized by binding.  The @"@"-style splicing syntax is no
    longer supported.
  }
]

@section[#:tag "haml-templates"]{Templates}
@defmodule[koyo/haml-template]

@deftech{HAML templates} let you define templatized versions of the
@racket[haml] form.

@defform[
  #:literals (slot)
  (define-haml-template id element)
  #:grammar ([element
              (code:line slot)
              (code:line element)]
             [slot
              (slot keyword maybe-default)
              (slot keyword)
              (slot)])
]{

  Defines a @tech{HAML template} named @racket[id] that expands to
  @racket[(haml element)] on use. The @racket[element] syntax is the
  same as for @racket[haml], but extended with a @racket[(slot)] form
  that indicates where the contents of the template should go.

  @examples[
    #:eval sandbox
    #:label #f
    (require racket/pretty)
    (define-haml-template nav-item
      (:li.nav-item
       (:a
        ([:href (slot #:destination "/")])
        (slot))))
    (define-haml-template container
      (.container
       (slot)))
    (pretty-print
     (container
      (:ul.nav
       (nav-item "Home")
       (nav-item #:destination "/me" "My Account"))
      (:strong "child 1")
      (:strong "child 2")))
  ]

  @history[
   #:changed "0.47" @elem{Added support for named slots.}
   #:added "0.22"
  ]
}

@defform[(slot)]{
  Within a @tech{HAML template}, determines where the content of the
  template will go. Raises a syntax error when used outside of a
  @racket[define-haml-template] form.
}
