#lang scribble/doc

@(require (for-label db
                     koyo
                     racket/base
                     racket/contract
                     racket/string)
          "koyo.rkt")

@title[#:tag "database-url"]{Database URL}

@defmodule[koyo/database-url]

This module provides a function for parsing @tt{DATABASE_URL}-style
connection strings.

@defproc[(parse-database-url [s string?]) (values procedure?
                                                  (or/c #f non-empty-string?)
                                                  (or/c #f (integer-in 1 65535))
                                                  (or/c non-empty-string? 'memory 'temporary)
                                                  (or/c #f string?)
                                                  (or/c #f string?))]{

  Parses a 12 Factor App-style @tt{DATABASE_URL} into six values:

  @itemlist[
    @item{a function that can be used to create database connections}
    @item{the database server host or @racket[#f] if the scheme is @tt{sqlite}}
    @item{the database server port or @racket[#f] if the scheme is @tt{sqlite}}
    @item{the database name or a filename if the scheme is @tt{sqlite}}
    @item{the database username or @racket[#f]}
    @item{the database password or @racket[#f]}
  ]

  Examples:

  @examples[
    #:eval sandbox
    #:label #f
    (parse-database-url "sqlite3:///:memory:")
    (parse-database-url "sqlite3:///db.sqlite3")
    (parse-database-url "sqlite3:////path/to/db.sqlite3")
    (parse-database-url "postgres:///example")
    (parse-database-url "postgres://127.0.0.1:15432/example")
    (parse-database-url "postgres://user:password@127.0.0.1:15432/example")
  ]
}
