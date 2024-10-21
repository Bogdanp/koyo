#lang scribble/doc

@(require (for-label db
                     koyo
                     json
                     racket/base
                     racket/contract
                     web-server/http)
          "koyo.rkt")

@title[#:tag "database"]{Database}

@defmodule[koyo/database]

This module provides a database component and functionality for
working with database connections.

@defproc[(database? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a database component.
}

@defproc[(make-database-factory [connector (-> connection?)]
                                [#:log-statements? log-statements? boolean? #f]
                                [#:max-connections max-connections exact-positive-integer? 16]
                                [#:max-idle-connections max-idle-connections exact-positive-integer? 2]) (-> database?)]{

  Returns a function that will create a database component containing a
  DB connection pool of size @racket[#:max-connections] which connects
  to the database using the @racket[connector].

  When the @racket[#:log-statements?] argument is @racket[#t],
  statements are logged to the @racket['koyo:db-statements] topic. The
  data part of the log message includes the thread the statement was
  executed from, the name of the procedure that executed the query and
  the statement itself.

  @history[
    #:changed "0.8" @elem{The component no longer forcefully shuts
     down its associated custodian when the component is stopped. There
     is now a lower bound on @tt{crypto-lib} for version 1.6 to ensure
     that shared libraries (eg. for libargon2) correctly get included in
     distributions (using @tt{koyo dist} or @tt{raco distribute}).}
    #:changed "0.20" @elem{Addded the @racket[#:log-statements?]
     argument.}]
}

@defproc[(call-with-database-connection [database database?]
                                        [proc (-> connection? any)]) any]{

  Retrieves a database connection from the pool and passes it to
  @racket[proc]. Once @racket[proc] completes, the connection is
  returned to the pool.

  Nested calls to @racket[call-with-database-connection] reuse the
  same connection.
}

@defproc[(call-with-database-transaction [database database?]
                                         [proc (-> connection? any)]
                                         [#:isolation isolation (or/c #f
                                                                      'serializable
                                                                      'repeatable-read
                                                                      'read-committed
                                                                      'read-uncommitted) #f]) any]{

  Retrieves a database connection from the pool, enters a transaction
  with the requested @racket[#:isolation] level and passes the
  connection to @racket[proc].  If @racket[proc] completes
  successfully, the transaction is committed, otherwise it is rolled
  back.

  Nested calls to @racket[call-with-database-transaction] reuse the
  same connection and, if the database supports it, create nested
  transactions.
}

@deftogether[(
  @defform[
    (with-database-connection [id database]
      e ...+)
    #:contracts ([database database?])]

  @defform[
    (with-database-transaction [id database]
      maybe-isolation
      e ...+)
    #:grammar ([maybe-isolation (code:line)
                                (code:line #:isolation isolation)])
    #:contracts ([database database?]
                 [isolation (or/c #f
                                  'serializable
                                  'repeatable-read
                                  'read-committed
                                  'read-uncommitted)])]
)]{

  These forms are syntactic sugar for calling @racket[call-with-database-connection]
  and @racket[call-with-database-transaction], respectively, with an
  anonymous thunk.

  For example, the following forms are equivalent:

  @racketblock[
    (with-database-connection [c the-db]
      (query-value c "select 42"))
  ]

  @racketblock[
    (call-with-database-connection the-db
      (lambda (c)
        (query-value c "select 42")))
  ]

}

@defthing[id/c contract?]{
  An alias for @racket[exact-positive-integer?].
}

@defthing[maybe-id/c contract?]{
  An alias for @racket[(or/c #f id/c)].
}

@defproc[(in-rows [conn connection?]
                  [stmt statement?]
                  [arg any/c] ...) sequence?]{

  Like @racket[in-query], but every column is pre-processed to convert
  null values to @racket[#f], arrays to lists, dates to Gregor dates,
  and timestamp to Gregor moments.
}

@defproc[(in-row [conn connection?]
                 [stmt statement?]
                 [arg any/c] ...) sequence?]{

  Like @racket[in-row], but stops iterating after the first result.
}

@defform[(if-null test-expr fallback-expr)]{
  Returns @racket[fallback-expr] if @racket[test-expr] is either
  @racket[(json-null)] or @racket[sql-null].

  @history[#:added "0.27"]
}

@defproc[(null-if [v V]
                  [e any/c]) (or/c sql-null V)]{

  Returns @racket[sql-null] if @racket[v] is @racket[equal?] to
  @racket[e]. Otherwise, returns @racket[v].

  @history[#:added "0.27"]
}

@include-section["database-url.scrbl"]
