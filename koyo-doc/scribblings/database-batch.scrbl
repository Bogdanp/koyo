#lang scribble/doc

@(require (for-label db
                     koyo/database
                     koyo/database/batch
                     racket/base
                     racket/contract/base
                     racket/string)
          "koyo.rkt")

@title[#:tag "database-batch"]{Batch Inserts}
@defmodule[koyo/database/batch]

This module provides functionality for batching up database inserts.
An @deftech{insert batcher} is an object that buffers rows in memory
in order to insert them into the database using as few roundtrips as
possible. Insert batchers are not thread safe.

@defproc[(insert-batcher? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is an @tech{insert batcher}.

  @history[#:added "0.30"]
}

@defproc[(make-insert-batcher [table symbol?]
                              [columns (listof (list/c symbol? string?))]
                              [#:dialect dialect (or/c 'postgresql) 'postgresql]
                              [#:batch-size batch-size exact-positive-integer? 10000]
                              [#:on-conflict on-conflict (or/c 'error
                                                               (list/c 'do-nothing (listof symbol?))
                                                               (list/c 'update (listof symbol?)
                                                                               (listof (list/c symbol? string?)))) 'error])
         insert-batcher?]{
  @margin-note{@bold{Warning:} The table name, column names and column
  types are interpolated into the insert queries. Avoid blindly passing
  in user input for these values as doing so could lead to SQL injection
  vulnerabilities.}

  Creates an @tech{insert batcher} for the given @racket[table] and
  @racket[columns]. The @racket[columns] must be a list of pairs of
  column names and column types, where the types are represented as
  strings for the target dbsystem. None of the columns may, themselves,
  be arrays.

  The @racket[#:dialect] controls how data is inserted into the target
  database. For example, when the dialect is @racket['postgresql], the
  data is passed to the database using one @racket[pg-array] per column.

  The @racket[#:batch-size] argument controls the maximum number of
  rows that are buffered in memory before they are flushed.

  The @racket[#:on-conflict] argument determines how conflicts are
  handled in the target dbsystem. The @racket['error] behavior causes
  inserts to fail with a constraint violation on conflict. The
  @racket['do-nothing] behavior ignores conflicts for rows that have a
  conflict for the given set of columns. The @racket['update] behavior
  updates the second set of columns on conflict with the first set. For
  an update, the second set of columns is a list of column, expression
  pairs.

  @racketblock[
    (define ib
      (make-insert-batcher
       #:on-conflict '(do-nothing (ticker))
       'tickers
       '([isin "TEXT"]
         [ticker "TEXT"]
         [added_at "TIMESTAMPTZ"])))
    (with-database-connection [conn db]
      (for ([(isin ticker added-at) (in-sequence datasource)])
        (ib-push! ib conn isin ticker added-at))
      (ib-flush! ib conn))
  ]

  @history[
   #:added "0.30"
   #:changed "0.38" @elem{Added the @racket['update] conflict behavior.}
  ]
}

@defproc[(ib-push! [ib insert-batcher?]
                   [conn connection?]
                   [col-value any/c] ...+) void?]{
  Add the given column values to @racket[ib], flushing any pending data
  to the database via @racket[conn] if necessary.

  @history[#:added "0.30"]
}

@defproc[(ib-flush! [ib insert-batcher?]
                    [conn connection?]) void?]{
  Write any pending data in @racket[ib] to the database using
  @racket[conn].

  @history[#:added "0.30"]
}
