#lang scribble/doc

@(require (for-label component
                     db
                     (only-in gregor +minutes moment? now/moment)
                     koyo
                     racket/base
                     racket/contract
                     web-server/dispatchers/dispatch
                     web-server/http
                     web-server/servlet-dispatch)
          racket/runtime-path
          racket/sandbox
          "koyo.rkt")

@title[#:tag "job"]{Job Queues}

@defmodule[koyo/job]

This module provides functionality for declaring and executing
asynchronous jobs.  The job queuing functionality is implemented on
top of PostgreSQL so you don't need an external message queue.  Jobs
are guaranteed to be executed at least once after being enqueued.

@(begin
   (define-syntax-rule (interaction e ...) (examples #:label #f e ...))
   (define-runtime-path log-file "job-log.rktd")
   (define log-mode (if (getenv "KOYO_JOB_RECORD") 'record 'replay))
   (define (make-pg-eval log-file)
     (make-log-based-eval log-file log-mode))
   (define db-eval (make-pg-eval log-file)))

@interaction[
#:hidden
#:eval db-eval
(require component db gregor koyo)
]

@interaction[
#:eval db-eval
(define-system example
 [broker (db) make-broker]
 [db (make-database-factory
      (lambda ()
        (postgresql-connect #:user "example"
                            #:database "example")))]
 [worker (broker) (make-worker-factory)])

(code:line)
(system-start example-system)
(current-broker (system-ref example-system 'broker))

(code:line)
(define executed? (make-semaphore))

(code:line)
(code:comment "Define a job:")
(define-job (say-hello name)
  (printf "hi ~a!~n" name)
  (semaphore-post executed?))

(code:line)
(code:comment "Enqueue a job:")
(say-hello "Bogdan")

(code:line)
(code:comment "Wait a few moments for the job to be dequeued and executed...")
(void (sync executed?))
]


@section{Jobs}

@defparam[execute-jobs-synchronously? sync? boolean? #:value #f]{
  A parameter that controls whether or not jobs should be executed
  synchronously when applied.  This comes in handy when you want to
  test jobs at the REPL.
}

@defproc[(job? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a job.
}

@defform[(define-job (id args)
           option ...
           body-e ...+)
         #:grammar ([args (code:line arg-id ...)
                          (code:line arg-id ... @#,racketparenfont{.} rest-id)]
                    [option (code:line #:queue queue-name)
                            (code:line #:priority priority)])
         #:contracts ([queue-name string?]
                      [priority exact-nonnegative-integer?])]{

  Binds a function to @racket[id] that enqueues an asynchronous job
  when applied.  The arguments passed to each job must be
  @racket[prop:serializable].  When the job is picked up by a worker,
  its arguments are deserialized and its body is executed.

  The @racket[queue-name] option controls which queue the jobs are
  enqueued to.  If not supplied, it defaults to @racket["default"].

  The @racket[priority] option controls what the priority of each job
  is within the queue.  Zero is the highest priority.  If not
  supplied, this option defaults to @racket[50].

  When a job is executed synchronously, its result is always
  @racket[#f], otherwise its result is the id of the job in the
  @tt{koyo_jobs} table.
}

@defform[(retry!)]{
  Immediately terminates the current job and re-enqueues it so that it
  may be executed again at a later time.  Using this form outside the
  body of a @racket[define-job] form is a syntax error.
}

@defform[(schedule-at when-expr job-expr)
         #:contracts ([when-expr moment?])]{

  Wraps @racket[job-expr] so that it will be executed some time after
  the timestamp represented by @racket[when-expr].

  @interaction[
  #:eval db-eval
  (schedule-at
   (+minutes (now/moment) 5)
   (say-hello "Bogdan"))
  ]
}


@section{Brokers}

@deftech{Job brokers} handle the details of storing and retrieving
jobs to and from the database.  Each broker permanently leases a
connection from the database pool to listen for notifications and then
other connections are leased and put back into the pool as needed.

@defparam[current-broker broker broker?]{
  Holds the current broker that is used to enqueue jobs.
}

@defproc[(broker? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @tech{job broker}.
}

@defproc[(make-broker [db database?]) broker?]{
  Creates a broker component that enqueues its jobs in @racket[db].
  The @racket[db] parameter must refer to a PostgreSQL database.
}


@section{Workers}

@deftech{Job workers} dequeue and execute jobs.

@defproc[(worker? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @tech{job worker}.
}

@defproc[((make-worker-factory [#:queue queue string? "default"]
                               [#:pool-size pool-size exact-positive-integer? 8]
                               [#:middleware middleware
                                             (-> job-metadata? procedure? procedure?)
                                             (lambda (_meta proc) proc)]) [broker broker?]) worker?]{

  Generates a function that, when supplied a @tech{job broker},
  produces a @tech{job worker} that dequeues and executes jobs
  published on the queue identified by @racket[queue].

  The @racket[#:pool-size] argument controls the maximum number of
  concurrent jobs for the worker.

  The @racket[#:middleware] argument wraps every job procedure before
  it is applied to its arguments.

  @history[
    #:changed "0.28" @elem{Added the @racket[#:middleware] argument.}
  ]
}

@defstruct[job-metadata ([id exact-nonnegative-integer?]
                         [queue string?]
                         [name string?]
                         [attempts exact-nonnegative-integer?])]{

  Information about a job that's about to be run. This struct may be
  extended in the future to contain additional information.

  @history[#:added "0.28"]
}


@subsection{Broker Admin UI}

@defproc[(make-broker-admin [broker broker?]
                            [middleware (-> procedure? (-> request? response?)) values]) dispatcher/c]{
  Returns a dispatcher that you can embed in your app in order to
  administer jobs. The @racket[middleware] argument is used to wrap
  the servlet before it is passed to @racket[dispatch/servlet]. Use
  @racket[dispatch/mount] to mount it under a specific path within your
  application.

  @history[#:added "0.28"]
}
