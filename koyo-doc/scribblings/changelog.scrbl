#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     racket/string
                     web-server/http)
          "koyo.rkt")

@title[#:tag "changelog"]{Changelog}

@section{v0.8.0}
@subsection{Added}
@itemlist[
  @item{@racket[mailer-send-email] and @racket[mail-adapter-send-email].}
]

@subsection{Fixed}
@itemlist[
  @item{The database component no longer forcefully shuts down its
  associated custodian when the component is stopped.}
  @item{There is now a lower bound on @tt{crypto-lib} for version 1.6
  to ensure that shared libraries (eg. for libargon2) correctly get
  included in distributions (using @tt{koyo dist} or @tt{raco
  distribute}).}
]
