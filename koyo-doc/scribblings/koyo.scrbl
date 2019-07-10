#lang scribble/manual

@(require "koyo.rkt")

@title{koyo - web app development toolkit}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

@defmodule[koyo]

@(define web-server-link
   (seclink "top"
            #:doc '(lib "web-server/scribblings/web-server.scrbl")
            #:indirect? #t
            "Racket's built-in web-server"))

koyo is a web application development toolkit that expands upon
@web-server-link with all the functionality that a web app typically
needs in a complete package.

All of koyo's components are decoupled so you get to pick and choose
exactly which parts you want to use.

@local-table-of-contents[]

@include-section["getting-started.scrbl"]
@include-section["architecture.scrbl"]
@include-section["cors.scrbl"]
@include-section["config.scrbl"]
@include-section["continuations.scrbl"]
@include-section["csrf.scrbl"]
@include-section["haml.scrbl"]
@include-section["http.scrbl"]
@include-section["logging.scrbl"]
@include-section["mime.scrbl"]
@include-section["random.scrbl"]
@include-section["server.scrbl"]

@index-section[]
