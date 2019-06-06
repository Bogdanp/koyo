#lang scribble/manual

@(require "koyo.rkt")

@title{koyo - web app development toolkit}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

@defmodule[koyo]

@section[#:tag "intro"]{Introduction}

@(define web-server-docs
   '(lib "web-server/scribblings/web-server.scrbl"))

@(define component-docs
   '(lib "component/component.scrbl"))

koyo is a web application development toolkit that expands upon
Racket's built-in @other-doc[web-server-docs] with all the
functionality that a web app typically needs in a complete package.

All of koyo's components are decoupled so you get to pick and choose
what you use.

@local-table-of-contents[]

@include-section["haml.scrbl"]

@index-section[]
