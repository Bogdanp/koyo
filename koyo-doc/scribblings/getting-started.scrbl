#lang scribble/doc

@(require (for-label component
                     koyo
                     koyo/console)
          "koyo.rkt")

@title[#:tag "getting-started"]{Getting Started}

@(define chief (hyperlink "https://github.com/Bogdanp/racket-chief" "chief"))

koyo comes with a command line utility which you can use to get
started quickly.

Running @exec{raco koyo new <project-name>} from the command line will
get you set up with a basic web application.  From there, you should
follow the instructions in the generated @filepath{README.md} file and
take a look at the generated source code.

You can then use @exec{raco koyo serve} to run your application,
although, generally, it's more convenient to use @chief (which iself
runs @exec{raco koyo serve} based on the configuration in
@filepath{Procfile}) in order to be able to both compile your assets
and run the web server from a single terminal session.


@section{Project Structure}

Running @exec{raco koyo new example} generates the following project
structure in the current directory:

@tabular[
  #:sep @hspace[2]
  #:style 'boxed
  #:row-properties '(bottom-border ())
  (list (list @bold{Folder}              @bold{Description})
        (list @filepath{example/}        "Contains all the application logic.")
        (list @filepath{example-tests/}  "Contains tests for all of the distinct components in the application.")
        (list @filepath{migrations/}     "Database schema migrations go in here.")
        (list @filepath{resources/}      "Various \"data\" files go in here, including translations, stylesheets, images and javascripts.")
        (list @filepath{static/}         "Assets compiled by the asset pipeline go in here.  Not committed to source control."))]

@tabular[
  #:sep @hspace[2]
  #:style 'boxed
  #:row-properties '(bottom-border ())
  (list (list @bold{File}               @bold{Description})
        (list @filepath{.env.default}   "A list of default environment variables for your application.  Generally committed to source control.")
        (list @filepath{.nvmrc}         "Contains the node version required to work on your application.  This is used by nvm to set up the environment appropriately for that respective version.")
        (list @filepath{build.mjs}      "The bundler (esbuild) script.  Describes how (and which) assets are to be preprocessed before being copied into the static/ folder.")
        (list @filepath{Procfile}       "The process configuration file.  Used by chief to determine which processes to run when it starts a development server.  By default, it runs a process to build the assets on change and the application web server.")
        (list @filepath{package.json}   "Describes the node dependencies for the asset pipeline."))]

The application folder is further subdivided into:

@tabular[
  #:sep @hspace[2]
  #:style 'boxed
  #:row-properties '(bottom-border ())
  (list (list @bold{Folder}           @bold{Description})
        (list @filepath{pages/}       "Contains the implementation for every page in your application (the presentation).")
        (list @filepath{components/}  "Contains the implementation for every bit of business logic in your application."))]

@tabular[
  #:sep @hspace[2]
  #:style 'boxed
  #:row-properties '(bottom-border ())
  (list (list @bold{File}             @bold{Description})
        (list @filepath{config.rkt}   "Contains all of the application's configuration options.")
        (list @filepath{dynamic.rkt}  "The main entrypoint for the application.  Everything gets wired together in this file.")
        (list @filepath{info.rkt}     "Describes the package that makes up your application and its dependencies."))]


@section{Interactive Console}

Running @exec{raco koyo console} from within the root of a koyo
application starts an interactive REPL with that application's
components loaded into scope.

Components are accessible via @racket[system-ref]. For example, if
you have a component named @tt{database} then you can access it from
the console using @racket[(system-ref 'database)]. Components are
also accessible at the top level using ids prefixed with the @tt|{@}|
character (eg. @racket[|@database|]).

The @racket[start] and @racket[stop] procedures can be used to start
and, respectively, stop the system from the console. The @racket[start]
procedure is implicitly called at the beginning of every console
session.

See @racket[start-console-here] for information on loading the console
functionality into an existing REPL session.

@history[#:changed "0.33" @elem{Added a custom @racket[#%top] form to
convert unbound variable references prefixed with @tt|{@}| to component
lookups.}]
