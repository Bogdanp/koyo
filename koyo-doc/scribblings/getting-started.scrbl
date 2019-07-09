#lang scribble/doc

@(require "koyo.rkt")

@title[#:tag "getting-started"]{Getting Started}

@(define honcho (hyperlink "https://honcho.readthedocs.io/en/latest/" "honcho"))

koyo comes with a command line utility which you can use to get
started quickly.

Running @exec{raco koyo new <project-name>} from the command line will
get you set up with a basic web application.  From there, you should
follow the instructions in the generated @filepath{README.md} file and
take a look at the generated source code.

You can then use @exec{raco koyo serve} to run your application,
although, generally, it's more convenient to use @honcho (which iself
runs @exec{raco koyo serve} based on the configuration in
@filepath{Procfile.dev}) in order to be able to both compile your
assets and run the web server from the same terminal session.

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
        (list @filepath{Brocfile.js}    "The asset pipeline (Broccoli) configuration file.  Describes how (and which) assets are to be preprocessed before being copied into the static/ folder.")
        (list @filepath{Procfile.dev}   "The process configuration file.  Used by honcho to determine which processes to run when it starts a development server.  By default, it runs a process to build the assets on change and the application web server.")
        (list @filepath{package.json}   "Describes the node dependencies for the asset pipeline.")
        (list @filepath{requirements.*} "Describes the python development dependencies, such as the process runner (honcho)."))]

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
        (list @filepath{dynamic.rkt}  "The mai entrypoint for the application.  Everything gets wired together in this file.")
        (list @filepath{info.rkt}     "Describes the package that makes up your application and its dependencies."))]
