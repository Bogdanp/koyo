# AppNameHere

## Setup

### Requirements

* You need [Racket] since this is a Racket application.
* You need access to a couple local [Postgres] databases.  One named
  `app_name_here` and the other `app_name_here_tests`.  The latter is
  exercised by unit tests.

### First-time Setup

    $ raco pkg install app-name-here/                                    # install and build the application and its deps
    $ raco north migrate -f -u postgres://127.0.0.1/app_name_here        # migrate the local database
    $ raco north migrate -f -u postgres://127.0.0.1/app_name_here_tests  # migrate the tests database

## Running the app locally

    $ raco koyo serve


[Postgres]: https://www.postgresql.org/
[Racket]: https://racket-lang.org/
