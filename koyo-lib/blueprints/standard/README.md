# AppNameHere

## Setup

### Requirements

* You need [Racket] since this is a Racket application.
* You need [node] and [nvm] to build the assets.
* You need [honcho] to run the local development server.  This means
  you also need a relatively recent version of [Python].
* You need access to a couple local [Postgres] databases.  One named
  `app_name_here` and the other `app_name_here_tests`.  The latter is
  exercised by unit tests.
* Finally, you need to have an [argon2] shared library installed.  On
  macOS, you can install it with `brew install argon2`.  This is used
  to securely hash passwords.

### First-time Setup

    $ nvm use && npm install
    $ pip install -r requirements.txt
    $ raco pkg install app-name-here/                                    # install and build the application and its deps
    $ raco north migrate -f -u postgres://127.0.0.1/app_name_here        # migrate the local database
    $ raco north migrate -f -u postgres://127.0.0.1/app_name_here_tests  # migrate the tests database

### Development environment

Copy `.env.default` to `.env`.  [honcho] will automatically load the
variables defined in this file into the environment of the
subprocesses defined in the `Procfile` whenever it is run.

The app expects to be run behind an SSL terminated connection (for
example, behind an nginx instance using a self-signed cert), even for
local development .  You can disable this requirement by setting
`current-continuation-key-cookie-secure?` parameter to `#f` before the
application is started.

## Running the app locally

    $ nvm use
    $ honcho -f Procfile.dev


[Postgres]: https://www.postgresql.org/
[Python]: https://python.org/
[Racket]: https://racket-lang.org/
[argon2]: https://www.argon2.com/
[honcho]: https://pypi.org/project/honcho/
[node]: https://nodejs.org/en/
[nvm]: https://github.com/nvm-sh/nvm
