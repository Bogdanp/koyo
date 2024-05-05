# AppNameHere

## Setup

### Requirements

* You need [Racket] since this is a Racket application.
* You need [Node.js] version 18 or higher to build the assets.
* You need access to a couple local [Postgres] databases.  One named
  `app_name_here` and the other `app_name_here_tests`.  The latter is
  exercised by unit tests.

### First-time Setup

    npm install && npm run build
    raco pkg install chief
    raco pkg install app-name-here/        # install and build the application and its deps
    raco pkg install app-name-here-tests/  # install and build the tests and their deps

### Development environment

Copy `.env.default` to `.env`.  [chief] will automatically load the
variables defined in this file into the environment of the
subprocesses defined in the `Procfile` whenever it is run.

The app expects to be run behind an SSL terminated connection (for
example, behind an nginx instance using a self-signed cert), even for
local development .  You can disable this requirement by setting
`current-continuation-key-cookie-secure?` parameter to `#f` before the
application is started.

## Running the app locally

    raco chief start


[Postgres]: https://www.postgresql.org/
[Racket]: https://racket-lang.org/
[Node.js]: https://nodejs.org/en/
[argon2]: https://www.argon2.com/
[chief]: https://github.com/Bogdanp/racket-chief
