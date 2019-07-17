# koyo

koyo is a web application development toolkit that expands upon
Racket's built-in web-server with all the functionality that a web app
typically needs in a complete package.

All of koyo's components are decoupled so you get to pick and choose
what you use.

## Quickstart

    $ raco pkg install koyo
    $ raco koyo new example  # or raco koyo new -b minimal example
    $ cd example && cat README.md

## Documentation

You can find the documentation on the [Racket package server][docs].
The package server only updates once every 24 hours so if you want the
absolute most recent docs, then you can visit [koyo.defn.io][docs-master].

## License

    koyo is licensed under the 3-Clause BSD license.


[docs]: https://docs.racket-lang.org/koyo@koyo-doc/index.html
[docs-master]: https://koyo.defn.io
