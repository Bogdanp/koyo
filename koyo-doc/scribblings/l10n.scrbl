#lang scribble/doc

@(require (for-label koyo
                     racket/base
                     racket/contract
                     srfi/29
                     web-server/http
                     web-server/servlet)
          "koyo.rkt")

@title[#:tag "l10n"]{Localization}

@defmodule[koyo/l10n]

This module provides a middleware and facilities for localizing
messages.

@defparam[current-locale-specifier spec symbol?]{
  A parameter that holds the locale specifier for the current
  application.  This value must be a symbol that uniquely identifies
  the current application among the set of installed packages.
}

@defparam[current-locales locales (listof (cons/c symbol? symbol?))]{
  A parameter that holds the current set of locales that translations
  exist for.
}

@defproc[(load-locales! [path path-string?]) void?]{
  Traverses the directory at @racket[path] to read the translation
  files within it.

  The directory must have one subdirectory per language and each
  language subdirectory must contain one translation file per region.

  For example, for English, one might define the following structure:

  @itemlist[
    @item[@filepath{resources/locales/en/}]
    @item[@filepath{resources/locales/en/uk.rktd}]
    @item[@filepath{resources/locales/en/us.rktd}]
  ]

  Each translation file must contain a list of pairs where the first
  element is a symbol and the second is the translation format string
  for that symbol.  For example:

  @racketblock[
  ((title    . "Bogdan's Blog")
   (subtitle . "Just another one of these things")
   (hello    . "Hi, ~a!"))
  ]
}

@defproc[(translate [message-name symbol?]
                    [arg any/c] ...) string?]{

  Looks up the translation format string named @racket[message-name]
  for the @racket[current-language], @racket[current-country] and
  @racket[current-locale-specifier] and proceeds to format it using
  the specified @racket[arg]s.

  If there is no translation named @racket[message-name] then
  @racket[message-name] is converted to a string and the @racket[arg]s
  are ignored.

  @racketblock[
  (translate 'title)
  (translate 'hello "Jim")
  ]
}

@defproc[((wrap-browser-locale [sessions session-manager?]) [handler (-> request? can-be-response?)]) (-> request? can-be-response?)]{
  A middleware that parameterizes the @racket[current-language],
  @racket[current-country] and @racket[current-locale] based on the
  @tt{Accept-Language} header.  If the value of the header refers to a
  locale that isn't in the list of @racket[current-locales], then it
  falls back to @tt{en-US}.

  If @racket['l10n.lang] is present in the current user's session then
  that value takes precedence over the value of the
  @tt{Accept-Language} header.
}
