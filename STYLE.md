# ThomasEngine2 Style Guide

This is the style guide for code written in ThomasEngine2.

# Style Guidelines

## General

* Every file should begin (roughly) with the template shown
  in [Appendix 1][ax-1].

## Whitespace

* Always use spaces for indentation.
* Always use Unix-style line endings. Git enforces this.
* Delete all trailing whitespace before committing.
* Never go over 80 characters of width in a file.
* Try to horizontally align things to achieve visual coherence.
    * However, do not ever indent lines such that one cannot
      just select the whole file in Emacs and reindent.

## Syntax

* Bindings in `let`-style expressions should look like this:

    ```
    (let ([x 5]
          [y 3]
          [z 4])
      scope)
    ```

* Generally, whenever you have something that semantically
  represents a binding of some kind, you should use the same
  style — square brackets nested inside parentheses — as the
  `let` example above.


## Naming

* The length of a variable/function name should correspond to
  the size of its scope.
* The variables `i`, `j`, and `k` are generally reserved for
  loop counter variables.
* A two-letter variable name that ends in `s` should represent
  a sequence structure, i.e.: `xs` could be a list.
* Classes should end in a `%`.
* Explicit namespacing is almost always unnecessary, as it can
  easily be accomplished with the Guile module system.

## Documentation

* All functions and variables should be defined with documention.
    * Use the following macros to add docstrings to definitions:
        * `define-macro-with-docs`
        * `define-with-docs`
        * `define-generic-with-docs`
        * `define-class-with-docs`
    * These macros are from [`(scheme documentation)`][docstrings].
    * Docstrings have the following rules:
        * First line is a complete sentence ending in punctuation.
        * Begin with a terse, complete sentence.
            * Use imperative language.
            * For example, prefer "Return" over "Returns",
              "Determine" over "Determines", etc.
            * The arguments should each appear in the docstring in
              the same order as they appear in the argument list.
        * Do not indent lines in a docstring.
        * Texinfo formatting can be used inside docstrings.
            * Refer to [Appendix 2][ax-2] for a primer on Texinfo syntax.
            * More information on Texinfo is available [here][texinfo].

## Tests

* Use the [`(unit-test)`][unit-test] library for creating unit tests.
* We should have 100% coverage if possible.


# Appendix

## 1. Standard File Template

```scheme
;;; File: <FILE-NAME>
;;
;;; License:
;; Copyright © <YEAR> <COPYRIGHT-HOLDER> <<COPYRIGHT-HOLDER-EMAIL>>
;;
;; This file is part of <PACKAGE>.
;;
;; <PACKAGE> is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; <PACKAGE> is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with <PACKAGE>. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Author:     <AUTHOR-NAME>     <<AUTHOR-EMAIL>>
;;; Maintainer: <MAINTAINER-NAME> <<MAINTAINER-EMAIL>>
;;; Created:    <DATE-CREATED>
;;
;;; Homepage:   <WEBSITE>
;;
;;; Commentary:
;; <LIBRARY-DESCRIPTION>
;;
;;; Code:

(define-module (<PACKAGE-MODULE> <LIBRARY-NAME>)
  #:use-module (scheme documentation)
  ;; ... extra imports etc. go here ...
  #:export     ())
```

| Variable           | Value                                   |
| ------------------ | --------------------------------------- |
| `<WEBSITE>`        | https://github.com/taktoa/ThomasEngine2 |
| `<PACKAGE>`        | ThomasEngine2                           |
| `<PACKAGE-MODULE>` | `thomas`                                |


## 2. Texinfo Reference

| Texinfo                | Meaning                      | Short Example        |
| ---------------------- |:---------------------------- | -------------------- |
| `@var{VARIABLE}`       | Metasyntactic variable       | `@var{foo}`          |
| `@samp{SHELL-CMD}`     | Used for full shell commands | `@samp{ls -l}`       |
| `@code{CODE}`          | Generic code reference       | `@code{null}`        |
| `@command{COMMAND}`    | A shell command              | `@command{ls}`       |
| `@option{OPTION}`      | A shell command option       | `@option{-l}`        |
| `@env{VARIABLE}`       | An environment variable      | `@env{PATH}`         |
| `@key{KEY}`            | A key code, in Emacs format  | `@key{ENTER}`        |
| `@kbd{KEY-SEQ}`        | A key sequence               | `@kbd{C-@key{TAB}}`  |
| `@file{FILE-NAME}`     | A file name                  | `@file{foo.scm}`     |
| `@dfn{TERM}`           | Introducing a technical term | `@dfn{deleting}`     |
| `@abbr{ABBR, EXP}`     | An abbreviation              |                      |
| `@acronym{ACR, EXP}`   | An acronym                   |                      |
| `@url{URL[, TEXT]}`    | A link to a URL              |                      |
| `@email{ADDR[, TEXT]}` | A link to an email address   |                      |
| `@emph{TEXT}`          | Emphasize text               |                      |
| `@strong{TEXT}`        | Emphasize text even more     |                      |
| `@sc{TEXT}`            | Small caps text              |                      |
| `@example`             | Begin an example block       |                      |
| `@end example`         | End an example block         |                      |


--------------------------------------------------------------------------------

[ax-1]: #standard-file-template
[ax-2]: #texinfo-reference

[texinfo]:    http://www.gnu.org/software/texinfo/manual/texinfo/texinfo.html
[unit-test]:  http://www.nongnu.org/guile-lib/doc/ref/unit-test
[docstrings]: http://www.nongnu.org/guile-lib/doc/ref/scheme.documentation

