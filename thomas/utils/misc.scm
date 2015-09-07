;;; File: utility.scm
;;
;;; License:
;; Copyright © 2015 Remy Goldschmidt <taktoa@gmail.com>
;;
;; This file is part of ThomasEngine2.
;;
;; ThomasEngine2 is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; ThomasEngine2 is distributed in the hope that it will be useful,
;; but WITHOUT any WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with ThomasEngine. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Author:     Remy Goldschmidt <taktoa@gmail.com>
;;; Maintainer: Remy Goldschmidt <taktoa@gmail.com>
;;; Created:    August 3rd, 2015
;;
;;; Homepage:   https://github.com/taktoa/ThomasEngine2
;;
;;; Commentary:
;; Utility functions for ThomasEngine2
;;
;;; Code:

(define-module (thomas utils misc)
  #:version    (0 0 1)
  #:duplicates (check)
  #:use-module (scheme documentation) ;; guile-lib: Documentation
  #:use-module (ice-9 pretty-print)   ;; ice-9:     Pretty-printing
  #:use-module (ice-9 match)          ;; ice-9:     Pattern matching
  #:use-module (ice-9 regex)          ;; ice-9:     Regular expressions
  #:use-module (ice-9 iconv)          ;; ice-9:     Internationalization
  #:use-module (rnrs io ports)        ;; R7RS:      I/O ports
  #:use-module (srfi srfi-19)         ;; SRFI:      Time type + procedures
  #:use-module (srfi srfi-26)         ;; SRFI:      Partial application (cut)
  #:use-module (srfi srfi-64)         ;; SRFI:      Unit testing
  #:use-module (oop goops)            ;; Other:     Object-oriented programming
  #:use-module (oop goops describe)   ;; Other:     Object pretty-printing
  #:use-module (statprof)             ;; Other:     Statistical profiling
  #:re-export  (;; (srfi srfi-26)
                cut cute
                ;; (ice-9 pretty-print)
                pretty-print
                ;; (ice-9 match)
                match
                ;; (scheme documentation)
                define-with-docs
                define-macro-with-docs
                define-class-with-docs
                define-generic-with-docs
                ;; (oop goops describe)
                describe
                ;; (statprof)
                statprof-active?
                statprof-start
                statprof-stop
                statprof-reset
                statprof-accumulated-time
                statprof-sample-count
                statprof-display
                statprof-display-anomolies
                statprof-fetch-stacks
                statprof-fetch-call-tree
                statprof
                with-statprof
                gcprof)
  #:export     (;; Macros
                EXIT-SUCCESS
                EXIT-FAILURE
                λ* thunk
                _ _%
                get-default-eol
                do-nothing

                ;; Classes
                <timer> make-timer

                ;; List functions
                pairs
                nil?
                one?
                ++
                make-hash
                hash-copy

                ;; String functions
                ++s
                string-concat
                string-sed
                split-lines

                ;; Output functions
                fmtstr
                cfmtstr
                printf
                printfln
                cprintfln
                outprintf
                outprintfln
                errprintf
                errprintfln
                warnf
                errf

                ;; Misc functions
                install-locale
                bound
                hz-to-ms
                ms-to-hz))


;;; ----------------------------------------------------------------------------
;;; --------------------------------- Macros -----------------------------------
;;; ----------------------------------------------------------------------------


;; FIXME: add documentation
(define-syntax EXIT-SUCCESS (λ [stx] #'0))
;; FIXME: add documentation
(define-syntax EXIT-FAILURE (λ [stx] #'-1))

;; FIXME: add documentation
(define-syntax λ*
  (syntax-rules ()
    [(λ* args body ...) (lambda* args body ...)]))
;; FIXME: add documentation
(define-syntax thunk
  (syntax-rules ()
    [(thunk body ...) (lambda [] body ...)]))

;; FIXME: add documentation
(define-syntax _
  (syntax-rules () [(_ msg)  (gettext msg "thomas-engine")]))
;; FIXME: add documentation
(define-syntax _%
  (syntax-rules () [(_% msg) (gettext msg "thomas-engine")]))

;; FIXME: add documentation
(define-syntax get-default-eol
  (λ [stx] #`#,(format #t "~%")))
;; FIXME: add documentation
(define-syntax do-nothing
  (λ [stx] #''()))


;;; ----------------------------------------------------------------------------
;;; -------------------------------- Parameters --------------------------------
;;; ----------------------------------------------------------------------------


(define-with-docs comment%
  (_ "The string to prepend to the output of cfmtstr, cprintf, and cprintfln.")
  (make-parameter ";; "))


;;; ----------------------------------------------------------------------------
;;; --------------------------------- Classes ----------------------------------
;;; ----------------------------------------------------------------------------


;; FIXME: needs documentation
(define-class <timer> ()
  (callback #:init-keyword #:callback
            #:init-form    (λ [] '()))
  (interval #:init-keyword #:interval
            #:getter       get-interval))


;;; ----------------------------------------------------------------------------
;;; --------------------------------- Methods ----------------------------------
;;; ----------------------------------------------------------------------------

;; FIXME: add methods for <timer>

;;; ----------------------------------------------------------------------------
;;; ----------------------------- Public functions -----------------------------
;;; ----------------------------------------------------------------------------


;;; ----------------------------- List functions -----------------------------

(define any (@ (srfi srfi-1) any))

(define* (++ #:rest lists)
  (_ "Same as `append', but shorter.")
  (apply append lists))

(define* (nil? #:rest values)
  (_ "Are the given @var{values} equivalent to @code{#nil}?")
  (any (λ [x] (and (null? x) (eqv? x #nil))) values))

(define* (one? list)
  (_ "Determines whether a @var{list} is a singleton.")
  (= (length list) 1))

(define* (pairs list)
  (_ "Converts a @var{list} with an even number of elements into a @var{list}
     of pairs, also known as an association @var{list.}")
  (cond [(null? list) '()]
        [(odd? (length list)) (error 'pairs "odd-length list")]
        [else (cons (cons (car list) (cadr list))
                    (pairs (cddr list)))]))

(define* (alist->plist xs) 
         (_ "Converts an association list (alist) into a proper list (plist).")
         (case xs
           ('() '())
           (else (++ (list (caar xs) (cadar xs)) (alist->plist (cdr xs))))))


;;; --------------------------- Hash table functions ---------------------------

(define* (make-hash #:rest args) 
         (_ "Converts a call on an even number of args into a hash-table with 
            the args as key-value pairs.")
         (let ([ht (make-hash-table (length args))])
         (for-each (lambda [p] (hash-set! ht (car p) (cdr p))) (pairs args)) ht))

(define* (ht-keys ht)
  (_ "Get a list of all the keys in the given hash-table.")
  (hash-map->list (lambda (x y) x) ht))

(define* (ht-vals ht)
  (_ "Get a list of all the values in the given hash-table.")
  (hash-map->list (lambda (x y) y) ht))

(define* (ht-pairs ht)
  (_ "Convert a hash-table (@var{ht}) to a list of pairs of keys and values.")
  ; This could be written as (hash-map->list list ht) or even as 
  ; (hash-map->list cons ht)
  (hash-map->list (lambda (x y) (cons x y)) ht))

(define* (hash-copy ht)  
  (_ "Creates a deep-copy of a hash-table.")
  (make-hash (alist->plist (ht-pairs ht))))

;;; ----------------------------- String functions -----------------------------

(define* (++s #:rest strings)
  (_ "Same as `string-append', but shorter.")
  (apply string-append strings))

(define* (string-concat #:rest lists)
  (_ "Concatenate a list of strings. If multiple arguments are given,
concatenate each list of strings and then concatenate the results.")
  (if (one? lists)
      (apply ++s (car lists))
      (map string-concat lists)))

(define* (string-sed input regexp new)
  (_ "Replace all instances of @var{regexp} in @var{input} with @var{new}.")
  (regexp-substitute/global #f regexp input 'pre new 'post))

(define* (split-lines input #:key (eol-string (get-default-eol)))
  (_ "Split an @var{input} string into lines.
Optionally, provide @var{eol-string} as a keyword argument;
@var{eol-string} represents the end of line string for the given input.")
  (let ([fix-eol (lambda* []  (string-sed   input eol-string "\n"))])
    (cond
     [(equal? eol-string "\n") (string-split input     #\lf)]
     [(equal? eol-string "\r") (string-split input     #\cr)]
     [else                     (string-split (fix-eol) #\lf)])))

;;; ----------------------------- I/O functions -----------------------------

(define* (outprintf fmt #:rest args)
  (_ "Format and print the given arguments to the current output port.")
  (apply format `(,(current-output-port) ,fmt ,@args)))

(define* (errprintf fmt #:rest args)
  (_ "Format and print the given arguments to the current error port.")
  (apply format `(,(current-error-port) ,fmt ,@args)))

(define* (outprintfln fmt #:rest args)
  (_ "Format and print the given arguments to the current output port with an
appended newline.")
  (apply format `(,(current-output-port)
                  ,(string-append fmt "~%")
                  ,@args)))

(define* (errprintfln fmt #:rest args)
  (_ "Format and print the given arguments to the current error port.")
  (apply format `(,(current-error-port)
                  ,(string-append fmt "~%")
                  ,@args)))

(define* (fmtstr fmt #:rest args)
  (_ "Return the result of substituting each of the format variables given in
@var{fmt} with @var{args}.")
  (apply format `(#f ,fmt ,@args)))

(define* (printf fmt #:rest args)
  (_ "Alias for `outprintf'.")
  (apply outprintf `(,fmt ,@args)))

(define* (printfln fmt #:rest args)
  (_ "Alias for outprintfln")
  (apply outprintfln `(,fmt ,@args)))

(define* (cfmtstr fmt #:rest args)
  (_ "Format the given string, prepending a comment -- which can be customized
with the @code{comment%} parameter.")
  (string-concat
   (map (cut string-append (comment%) <>)
        (split-lines (apply fmtstr `(,fmt ,@args))))))

(define* (cprintf fmt #:rest args)
  (_ "Print a formatted string, prepending a comment.")
  (printf "~a" (apply cfmtstr `(,fmt ,@args))))

(define* (cprintfln fmt #:rest args)
  (_ "Print a formatted string, prepending a comment and appending an EOL.")
  (printf "~a~%" (apply cfmtstr `(,fmt ,@args))))

(define* (warnf fmt #:rest args)
  (_ "Print a formatted warning message.")
  (errprintf (string-append "~a" (_% "Warning:") "~%" "~a" "~%")
             (cfmtstr "")
             (apply cfmtstr `(,fmt ,@args))))

(define* (errf fmt #:rest args)
  (_ "Print a formatted error message.")
  (errprintf (string-append "~a" (_% "Error:") "~%" "~a" "~%")
             (cfmtstr "")
             (apply cfmtstr `(,fmt ,@args))))

;;; ----------------------------- Miscellaneous functions -----------------------------

(define* (install-locale)
  (_ "Install the system locale. This is required for printing Unicode properly
 to the console (you may experience mojibake or similar without this).")
  (when (defined? 'setlocale)
    (catch 'system-error
      (thunk (setlocale LC_ALL ""))
      (lambda* [#:rest args] (warnf (++s (_% "failed to install locale: ") "~a")
                                    (strerror (system-error-errno args)))))))

(define* (bound x a b)
  (_ "Bracket @var{x} within [@var{a}, @var{b}].
More specifically, the following case analysis is performed:
@example
If a <= x <= b, return x
If x < a,       return a
If x > b,       return b
@end example")
  (cond [(> x b) b]
        [(< x a) a]
        [else    x]))

(define* (hz-to-ms freq)
  (_ "Determine the period time in milliseconds of a periodic event when given
its frequency in Hz.")
  (/ 1000 freq))

(define* (ms-to-hz period)
  (_ "Determine the frequency in Hz of a periodic event when given its period
in milliseconds.")
  (/ 1000 period))

(define* (make-timer callback rate)
  (_ "Create a timer with a given callback and call frequency (in Hz).")
  (make <timer>
    #:callback callback
    #:interval (hz-to-ms rate)))
