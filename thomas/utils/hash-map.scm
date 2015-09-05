;;; File: hash-map.scm
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
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with ThomasEngine. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Author:     Remy Goldschmidt <taktoa@gmail.com>
;;; Maintainer: Remy Goldschmidt <taktoa@gmail.com>
;;; Created:    August 25th, 2015
;;
;;; Homepage:   https://github.com/taktoa/ThomasEngine2
;;
;;; Commentary:
;; The <hash-map> class is a GOOPS wrapper over the hash tables provided
;; by the (ice-9 hash-table) module included in Guile.
;;
;;; Code:

(define-module (thomas utils hash-map)
  #:version    (0 0 1)
  #:use-module (ice-9  hash-table)
  #:use-module (srfi   srfi-1)
  #:use-module (srfi   srfi-11)
  #:use-module (srfi   srfi-26)
  #:use-module (oop    goops)
  #:export     (<hash-map>
                incr1
                make-hash-map
                example-method
                <position>
                make-position
                ;; FIXME: add the rest
                ))

;;; Classes
(define-class <hash-map> ()
  (ht #:init-form (make-hash-table)
      #:getter    get-hash-table))

(define-class <position> ()
  (x #:init-keyword #:x
     #:accessor     x)
  (y #:init-keyword #:y
     #:accessor     y))

;;; Public methods
;; Get the property at the given position
(define-method (example-method (hash <hash-map>)) '())

;;; Public functions
;; Make a hash map
(define (make-hash-map)
  "testing"
  (make <hash-map>))

;; Make a position
(define (make-position x y) (make <position> #:x x #:y y))

;;; Private functions
;; Increments a number by 1
(define (incr1 i) (+ i 1))
