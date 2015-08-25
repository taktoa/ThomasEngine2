;;; File: entity.scm
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
;;; Created:    August 3rd, 2015
;;
;;; Homepage:   https://github.com/taktoa/ThomasEngine2
;;
;;; Commentary:
;; Provides a notion of an entity
;;
;;; Code:

(define-module (thomas entity)
  #:use-module (scheme documentation)
  #:use-module (ice-9  hash-table)
  #:export     (make-entity
                <entity>))

(define (make-entity prop)
  (new <entity> [properties prop]))

(define <entity>
  (class <object>
    ;;; Class fields
    (init-field
     [properties (make-hash-table)])

    ;;; Methods
    ;; Change a property
    (define/public prop-update
      (case-lambda
        [(k v)   (prop-update properties k v)]
        [(p k v) (if (equal? v 'delete)
                     (hash-remove! p k)
                     (hash-set! p k v))]))

    ;; Change multiple properties at once
    (define/public (modify-props changes)
      (define cl (hash->list changes))
      (define (compose-list list) (apply compose list))

      (define (update-item c)
        (λ (p) (prop-update p (car c) (cdr c))))
      (define update
        (compose-list (map update-item cl)))

      (new <entity> [properties (update properties)]))

    ;; Get a specific property of the entity
    (define/public (prop-get k) (hash-ref properties k))

    ;; Get all properties of this entity
    (define/public (prop-get-all) (hash-copy properties))

    ;;; Class initialization
    (super-new)))
