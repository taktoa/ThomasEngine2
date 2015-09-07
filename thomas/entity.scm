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
;; along with ThomasEngine2. If not, see <http://www.gnu.org/licenses/>.
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
  #:version    (0 0 1)
  #:use-module (srfi   srfi-26)
  #:use-module (ice-9  hash-table)
  #:use-module (oop    goops)
  #:use-module (thomas utils misc)
  #:export     (entity?
                make-entity
                modify-props
                <entity>
                prop-get-all
                prop-get))

(define* (entity? value)
  "Return true when the given value is an entity. Otherwise, return false."
  (is-a? value <entity>))

(define* (undefined #:rest args) '())

(define* (make-entity #:optional properties)
  "Make an entity with the given initial properties"
  (make <entity> (#:properties prop)))

(define-class <entity> (<class>)
;;  "docstring"
  (properties #:init-keyword #:properties
              #:init-form    (make-hash-table)
              #:getter       prop-get-all))

;;; Methods
(define-method (prop-update (entity <entity>) k v)
  "Change a property."
  (match v
    ['delete (hash-remove! (get-properties entity) k)]
    [_       (hash-set!    (get-properties entity) k v)]))

;; Change multiple properties at once
(define-method (modify-props (entity <entity>) changes)
  (let* ([update-pair (lambda [k v] (lambda [x] (prop-update x k v)))]
         [change-list  (hash-map->list update-pair changes)]
         [update       (apply compose change-list)])
        (make <entity> #:properties (update (get-properties entity)))))

;; Get a specific property of the entity
(define-method (prop-get (entity <entity>) k)
  (hash-ref (get-properties entity) k))

;; Get all properties of this entity
;(define-method (prop-get-all (entity <entity>)) (hash-copy properties))
