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
  #:use-module (oop goops)
  #:use-module (scheme documentation)
  #:use-module (srfi   srfi-26)
  #:use-module (ice-9  hash-table)
  #:use-module (thomas utils misc)
  #:export     (make-entity
                modify-props
                <entity>
                prop-get-all
                prop-get))

(define* (undefined #:rest args) '())

(define (make-entity prop)
  (make <entity> (#:properties prop)))

(define-class <entity> (<class>)
     (properties 
       #:init-keyword #:properties 
       #:init-form (make-hash-table)
       #:getter prop-get-all))

(define hash-remove! undefined)
(define hash-set! undefined)
;;; Methods
;; Change a property
(define-method (prop-update (properties <entity>) k v)
               (if (eq? v (quote delete)) 
                 (hash-remove! properties k) 
                 (hash-set! properties k v)))

;; Change multiple properties at once
(define-method (modify-props (entity <entity>) changes)
  (let* ([update-pair (lambda [k v] (lambda [x] (prop-update x k v)))]
         [change-list  (hash-map->list update-pair changes)]
         [update       (apply compose change-list)])
        (make <entity> #:properties (update (get-properties entity)))))

;; Get a specific property of the entity
(define-method (prop-get k (entity <entity>)) (hash-ref (get-properties entity) k))

;; Get all properties of this entity
;(define-method (prop-get-all (entity <entity>)) (hash-copy properties))
