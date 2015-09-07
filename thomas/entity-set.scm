;;; File: entity-set.scm
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
;; This library provides a notion of entity sets
;;
;;; Code:

(define-module (thomas entity-set)
  #:version    (0 0 1)
  #:use-module (thomas entity)
  #:use-module (oop    goops)
  #:use-module (ice-9  q)
  #:use-module (ice-9  match)
  #:use-module (ice-9  hash-table)
  #:use-module (srfi srfi-26)
  #:use-module (thomas entity)
  #:use-module (thomas utils misc)
  #:export     (<entity-set>))

(define* (hash-update! hash-table key function)
  "docstring"
  (letrec ([old-value (hash-ref hash-table key)]
           [new-value (function old-value)])
    (hash-set! hash-table key new-value)))

(define-method (apply-update update)
  "docstring"
  (letrec ([mod-prop   (λ [ph]      (cut modify-props <> ph))]
           [set-props! (λ [eh n ph] (hash-update! eh n (mod-prop ph)))]
           [name       (car update)]
           [entity     (λ [entity <entity>] entity)]
           [ent-hash   (get-entity-hash entity)])
    (match (cdr update)
      [(? hash-table? ph)  (set-props!   ent-hash name ph)]
      [(? entity? entity)  (hash-set!    ent-hash name entity)]
      ['add                (hash-set!    ent-hash name #f)]
      ['delete             (hash-remove! ent-hash name)]
      [_                   (throw 'apply-update! "entity" update)])))

(define* (entity-set? value)
  "Return true when the given value is an entity set. Otherwise, return false."
  (is-a? value <entity-set>))

(define-class <entity-set> (<class>)
  (update-queue #:init-keyword #:update-queue
                #:init-form    (make-q)
                #:getter       get-update-queue)
  (entity-hash  #:init-keyword #:entity-hash
                #:init-form    (make-hash-table)
                #:getter       get-entity-hash))

;;; Methods
(define-method (add-entity (entity-set <entity-set>) name (ent <entity>))
  "Queue up an entity addition."
  (enq! (get-update-queue entity-set)
        (cons name ent)))

(define-method (rem-entity (entity-set <entity-set>) name)
  "Queue up an entity removal."
  (enq! (get-update-queue entity-set)
        (cons name 'delete)))

(define-method (set-entity name ent (entity-set <entity-set>))
  "Set the value of the entity with the given @var{name}."
  (enq! (get-update-queue entity-set) (cons name ent)))

(define-method (set-entity-properties name props (entity-set <entity-set>))
  "Queue up changes to the entity with the given @var{name}."
  (enq! (get-update-queue entity-set) (cons name props)))

(define-method (set-entity-property name key value)
  "Queue up a single change to the entity with the given @var{name}."
  (set-entity-properties name (make-hash key value)))

(define-method (get-entities (entities <entity-set>))
  "Get all entities."
  (update!)
  (hash-copy (get-entity-hash entities)))

(define-method (get-entity name (entity-set <entity-set>))
  "Get the entity with the given @var{name}."
  (hash-ref (get-entity-hash entity-set) name))

(define-method (get-entity-properties name (entity-set <entity-set>))
  "Get all properties of an entity."
  (prop-get-all (get-entity name entity-set)))

(define-method (get-entity-property (entity-set <entity-set>) name prop)
  "Get a property of an entity."
  (prop-get prop (get-entity name)))

(define-method (clear-update-queue (entity-set <entity-set>))
  "Clear the update queue"
  (set! update-queue (make-q)))

(define (in-queue queue)
  "Return a list corresponding to the contents of the given queue."
  '())

(define-method (update!)
  "Apply all updates in queue and clear it."
  (let loop ([xs (in-queue update-queue)])
       (when (not-null? xs)
         (apply-update! c)
         (loop (cdr xs))))
  (clear-update-queue))
