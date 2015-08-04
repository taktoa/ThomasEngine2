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
;; along with ThomasEngine. If not, see <http://www.gnu.org/licenses/>.
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
  #:use-module (scheme documentation)
  #:use-module (ice-9  q)
  #:use-module (thomas entity)
  #:export     (entity-set%))

(define entity-set%
  (class object%
    ;;; Class fields
    (field
     [update-queue (make-queue)]
     [entity-hash  (make-hash)])

    ;;; Private functions
    ;; Set the properties of one entity
    (define/private (set-entity-properties! name props)
      (hash-update! entity-hash name
                    (λ (e) (send e modify-props props))))

    ;; Apply one update
    (define/private (apply-update! c)
      (match c
        [(cons n 'add) (hash-set! entity-hash n #f)]
        [(cons n 'delete) (hash-remove! entity-hash n)]
        [(cons n (? hash? p)) (set-entity-properties! n p)]
        [(cons n ent) (hash-set! entity-hash n ent)]
        [else (raise-argument-error 'apply-update! "entity" c)]))

    ;; Clear queue
    (define/private (clear-queue!)
      (set! update-queue (make-queue)))

    ;;; Public functions
    ;; Queue up an entity addition
    (define/public (add-entity name)
      (enqueue! update-queue (cons name 'add)))

    ;; Queue up an entity removal
    (define/public (rem-entity name)
      (enqueue! update-queue (cons name 'delete)))

    ;; Queue up entity changes (directly setting entity)
    (define/public (set-entity name ent)
      (enqueue! update-queue (cons name ent)))

    ;; Queue up entity changes
    (define/public (set-entity-properties name props)
      (enqueue! update-queue (cons name props)))

    ;; Queue up a single entity change
    (define/public (set-entity-property name key value)
      (set-entity-properties name (hash key value)))

    ;; Get all entities
    (define/public (get-entities)
      (update!)
      (hash-copy entity-hash))

    ;; Get one entity
    (define/public (get-entity name)
      (hash-ref entity-hash name))

    ;; Get all properties of an entity
    (define/public (get-entity-properties name)
      (send (get-entity name) prop-get-all))

    ;; Get a property of an entity
    (define/public (get-entity-property name prop)
      (send (get-entity name) prop-get prop))

    ;; Apply all updates in queue and clear it
    (define/public (update!)
      (for ([c (in-queue update-queue)])
        (apply-update! c))
      (clear-queue!))

    ;;; Class initialization
    (super-new)))
