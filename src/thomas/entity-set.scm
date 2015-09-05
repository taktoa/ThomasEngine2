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
  #:version    (0 0 1)
  #:use-module (scheme documentation)
  #:use-module (oop goops)
  #:use-module (ice-9  q)
  #:use-module (ice-9  match)
  #:use-module (ice-9  hash-table)
  #:use-module (thomas entity)
  #:export     (<entity-set>))

(define (hash-update! hash-table key function)
  (letrec ([old-value (hash-ref hash-table key)]
           [new-value (function old-value)])
    (hash-set! hash-table key new-value)))
(define* (apply-update! update)
         (letrec ([mod-prop   (λ  [ph]  (cut modify-props <> ph))]
                  [set-props! (λ  [eh n ph]  (hash-update! eh n (mod-prop ph)))]
                  [name       (car update)]
                  [ent-hash   (get-entity-hash entity)])
           (match (cdr update)
                  [(? hash-table? ph)  (set-props!   ent-hash name ph)]
                  [(? entity? entity)  (hash-set!    ent-hash name ent)]
                  ['add               (hash-set!    ent-hash name #f)]
                  ['delete            (hash-remove! ent-hash name)]      
                  [_                  (throw 'apply-update! "entity" update)])))

(define* (entity? value) (is-a? value <entity>))

(define-class <entity-set> (<class>)
    (update-queue 
      #:init-keyword #:update-queue
      #:init-form (make-q)
      #:getter get-update-queue)
    (entity-hash  
      #:init-keyword #:entity-hash
      #:init-form (make-hash-table)
      #:getter get-entity-hash))

;;; Methods
;; Queue up an entity addition
(define-method (add-entity name)
  (enq! update-queue (cons name 'add)))

;; Queue up an entity removal
(define-method (rem-entity name)
  (enq! update-queue (cons name 'delete)))

;; Queue up entity changes (directly setting entity)
(define-method (set-entity name ent)
  (enq! update-queue (cons name ent)))

;; Queue up entity changes
(define-method (set-entity-properties name props)
  (enq! update-queue (cons name props)))

;; Queue up a single entity change
(define-method (set-entity-property name key value)
  (set-entity-properties name (mkhash key value)))

;; Get all entities
(define-method (get-entities)
  (update!)
  (hash-copy entity-hash))

;; Get one entity
(define-method (get-entity name)
  (hash-ref entity-hash name))

;; Get all properties of an entity
(define-method (get-entity-properties name)
  (send (get-entity name) prop-get-all))

;; Get a property of an entity
(define-method (get-entity-property name prop)
  (send (get-entity name) prop-get prop))

;; Apply all updates in queue and clear it
(define-method (update!)
  (for ([c (in-queue update-queue)])
  (apply-update! c))
  (set! update-queue (make-q)))

;;; Class initialization
;    (super-new)))
