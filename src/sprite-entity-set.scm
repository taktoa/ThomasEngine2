;;; File: sprite-entity-set.scm
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
;; Provides a notion of sprite entity sets
;;
;;; Code:

(define-module (thomas sprite-entity-set)
  #:use-module (scheme documentation)
  #:use-module (thomas entity-set)
  #:use-module (thomas sprite-entity)
  #:export     (sprite-entity-set%))

(define sprite-entity-set%
  (class entity-set%
    ;;; Class fields
    (inherit
      add-entity
      rem-entity
      set-entity-properties
      set-entity-property
      get-entity
      get-entities
      get-entity-properties
      get-entity-property
      update!)

    (inherit-field
     entity-hash)

    ;;; Private functions
    (define/private (get-entities-within-area w h x y)
      (define (posn-within-area px py)
        (and (< (- px x) w) (< (- py y) h)))
      (define (entity-within-area ent)
        (posn-within-area
         (send ent prop-get 'position-x)
         (send ent prop-get 'position-y)))
      (define result (make-hash))
      (hash-for-each
       (get-entities)
       (λ (k v) (if (entity-within-area v) (hash-set! result k v) (void))))
      result)

    ;;; Public functions
    (define/public (set-entity-position name x y)
      (set-entity-properties name (hash 'position-x x 'position-y y)))

    (define/public (set-entity-rotation name r)
      (set-entity-property name 'rotation r))

    (define/public (set-entity-scale name s)
      (set-entity-property name 'scale s))

    (define/public (get-entity-position name)
      (values
       (get-entity-property name 'position-x)
       (get-entity-property name 'position-y)))

    (define/public (get-entity-rotation name)
      (get-entity-property name 'rotation))

    (define/public (get-entity-scale name)
      (get-entity-property name 'scale))

    (define/public (render width height x y)
      (update!)
      (define to-draw (get-entities-within-area width height x y))
      (define sprites (hash-map to-draw (λ (k v) (send v render))))
      (define dc (new bitmap-dc% [bitmap (make-bitmap width height #t)]))
      (for-each
       (match-lambda
         [(list rr px py) (send dc draw-bitmap rr
                                (- (- px (* 1/2 (send rr get-width)))  x)
                                (- (- py (* 1/2 (send rr get-height))) y)
                                'xor)])
       sprites)
      (send dc get-bitmap))

    ;;; Class initialization
    (super-new)))
