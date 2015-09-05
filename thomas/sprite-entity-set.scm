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
;; along with ThomasEngine2. If not, see <http://www.gnu.org/licenses/>.
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
  #:version    (0 0 1)
  #:use-module (scheme documentation)
  #:use-module (oop goops)
  #:use-module (ice-9  hash-table)
  #:use-module (thomas entity-set)
  #:use-module (thomas sprite-entity)
  #:export     (<sprite-entity-set>))

(define-class <sprite-entity-set> (<entity-set>))

;;; Private functions
(define (get-entities-within-area w h x y)
    (define (posn-within-area px py)
    (and (< (- px x) w) (< (- py y) h)))
    (define (entity-within-area ent)
        (posn-within-area
        (send ent prop-get 'position-x)
        (send ent prop-get 'position-y)))
    (define result (make-hash))
    (hash-for-each
    (get-entities)
    (λ (k v) (if (entity-within-area v) (hash-set! result k v) (void)))) result)

;;; Public functions
(define-method (set-entity-position name x y)
    (set-entity-properties name (mkhash 'position-x x 'position-y y)))

(define-method (set-entity-rotation name r)
    (set-entity-property name 'rotation r))

(define-method (set-entity-scale name s)
    (set-entity-property name 'scale s))

(define-method (get-entity-position name)
    (values
    (get-entity-property name 'position-x)
    (get-entity-property name 'position-y)))

(define-method (get-entity-rotation name)
    (get-entity-property name 'rotation))

(define-method (get-entity-scale name)
    (get-entity-property name 'scale))

(define-method (render width height x y)
    (update!)
    (define to-draw (get-entities-within-area width height x y))
    (define sprites (hash-map to-draw (λ (k v) (send v render))))
    (define dc (new <bitmap-dc> [bitmap (make-bitmap width height #t)]))
    (for-each
    (match-lambda
        [(list rr px py) (send dc draw-bitmap rr
                            (- (- px (* 1/2 (send rr get-width)))  x)
                            (- (- py (* 1/2 (send rr get-height))) y) 'xor)])
    sprites)
    (send dc get-bitmap))
