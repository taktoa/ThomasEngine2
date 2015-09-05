;;; File: sprite-entity.scm
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
;; Provides a notion of sprite entities
;;
;;; Code:

(define-module (thomas sprite-entity)
  #:version    (0 0 1)
  #:use-module (scheme documentation)
  #:use-module (oop goops)
  #:use-module (ice-9  hash-table)
  #:use-module (thomas utils misc)
  #:use-module (thomas entity)
  #:export     (make-sprite-entity
                <sprite-entity>))

(define pi 3.14592654)

(define (make-sprite-entity sprite x y r s)
  (define props (make-hash 'sprite sprite
                           'position-x x
                           'position-y y
                           'rotation r
                           'scale s))
  (make <sprite-entity> #:properties props))

(define-class <sprite-entity> (<entity>))

;;; Private functions
;; Convert degrees to radians
(define (dtr d) (* d pi 1/180))

;; Dependent on guile's equivalent of drawing-contexts
;(define (render-sprite)
;    (let* ([sprite (prop-get 'sprite)]
;           [s (prop-get 'scale)]
;           [r (prop-get 'rotation)]
;           [sw (send sprite get-width)]
;           [sh (send sprite get-height)])
;    (define (gen-blank-bm) (make-bitmap (* 2 s sw) (* 2 s sh) #t))
;    (define sprite-dc (new <bitmap-dc> [bitmap (gen-blank-bm)]))
;    (send sprite-dc set-origin (* s sw) (* s sh))
;    (send sprite-dc set-rotation (dtr r))
;    (send sprite-dc set-scale s s)
;    (send sprite-dc draw-bitmap sprite (* -1/2 sw) (* -1/2 sh))
;    (send sprite-dc set-rotation r)
;    (send sprite-dc get-bitmap)))

;;; Public functions
;; Dependent on guile's render functions
;(define-method (render)
;    (list (render-sprite)
;        (prop-get 'position-x)
;        (prop-get 'position-y)))
