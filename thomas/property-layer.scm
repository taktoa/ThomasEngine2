;;; -*- mode: scheme; geiser-scheme-implementation: guile; coding: utf-8 -*-
;;; File: property-layer.scm
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
;; Property layers are a way to add arbitrary information to the 2D plane
;;
;;; Code:


(define-module (thomas property-layer)
  #:version    (0 0 1)
  #:use-module (scheme documentation)
  #:use-module (ice-9  hash-table)
  #:use-module (oop goops)
  #:use-module (thomas utils misc)
  #:export     (<property-layer>
                property-at-pos))

(define* (undefined #:rest args) '())

;;; Classes
(define-class <hash-map> (<class>)
  (ht #:init-value (undefined)))

(define-class <property-layer> (<class>)
  (defval
   #:init-keyword #:default-value
   #:getter       get-default-value)
  (height
   #:init-keyword #:height
   #:getter       get-height)
  (width
   #:init-keyword #:width
   #:getter       get-width)
  (prop-table
   #:init-keyword #:prop-table
   #:init-form    (make-hash-table)
   #:getter       get-prop-table))

(define-class <position> (<class>)
  (x #:init-keyword #:x #:accessor x)
  (y #:init-keyword #:y #:accessor y))

;;; Public methods
;; Get the property at the given position
(define-method (property-at-pos (prop-layer <property-layer>)
                                (position   <position>))
  (let* ([pos-x   (x position)]
         [pos-y   (y position)]
         [width   (get-width         prop-layer)]
         [height  (get-height        prop-layer)]
         [default (get-default-value prop-layer)]
         [valid-x (bound pos-x 0 width)]
         [valid-y (bound pos-y 0 height)]
         [table   (get-prop-table prop-layer)])
    (cond [(not valid-x) default]
          [(not valid-y) default]
          [else          (undefined)])))

;;; Public functions
;; Returns a function that will get the property at a provided x and y location.
(define-method (property-getter (prop-layer <property-layer>))
  (λ [x y] (property-at-pos prop-layer (make-pos x y))))

(define-method (get-property (prop-layer <property-layer>) x y)
  ((property-getter prop-layer) x y))

(define (make-pos x y)
  (make <position> #:x x #:y y))

;;  (let ([prop-gotten (make-object <color> "white")])
;;    (get-pixel bitmap-dc (x pos) (y pos) prop-gotten)
;;    (hash-ref color-value-hash (color-numbers color-gotten) 'unknown)))

(define-class <color> (<object>)
              (red
                #:init-keyword #:red
                #:init-value 0
                #:getter get-red)
              (blue
                #:init-keyword #:blue
                #:init-value 0
                #:getter get-blue)
              (green
                #:init-keyword #:green
                #:init-value 0
                #:getter get-green)
              (name
                #:init-keyword #:name
                #:init-value "white"))

;;; Private functions
;; Utility function used to turn a color object into a list of form '(R G B)
(define-method (color-numbers (color <color>))
  (list (get-red color) (get-green color) (get-blue color)))

;; General function that maps f over the keys in a hash
(define (hash-key-map f h) undefined)
  ;(for/hash ([(k v) (in-hash h)]) (values (f k) v)))

(define-class <color> (<object>)
              (red
                #:init-keyword #:red
                #:init-value 0)
              (blue
                #:init-keyword #:blue
                #:init-value 0)
              (green
                #:init-keyword #:green
                #:init-value 0)
              (name
                #:init-keyword #:name
                #:init-value "white"))

;; Takes a color name (from the-color-database) and gives its RGB components
(define (color-value color-name)
  (define color (make <color> #:name color-name))
  (color-numbers color))

;; Takes the hash-table given at initialization and creates a list from it
(define-method (color-value-hash (prop-layer <property-layer>))
  (hash-key-map (λ (color-name) (color-value color-name)) (get-prop-table prop-layer)))



;(define <property-layer>
;  (class <object>
    ;;; Class fields
;    ('init-field
;     bitmap
;     (bitmap-dc (send bitmap make-dc))
;     hash-table)

    ;;; Methods
    ;; Get property at a given position
;    (define/public (property-at-pos x y)
;      (hash-color x y))

    ;;; Class initialization
;    (super-new)))
