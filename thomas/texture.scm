;;; -*- mode: scheme; geiser-scheme-implementation: guile; coding: utf-8 -*-
;;; File: texture.scm
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
;; Defines a texture canvas for drawing
;;
;;; Code:

(define-module (thomas texture)
  #:version    (0 0 1)
  #:use-module (scheme documentation)
  #:use-module (oop goops)
  #:use-module (thomas utils misc)
  #:export     (<texture-canvas>))

(define-class <canvas> (<class>))
(define-class <texture-canvas> (<canvas>)
    ;;; Class fields
    ;(inherit
    ; get-width
    ; get-height
    ; refresh)

    ;(init-field
    ; parent
    ; texture
    ; width
    ; height
    ; [event-callback  (λ (c) #f)]
    ; [render-callback (λ (w h x y) (void))])

    (position-x
      #:init-value 0)
    (position-y
      #:init-value 0))

;;; Local variables
;; Get texture width and height
;; Dependent on <canvas>
;(define texture-width  (send texture get-width))
;(define texture-height (send texture get-height))

;;; Private functions
;; Draw the texture at a position
;; Probably going to be replaced entirely
;(define (draw-texture x y dc)
;    (send dc draw-bitmap-section texture 0 0 position-x position-y (get-width) (get-height)))

;;; Public functions
;; Utility functions for allowable position bounds
(define-method (min-x) 0)
(define-method (min-y) 0)
;; Dependent on <canvas> again
;(define-method (max-x) (- texture-width width))
;(define-method (max-y) (- texture-height height))

;; Set the screen position if it has changed, bracketed by position bounds
;; <canvas> dependent, slightly
;(define-method (set-position! x y)
;    (define adj-x (bound x (min-x) (max-x)))
;    (define adj-y (bound y (min-y) (max-y)))
;    (unless (and (= position-x adj-x) (= position-y adj-y))
;    (set! position-x adj-x)
;    (set! position-y adj-y)))

;;; Superclass overrides
;; Override on-char and on-event with the event callback
;; define/override probably not needed, but keeping for reference
;(define/override (on-char key-event) (event-callback key-event))
;(define/override (on-event key-event) (event-callback key-event))

;; Override screen-painting function
;(define/override (on-paint)
;    (let ([dc (send this get-dc)])
;    (send dc suspend-flush)
;    (draw-texture position-x position-y dc)
;    (send dc draw-bitmap
;            (render-callback (get-width) (get-height) position-x position-y) 0 0 'xor)
;    (send dc resume-flush)))

;;; Class initialization
;; Set paint callback, minimum width, and minimum height
;(super-new
;    [parent parent]
;    [min-width width]
;    [min-height height])

;; Focus the canvas
;(send this focus)
