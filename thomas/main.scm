;;; File: main.scm
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
;; Main entry point of execution for ThomasEngine2
;;
;;; Code:

(define-module (thomas main)
  #:version    (0 0 1)
  #:use-module (scheme documentation)
  #:use-module (ice-9  hash-table)
  #:use-module (thomas texture)
  #:use-module (thomas event)
  #:use-module (thomas utility)
  #:use-module (thomas property-layer)
  #:use-module (thomas sprite-entity)
  #:use-module (thomas sprite-entity-set)
  #:export     ())

;;; ---------------------------- Program parameters ----------------------------

;; Canvas width and height
(define canvas-width  500)
(define canvas-height 500)

;; Center pixel location
(define (center-pixel-x canvas) (+ (get-field position-x canvas)
                                   (/ canvas-width 2)))
(define (center-pixel-y canvas) (+ (get-field position-y canvas)
                                   (/ canvas-height 2)))

;; Velocity in pixels per movement thread update
(define vel 5)

;; Texture file name
(define texture-path "../res/texture.png")

;; Property layer file name
(define property-layer-path "../res/properties.png")

;; Frame label
(define main-frame-label "Testing")

;; Define refresh rates in Hz
(define misc-key-refresh-rate 10)
(define screen-refresh-rate 60)
(define move-refresh-rate 120)

;; Hash table for property layer functions
(define property-hash
  (mkhash "black" 'collision
          "white" 'nothing))

;; Up, down, left, and right keys
(define up-key    #\w)
(define down-key  #\s)
(define left-key  #\a)
(define right-key #\d)
(define misc-key-hash
  (mkhash #\q exit))

;;; ---------------------------- Utility functions -----------------------------

;; Read in texture file at path
(define (get-texture path)
  (read-bitmap path 'unknown))

(define grass-bm (get-texture "../res/grass.png"))

(define main-character-entity
  (make-sprite-entity grass-bm 100 100 0 1/2))

(define main-entity-set (new <sprite-entity-set>))

(add-entity main-entity-set 'a main-character-entity)
(update! main-entity-set)

(define (test-renderer w h x y)
  (render main-entity-set w h x y))

(define (dmv dx dy canvas)
  (define (is-colliding x y)
    (equal? (property-at-pos property-layer x y)
            'collision))
  (let-values ([(cx cy) (get-entity-position main-entity-set 'a)])
    (define cdx (if (is-colliding (+ dx cx) cy) 0 dx))
    (define cdy (if (is-colliding cx (+ dy cy)) 0 dy))
    (set-entity-position main-entity-set 'a (+ cx cdx) (+ cy cdy))))

;;; ----------------------- Instantiate relevant objects -----------------------

;; Define a new frame
(define main-frame
  (new <frame>
       [label              main-frame-label]
       [style              '(no-resize-border)]
       [stretchable-width  #f]
       [stretchable-height #f]))

;; Define a new event handler
(define event-handler (make-object <evt-handler>))
(define event-handler-thread (get-key-thread event-handler))

;; Define a new canvas
(define main-ac
  (new <texture-canvas>
       [parent main-frame]
       [texture (get-texture texture-path)]
       [event-callback (λ (c) (thread-send event-handler-thread c))]
       [render-callback test-renderer]
       [width canvas-width]
       [height canvas-height]))

;; Define a property layer
(define property-layer
  (new <property-layer>
       [bitmap (get-texture property-layer-path)]
       [hash-table property-hash]))

;;; ---------------------- Timers, callbacks, and threads ----------------------

;; Screen refresh callback
(define (screen-refresh-callback)
  (define w (get-width main-ac))
  (define h (get-height main-ac))
  (let-values ([(x y) (get-entity-position main-entity-set 'a)])
    (set-position! main-ac (- x (/ w 2)) (- y (/ h 2))))
  (refresh main-frame))

;; Miscellaneous key callback
(define (misc-key-callback)
  (define (when-pressed? c a) (when (is-pressed? event-handler c) (a)))
  (hash-for-each misc-key-hash (λ (k v) (when-pressed? k v))))

;; Check if the requisite keys are being pressed
(define (move-key-checker)
  (let ([pressed? (λ [c] (is-pressed? event-handler c))])
    (values (pressed? up-key)
            (pressed? down-key)
            (pressed? left-key)
            (pressed? right-key))))

;; Movement update callback
(define (move-callback)
  (define-values (u d l r) (move-key-checker))
  (define (keys->vel a b) (* vel (- (if a 1 0) (if b 1 0))))
  (define v-x (keys->vel r l))
  (define v-y (keys->vel d u))
  (dmv v-x v-y main-ac))

;; Screen refresh timer
(define screen-refresh-timer
  (create-timer screen-refresh-callback screen-refresh-rate))

;; Miscellaneous key timer
(define misc-key-timer
  (create-timer misc-key-callback misc-key-refresh-rate))

;; Movement update timer
(define move-timer
  (create-timer move-callback move-refresh-rate))

;;; ------------------------------ Initialization ------------------------------

;; Show the canvas
(show main-frame #t)
