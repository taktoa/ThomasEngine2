;;; File: event.scm
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
;; Defines a class for event handlers
;;
;;; Code:

(define-module (thomas event)
  #:version    (0 0 1)
  #:use-module (scheme documentation)
  #:use-module (oop goops)
  #:use-module (ice-9 futures)
  #:use-module (ice-9 match)
  #:export     (<evt-handler>))

(define undefined '())
(define true #t)
(define* (void #:rest args) undefined)
(define* (mutable-set #:rest args) undefined)
(define get-event-type void)
(define get-x void)
(define get-y void)
(define get-key-code void)
(define get-key-release-code void)
(define set-remove! void)
(define set-add! void)
(define pressed-keys undefined)
(define set-member? void)

(define-class <evt-handler> (<class>)
    (pressed-keys 
      #:init-keyword #:keys
      #:init-form (mutable-set)))

;;; Private functions
;; Translate an event to a more usable form
(define (translate-event e)
  (cond
   [(is-a? e <mouse-event>) (mouse-translate-event e)]
   [(is-a? e <key-event>) (key-translate-event e)]))

(define-class <mouse-event> (<class>))
(define-class <key-event> (<class>))

;; Unwrap mouse event type, x, and y, and pass them to mouse-translate
(define (mouse-translate-event e)
  (mouse-translate (get-event-type e) (get-x e) (get-y e)))

;; Translate mouse event type, x, and y to a list
(define (mouse-translate t x y) (list t x y))

;; Unwrap the key-codes from the key-event and pass them to key-translate
(define (key-translate-event e)
  (key-translate (get-key-code e) (get-key-release-code e)))

;; Translate raw key-events to more useable pairs
(define (key-translate x y)
 (cond
   [(eq? x 'release) (list y 'release)]
   [(eq? y 'press)   (list x 'press)]
   [true             (void)]))

;; Add or remove a key from the key-set
(define (set-pressed-keys l)
    (match l
           [(list x 'press)   (set-add! pressed-keys x)]
           [(list x 'release) (set-remove! pressed-keys x)]
           [_                 (void)]))

;;; Public functions
;; Getter for key capture thread and key state
(define-method (get-key-thread) key-thread)
(define-method (get-key-state) pressed-keys)

;; Utility function for determining whether or not a key is pressed
(define-method (is-pressed? c) (set-member? pressed-keys c))

;; TODO finish code
(define* (thread-receive #:rest args) '())

;;; Class initialization
;; Key capture thread
;(define key-thread
;  (make-future (let loop ()
;                 (set-pressed-keys (translate-event (thread-receive)))
;                 (loop))))

(define key-thread undefined)
