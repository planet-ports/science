#lang racket
;;; Science Collection
;;; constants-example.rkt
;;; Copyright (c) 2011 M. Douglas Williams
;;;
;;; This file is part of the Science Collection.
;;;
;;; The Science Collection is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the License
;;; or (at your option) any later version.
;;;
;;; The Science Collection is distributed in the hope that it will be useful,
;;; but WITHOUT WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with the Science Collection.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;;
;;; -----------------------------------------------------------------------------
;;;

(require (planet williams/science/constants/mksa-constants))

(define c mksa-speed-of-light) ; m / s
(define au mksa-astronomical-unit) ; m
(define minutes mksa-minute) ; s

;;; Distance in meters
(define r-earth (* 1.0 au)) ; m
(define r-mars (* 1.52 au)) ; m

;;; Time in seconds
(define t-min (/ (- r-mars r-earth) c)) ; s
(define t-max (/ (+ r-mars r-earth) c)) ; s

(printf "Light travel time from Earth to Mars:~n")
(printf "min = ~a minutes~n" (real->decimal-string (/ t-min minutes) 1))
(printf "max = ~a minutes~n" (real->decimal-string (/ t-max minutes) 1))
