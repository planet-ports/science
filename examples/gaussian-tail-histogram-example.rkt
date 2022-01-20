#lang racket
;;; Science Collection
;;; gaussian-tail-histogram-example.rkt
;;; Copyright (c) 2004-2011 M. Douglas Williams
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
;;; -------------------------------------------------------------------
;;;

(require (planet williams/science/random-distributions/gaussian-tail)
         (planet williams/science/histogram-with-graphics))

(let ((h (make-histogram-with-ranges-uniform 40 16.0 22.0)))
  (for ((i (in-range 10000)))
    (histogram-increment! h (random-gaussian-tail 16.0 10.0 2.0)))
  (histogram-plot h "Histogram of the Gaussian Tail Distribution"))

(let ((h (make-histogram-with-ranges-uniform 40 3.0 6.0)))
  (for ((i (in-range 10000)))
    (histogram-increment! h (random-unit-gaussian-tail 3.0)))
  (histogram-plot h "Histogram of the Unit Gaussian Tail Distribution"))