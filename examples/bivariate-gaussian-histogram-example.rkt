#lang racket
;;; Science Collection
;;; bivariate-gaussian-histogram-example.ss
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

(require (planet "bivariate-gaussian.ss" ("williams" "science.plt")
                 "random-distributions"))
(require (planet "histogram-2d-with-graphics.ss"
                 ("williams" "science.plt")))

(let ((h (make-histogram-2d-with-ranges-uniform
          20 20 -3.0 3.0 -3.0 3.0)))
  (for ((i (in-range 10000)))
    (let-values (((x y) (random-bivariate-gaussian 1.0 1.0 0.0)))
      (histogram-2d-increment! h x y)))
  (histogram-2d-plot
   h "Histogram of the Bivariate Gaussian Distribution"))
