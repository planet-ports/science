#lang racket
;;; Science Collection
;;; chebyshev-example.rkt
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
;;; -----------------------------------------------------------------------------
;;;

(require (planet williams/science/chebyshev)
         plot)

(define (f x)
  (if (< x 0.5) .25 .75))

(define (chebyshev-example n)
  (let ((cs (make-chebyshev-series-order 40))
        (y-values '())
        (y-cs-10-values '())
        (y-cs-40-values '()))
    (chebyshev-series-init cs f 0.0 1.0)
    (for ((i (in-range n)))
      (let* ((x (exact->inexact (/ i n)))
             (y (f x))
             (y-cs-10 (chebyshev-eval-n cs 10 x))
             (y-cs-40 (chebyshev-eval cs x)))
        (set! y-values (cons (vector x y) y-values))
        (set! y-cs-10-values
              (cons (vector x y-cs-10) y-cs-10-values))
        (set! y-cs-40-values
              (cons (vector x y-cs-40) y-cs-40-values))))
    (printf "~a~n" (plot (list (points (reverse y-values)
                                       #:color "blue" #:size 3)
                               (points (reverse y-cs-10-values)
                                       #:color "red" #:size 3))
                         #:x-min 0 #:x-max 1
                         #:y-min 0 #:y-max 1
                         #:title "Chebyshev Series Order 10"))
    (printf "~a~n" (plot (list (points (reverse y-values)
                                        #:color "blue" #:size 3)
                               (points (reverse y-cs-40-values)
                                        #:color "red" #:size 3))
                         #:x-min 0 #:x-max 1
                         #:y-min 0 #:y-max 1
                         #:title "Chebyshev Series Order 40"))))

(chebyshev-example 100)