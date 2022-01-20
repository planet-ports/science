#lang racket
;;; Science Collection
;;; ode-example-2.rkt
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

(require plot
         (planet williams/science/ode-initval))

(define (func t y f params)
  (let ((mu (car params))
        (y0 (vector-ref y 0))
        (y1 (vector-ref y 1)))
    (vector-set! f 0 y1)
    (vector-set! f 1 (- (- y0) (* mu y1 (- (* y0 y0) 1.0))))))

(define (main)
  (let* ((type rk4-ode-type)
         (step (make-ode-step type 2))
         (control (control-y-new 1.0e-6 0.0))
         (evolve (make-ode-evolve 2))
         (mu 10.0)
         (system (make-ode-system func #f 2 (list mu)))
         (t (box 0.0))
         (t1 100.0)
         (h (box 1.0e-6))
         (y (vector 1.0 0.0))
         (y0-values '())
         (y1-values '()))
    (let loop ()
      (when (< (unbox t) t1)
        (ode-evolve-apply
         evolve control step system
         t t1 h y)
        (set! y0-values (cons (vector (unbox t) (vector-ref y 0)) y0-values))
        (set! y1-values (cons (vector (unbox t) (vector-ref y 1)) y1-values))
        (loop)))
    (printf "Number of iterations   = ~a~n" (ode-evolve-count evolve))
    (printf "Number of failed steps = ~a~n" (ode-evolve-failed-steps evolve))
    (printf "~a~n" (plot (points y0-values #:size 3)
                         #:x-min 0.0
                         #:x-max 100.0
                         #:x-label "x"
                         #:y-min -2.0
                         #:y-max 2.0
                         #:y-label "y"))
    (printf "~a~n" (plot (points y1-values #:size 3)
                         #:x-min 0.0
                         #:x-max 100.0
                         #:x-label "x"
                         #:y-label "y"))))

(main)