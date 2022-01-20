#lang racket
;;; Science Collection
;;; ode-example-3.rkt
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
  (let ((y0 (vector-ref y 0)))
    (vector-set!
     f 0
     (* (- 1500.0 (vector-ref y 0)) 0.12))))

(define (main)
  (let* ((type rk4-ode-type)
         (step (make-ode-step type 1))
         (control #f) ;(control-y-new 1.0e-6 0.0))
         (evolve (make-ode-evolve 1))
         (system (make-ode-system func #f 1 '()))
         (t (box 0.0))
         (t1 20.0)
         (h (box (/ 1.0 60.0)))
         (y (vector 150.0))
         (y0-values '()))
    (let loop ()
      (when (< (unbox t) t1)
        (begin
          (ode-evolve-apply
           evolve control step system
           t t1 h y)
          (set! y0-values (cons (vector (unbox t) (vector-ref y 0)) y0-values))
          (loop))))
    (printf "Number of iterations   = ~a~n" (ode-evolve-count evolve))
    (printf "Number of failed steps = ~a~n" (ode-evolve-failed-steps evolve))
    (printf "~a~n" (plot (points y0-values #:size 3)
                         #:x-min 0.0
                         #:x-max 20.0
                         #:x-label "Time"
                         #:y-min 0.0
                         #:y-max 1500.0
                         #:y-label "Temperature"
                         #:title "Ingot Temperature over Time"))))

(main)