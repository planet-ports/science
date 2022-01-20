#lang racket
;;; Science Collection
;;; ode-example-1.rkt
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
    (vector-set! f 1
                 (- (- y0) (* mu y1 (- (* y0 y0) 1.0))))))

(define (main)
  (let* ((type rk4-ode-type)
         (step (make-ode-step type 2))
         (mu 10.0)
         (system (make-ode-system func #f 2 (list mu)))
         (t 0.0)
         (t1 100.0)
         (h 1.0e-2)
         (y (vector 1.0 0.0))
         (y-err (make-vector 2 0.0))
         (dydt-in (make-vector 2 0.0))
         (dydt-out (make-vector 2 0.0))
         (y0-values '())
         (y1-values '()))
    (ode-system-function-eval system t y dydt-in)
    (let loop ()
      (when (< t t1)
        (ode-step-apply step t h
                        y y-err
                        dydt-in
                        dydt-out
                        system)
        ;(printf "~a ~a ~a~n" t (vector-ref y 0) (vector-ref y 1))
        (set! y0-values (cons (vector t (vector-ref y 0)) y0-values))
        (set! y1-values (cons (vector t (vector-ref y 1)) y1-values))
        (vector-set! dydt-in 0 (vector-ref dydt-out 0))
        (vector-set! dydt-in 1 (vector-ref dydt-out 1))
        (set! t (+ t h))
        (loop)))
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