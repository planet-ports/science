#lang racket
;;; Science Collection
;;; unchecked-science-test.rkt
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

(require (planet williams/science/science-with-graphics)
         plot)

;; Special Functions

(printf "--- Gamma Function ---~n")

(plot (function unchecked-gamma)
      #:x-min 0.0 #:x-max 6
      #:x-label "x"
      #:y-min 0 #:y-max 120
      #:y-label "y"
      #:title "Gamma Function, gamma(x)")

;; Random distributions

;; Continuous distributions

;; Beta distribution

(printf "--- Beta Distribution ---~n")

(let ((h (make-histogram-with-ranges-uniform 40 0.0 1.0)))
  (for ((i (in-range 10000)))
    (unchecked-histogram-increment! h (unchecked-random-beta 2.0 3.0)))
  (histogram-plot h "Histogram of the Beta Distribution"))

(beta-plot 2.0 3.0)

;; Bivariate Gaussian distribution

(printf "--- Bivariate Gaussian Distribution --- ~n")

(let ((h (make-histogram-2d-with-ranges-uniform
          20 20 -3.0 3.0 -3.0 3.0)))
  (for ((i (in-range 10000)))
    (let-values (((x y) (unchecked-random-bivariate-gaussian 1.0 1.0 0.0)))
      (unchecked-histogram-2d-increment! h x y)))
  (histogram-2d-plot
   h "Histogram of the Bivariate Gaussian Distribution"))

(bivariate-gaussian-plot 1.0 1.0 0.0)

;; Chi squared distribution

(printf "--- Chi Squared Distribution ---~n")

(let ((h (make-histogram-with-ranges-uniform 40 0.0 10.0)))
  (for ((i (in-range 10000)))
    (unchecked-histogram-increment! h (unchecked-random-chi-squared 3.0)))
  (histogram-plot h "Histogram of the Chi Squared Distribution"))

(chi-squared-plot 3.0)

;;  Exponential distribution

(printf "--- Exponential Distribution ---~n")

(let ((h (make-histogram-with-ranges-uniform 40 0.0 8.0)))
  (for ((i (in-range 10000)))
    (unchecked-histogram-increment! h (unchecked-random-exponential 1.0)))
  (histogram-plot h "Histogram of the Exponential Distribution"))

(exponential-plot 2.0)

;; F distribution

(printf "--- F Distribution ---~n")

(let ((h (make-histogram-with-ranges-uniform 40 0.0 10.0)))
  (for ((i (in-range 10000)))
    (unchecked-histogram-increment! h (unchecked-random-f-distribution 2.0 3.0)))
  (histogram-plot h "Histogram of the F-Distribution"))

(f-distribution-plot 2.0 3.0)

;; Flat (uniform) distribution

(printf "--- Flat (Uniform) Distribution ---~n")

(let ((h (make-histogram-with-ranges-uniform 40 1.0 4.0)))
  (for ((i (in-range 10000)))
    (unchecked-histogram-increment! h (unchecked-random-flat 1.0 4.0)))
  (histogram-plot h "Histogram of the Flat (Uniform) Distribution"))

(flat-plot 1.0 4.0)

;; Gamma distribution

(printf "--- Gamma Distribution ---~n")
         
(let ((h (make-histogram-with-ranges-uniform 40 0.0 24.0)))
  (for ((i (in-range 10000)))
    (unchecked-histogram-increment! h (unchecked-random-gamma 3.0 3.0)))
  (histogram-plot h "Histogram of the Gamma Distribution"))

(gamma-plot 3.0 3.0)

;; Gaussian distribution

(printf "--- Gaussian Distribution ---~n")

(let ((h (make-histogram-with-ranges-uniform 40 4.0 16.0)))
  (for ((i (in-range 10000)))
    (unchecked-histogram-increment! h (unchecked-random-gaussian 10.0 2.0)))
  (histogram-plot h "Histogram of the Gaussian Distribution"))

(let ((h (make-histogram-with-ranges-uniform 40 -3.0 3.0)))
  (for ((i (in-range 10000)))
    (unchecked-histogram-increment! h (unchecked-random-unit-gaussian)))
  (histogram-plot h "Histogram of the Unit Gaussian Distribution"))

(gaussian-plot 10.0 2.0)

(unit-gaussian-plot)

;; Gaussian tail distribution

(printf "--- Gaussian Tail Distribution ---~n")

(let ((h (make-histogram-with-ranges-uniform 40 16.0 22.0)))
  (for ((i (in-range 10000)))
    (unchecked-histogram-increment! h (unchecked-random-gaussian-tail 16.0 10.0 2.0)))
  (histogram-plot h "Histogram of the Gaussian Tail Distribution"))

(let ((h (make-histogram-with-ranges-uniform 40 3.0 6.0)))
  (for ((i (in-range 10000)))
    (unchecked-histogram-increment! h (unchecked-random-unit-gaussian-tail 3.0)))
  (histogram-plot h "Histogram of the Unit Gaussian Tail Distribution"))

(gaussian-tail-plot 16.0 10.0 2.0)

(unit-gaussian-tail-plot 3.0)

;; Pareto distribution

(printf "--- Pareto Distribution~n")

(let ((h (make-histogram-with-ranges-uniform 40 1.0 21.0)))
  (for ((i (in-range 10000)))
    (unchecked-histogram-increment! h (unchecked-random-pareto 1.0 1.0)))
  (histogram-plot h "Histogram of the Pareto Distribution"))

(pareto-plot 1.0 1.0)

;; T distribution

(printf "--- T Distribution ---~n")

(let ((h (make-histogram-with-ranges-uniform 40 -6.0 6.0)))
  (for ((i (in-range 10000)))
    (unchecked-histogram-increment! h (unchecked-random-t-distribution 1.0)))
  (histogram-plot h "Histogram of the t-Distribution"))

(t-distribution-plot 1.0)

;; Triangular distribution

(printf "--- Triangular Distribution ---~n")

(let ((h (make-histogram-with-ranges-uniform 40 1.0 4.0)))
  (for ((i (in-range 10000)))
    (unchecked-histogram-increment! h (unchecked-random-triangular 1.0 4.0 2.0)))
  (histogram-plot h "Histogram of the Triangular Distribution"))

(triangular-plot 1.0 4.0 2.0)

;; Discrete distributions

;; Bernoulli distribution

(printf "--- Bernoulli Distribution ---~n")

(let ((h (make-discrete-histogram)))
  (for ((i (in-range 10000)))
    (unchecked-discrete-histogram-increment! h (unchecked-random-bernoulli .6)))
  (discrete-histogram-plot h "Histogram of the Bernoulli Distribution"))

(bernoulli-plot 0.6)

;; Binomial distribution

(printf "--- Binomial Distribution ---~n")

(let ((h (make-discrete-histogram)))
  (for ((i (in-range 10000)))
    (unchecked-discrete-histogram-increment! h (unchecked-random-binomial 0.5 20)))
  (discrete-histogram-plot h "Histogram of the Binomial Distribution"))

(binomial-plot .5 20)

;; Geometric distribution

(printf "--- Geometric Distribution~n")

(let ((h (make-discrete-histogram)))
  (for ((i (in-range 10000)))
    (unchecked-discrete-histogram-increment! h (unchecked-random-geometric 0.5)))
  (discrete-histogram-plot h "Histogram of the Geometric Distribution"))

(geometric-plot 0.5)

;; Logarithmic distribution

(printf "--- Logarithmic Distribution ---~n")

(let ((h (make-discrete-histogram)))
  (for ((i (in-range 10000)))
    (unchecked-discrete-histogram-increment! h (unchecked-random-logarithmic 0.5)))
  (discrete-histogram-plot h "Histogram of the Logarithmic Distribution"))

(logarithmic-plot 0.5)

;; Poisson distribution

(printf "--- Poisson Distribution ---~n")

(let ((h (make-discrete-histogram)))
  (for ((i (in-range 10000)))
    (unchecked-discrete-histogram-increment! h (unchecked-random-poisson 10.0)))
  (discrete-histogram-plot h "Histogram of the Poisson Distribution"))

(poisson-plot 10.0)

;; General discrete distribution

(printf "--- General Discrete Distribution ---~n")

(let ((h (make-discrete-histogram))
      (d (make-discrete #(.1 .4 .9 .8 .7 .6 .5 .4 .3 .2 .1))))
  (for ((i (in-range 10000)))
    (unchecked-discrete-histogram-increment! h (unchecked-random-discrete d)))
  (discrete-histogram-plot h "Histogram of a Discrete Distribution"))

(let ((d (make-discrete #(.1 .4 .9 .8 .7 .6 .5 .4 .3 .2 .1))))
  (discrete-plot d))

;; Statistics

(define (naive-sort! data)
  (let loop ()
    (let ((n (vector-length data))
          (sorted? #t))
      (do ((i 1 (+ i 1)))
          ((= i n) data)
        (when (< (vector-ref data i)
                 (vector-ref data (- i 1)))
          (let ((t (vector-ref data i)))
            (vector-set! data i (vector-ref data (- i 1)))
            (vector-set! data (- i 1) t)
            (set! sorted? #f))))
      (unless sorted?
        (loop)))))

(let ((data1 (make-vector 1000))
      (data2 (make-vector 1000))
      (w     (make-vector 1000)))
  (for ((i (in-range 1000)))
    ;; Random data from unit gaussian
    (vector-set! data1 i (random-unit-gaussian))
    (vector-set! data2 i (random-unit-gaussian))
    ;; Cos^2 weighting
    (vector-set! w i
      (expt (cos (- (* 2.0 pi (/ i 1000.0)) pi)) 2)))
  (newline)
  (printf "--- Statistics Example ---~n")
  (printf "                                mean = ~a~n"
          (unchecked-mean data1))
  (printf "                            variance = ~a~n"
          (unchecked-variance data1))
  (printf "                  standard deviation = ~a~n"
          (unchecked-standard-deviation data1))
  (printf "                   variance from 0.0 = ~a~n"
          (unchecked-variance-with-fixed-mean data1 0.0))
  (printf "         standard deviation from 0.0 = ~a~n"
          (unchecked-standard-deviation-with-fixed-mean data1 0.0))
  (printf "                  absolute deviation = ~a~n"
          (unchecked-absolute-deviation data1))
  (printf "         absolute deviation from 0.0 = ~a~n"
          (unchecked-absolute-deviation data1 0.0))
  (printf "                                skew = ~a~n"
          (unchecked-skew data1))
  (printf "                            kurtosis = ~a~n"
          (kurtosis data1))
  (printf "               lag-1 autocorrelation = ~a~n"
          (unchecked-lag-1-autocorrelation data1))
  (printf "                          covariance = ~a~n"
          (unchecked-covariance data1 data2))
  (printf "                          correlation = ~a~n"
          (unchecked-correlation data1 data2))
  (printf "                       weighted mean = ~a~n"
          (unchecked-weighted-mean w data1))
  (printf "                   weighted variance = ~a~n"
          (unchecked-weighted-variance w data1))
  (printf "         weighted standard deviation = ~a~n"
          (unchecked-weighted-standard-deviation w data1))
  (printf "          weighted variance from 0.0 = ~a~n" 
          (unchecked-weighted-variance-with-fixed-mean w data1 0.0))
  (printf "weighted standard deviation from 0.0 = ~a~n" 
          (unchecked-weighted-standard-deviation-with-fixed-mean w data1 0.0))
  (printf "         weighted absolute deviation = ~a~n"
          (unchecked-weighted-absolute-deviation w data1))
  (printf "weighted absolute deviation from 0.0 = ~a~n"
          (unchecked-weighted-absolute-deviation w data1 0.0))
  (printf "                       weighted skew = ~a~n"
          (unchecked-weighted-skew w data1))
  (printf "                   weighted kurtosis = ~a~n"
          (unchecked-weighted-kurtosis w data1))
  (printf "                             maximum = ~a~n"
          (unchecked-maximum data1))
  (printf "                             minimum = ~a~n"
          (unchecked-minimum data1))
  (printf "              index of maximum value = ~a~n"
          (unchecked-maximum-index data1))
  (printf "              index of minimum value = ~a~n"
          (unchecked-minimum-index data1))
  (naive-sort! data1)
  (printf "                              median = ~a~n"
          (unchecked-median-from-sorted-data data1))
  (printf "                        10% quantile = ~a~n"
          (unchecked-quantile-from-sorted-data data1 .1))
  (printf "                        20% quantile = ~a~n"
          (unchecked-quantile-from-sorted-data data1 .2))
  (printf "                        30% quantile = ~a~n"
          (unchecked-quantile-from-sorted-data data1 .3))
  (printf "                        40% quantile = ~a~n"
          (unchecked-quantile-from-sorted-data data1 .4))
  (printf "                        50% quantile = ~a~n"
          (unchecked-quantile-from-sorted-data data1 .5))
  (printf "                        60% quantile = ~a~n"
          (unchecked-quantile-from-sorted-data data1 .6))
  (printf "                        70% quantile = ~a~n"
          (unchecked-quantile-from-sorted-data data1 .7))
  (printf "                        80% quantile = ~a~n"
          (unchecked-quantile-from-sorted-data data1 .8))
  (printf "                        90% quantile = ~a~n"
          (unchecked-quantile-from-sorted-data data1 .9))
  (newline)
)

;; Chebyshev

(printf "--- Chebyshev Example ---~n")

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
                                       #:size 3 #:color "blue")
                               (points (reverse y-cs-10-values)
                                       #:size 3 #:color "red"))
                         #:x-min 0 #:x-max 1
                         #:x-label "x"
                         #:y-min 0 #:y-max 1
                         #:y-label "y"
                         #:title "Chebyshev Series Order 10"))
    (printf "~a~n" (plot (list (points (reverse y-values)
                                       #:size 3 #:color "blue")
                               (points (reverse y-cs-40-values)
                                       #:size 3 #:color "red"))
                         #:x-min 0 #:x-max 1
                         #:x-label "x"
                         #:y-min 0 #:y-max 1
                         #:y-label "y"
                         #:title "Chebyshev Series Order 40"))))

(chebyshev-example 100)

;; ODE

(printf "--- Ordinary Differential Equation Solver ---~n")

(printf "~n--- ODE Example 1 ---~n")

(define (func t y f params)
  (let ((mu (car params))
        (y0 (vector-ref y 0))
        (y1 (vector-ref y 1)))
    (vector-set! f 0 y1)
    (vector-set! f 1 (- (- y0) (* mu y1 (- (* y0 y0) 1.0))))))

(define (ode-example-1)
  (let* ((type rk4-ode-type)
         (step (make-ode-step type 2))
         (mu 10.0)
         (system (make-ode-system func #f 2 (list mu)))
         (t 0.0)
         (t1 100.0)
         (h 1.0e-2)
         (y (vector 1.0 0.0))
         (y-err (make-vector 2))
         (dydt-in (make-vector 2))
         (dydt-out (make-vector 2))
         (y0-values '())
         (y1-values '()))
    (unchecked-ode-system-function-eval system t y dydt-in)
    (let loop ()
      (when (< t t1)
        (begin
          (unchecked-ode-step-apply step t h
                                    y y-err
                                    dydt-in
                                    dydt-out
                                    system)
          (set! y0-values (cons (vector t (vector-ref y 0)) y0-values))
          (set! y1-values (cons (vector t (vector-ref y 1)) y1-values))
          (vector-set! dydt-in 0 (vector-ref dydt-out 0))
          (vector-set! dydt-in 1 (vector-ref dydt-out 1))
          (set! t (+ t h))
          (loop))))
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

(ode-example-1)

(printf "--- ODE Example 2 ---~n~n")

(define (ode-example-2)
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
        (unchecked-ode-evolve-apply
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

(ode-example-2)