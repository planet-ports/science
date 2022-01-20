#lang racket
;;; Science Collection
;;; statistics-example.rkt
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

(require (planet williams/science/random-distributions/gaussian)
         (planet williams/science/statistics)
         (planet williams/science/math))

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
      (w     (make-vector 1000))
      (rs (make-statistics)))
  (for ((i (in-range 1000)))
    ;; Random data from unit gaussian
    (vector-set! data1 i (random-unit-gaussian))
    (vector-set! data2 i (random-unit-gaussian))
    ;; Cos^2 weighting
    (vector-set! w i
      (expt (cos (- (* 2.0 pi (/ i 1000.0)) pi)) 2))
    ;; Running statistics
    (statistics-tally! rs (vector-ref data1 i)))
  (printf "Statistics Example~n")
  (let-values (((mu sigma-squared) (mean-and-variance data1)))
    (printf "                                mean = ~a (~a)~n"
            (mean data1) mu)
    (printf "                            variance = ~a (~a)~n"
            (variance data1) sigma-squared))
  (printf "                  standard deviation = ~a~n"
          (standard-deviation data1))
  (printf "                   variance from 0.0 = ~a~n"
          (variance-with-fixed-mean data1 0.0))
  (printf "         standard deviation from 0.0 = ~a~n"
          (standard-deviation-with-fixed-mean data1 0.0))
  (printf "                  absolute deviation = ~a~n"
          (absolute-deviation data1))
  (printf "         absolute deviation from 0.0 = ~a~n"
          (absolute-deviation data1 0.0))
  (printf "                              skew-p = ~a~n"
          (skew-p data1))
  (printf "                                skew = ~a~n"
          (skew data1))
  (printf "                          kurtosis-p = ~a~n"
          (kurtosis-p data1))
  (printf "                            kurtosis = ~a~n"
          (kurtosis data1))
  (printf "               lag-1 autocorrelation = ~a~n"
          (lag-1-autocorrelation data1))
  (printf "                          covariance = ~a~n"
          (covariance data1 data2))
  (printf "                         correlation = ~a~n"
          (correlation data1 data2))
  (printf "                       weighted mean = ~a~n"
          (weighted-mean w data1))
  (printf "                   weighted variance = ~a~n"
          (weighted-variance w data1))
  (printf "         weighted standard deviation = ~a~n"
          (weighted-standard-deviation w data1))
  (printf "          weighted variance from 0.0 = ~a~n" 
          (weighted-variance-with-fixed-mean w data1 0.0))
  (printf "weighted standard deviation from 0.0 = ~a~n" 
          (weighted-standard-deviation-with-fixed-mean w data1 0.0))
  (printf "         weighted absolute deviation = ~a~n"
          (weighted-absolute-deviation w data1))
  (printf "weighted absolute deviation from 0.0 = ~a~n"
          (weighted-absolute-deviation w data1 0.0))
  (printf "                       weighted skew = ~a~n"
          (weighted-skew w data1))
  (printf "                   weighted kurtosis = ~a~n"
          (weighted-kurtosis w data1))
  (printf "                             maximum = ~a~n"
          (maximum data1))
  (printf "                             minimum = ~a~n"
          (minimum data1))
  (printf "              index of maximum value = ~a~n"
          (maximum-index data1))
  (printf "              index of minimum value = ~a~n"
          (minimum-index data1))
  (naive-sort! data1)
  (printf "                              median = ~a~n"
          (median-from-sorted-data data1))
  (printf "                        10% quantile = ~a~n"
          (quantile-from-sorted-data data1 .1))
  (printf "                        20% quantile = ~a~n"
          (quantile-from-sorted-data data1 .2))
  (printf "                        30% quantile = ~a~n"
          (quantile-from-sorted-data data1 .3))
  (printf "                        40% quantile = ~a~n"
          (quantile-from-sorted-data data1 .4))
  (printf "                        50% quantile = ~a~n"
          (quantile-from-sorted-data data1 .5))
  (printf "                        60% quantile = ~a~n"
          (quantile-from-sorted-data data1 .6))
  (printf "                        70% quantile = ~a~n"
          (quantile-from-sorted-data data1 .7))
  (printf "                        80% quantile = ~a~n"
          (quantile-from-sorted-data data1 .8))
  (printf "                        90% quantile = ~a~n"
          (quantile-from-sorted-data data1 .9))
  (printf "~nRunning Statistics~n")
  (printf "                                   n = ~a~n"
          (statistics-n rs))
  (printf "                                 min = ~a~n"
          (statistics-min rs))
  (printf "                                 max = ~a~n"
          (statistics-max rs))
  (printf "                               range = ~a~n"
          (statistics-range rs))
  (printf "                                mean = ~a~n"
          (statistics-mean rs))
  (printf "                          variance-p = ~a~n"
          (statistics-variance-p rs))
  (printf "                standard-deviation-p = ~a~n"
          (statistics-standard-deviation-p rs))
  (printf "                                CV-p = ~a~n"
          (statistics-CV-p rs))
  (printf "                            variance = ~a~n"
          (statistics-variance rs))
  (printf "                  standard-deviation = ~a~n"
          (statistics-standard-deviation rs))
  (printf "                                  CV = ~a~n"
          (statistics-CV rs))
  (printf "                              skew-p = ~a~n"
          (statistics-skew-p rs))
  (printf "                                skew = ~a~n"
          (statistics-skew rs))
  (printf "                          kurtosis-p = ~a~n"
          (statistics-kurtosis-p rs))
  (printf "                            kurtosis = ~a~n"
          (statistics-kurtosis rs))
)