#lang racket
;;; Science Collection
;;; special-functions.rkt
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

(require (planet williams/science/special-functions)
         plot)

;;; Error Function

(plot (function erf)
      #:x-min -4.0 #:x-max 4.0
      #:y-min -1.0 #:y-max 1.0
      #:title "Error Function, erf(x)")

;;; Complementary Error Function

(plot (function erfc)
      #:x-min -4.0 #:x-max 4.0
      #:y-min 0.0 #:y-max 2.0
      #:title "Complementary Error Function, erfc(x)")

;;; Hazard Function

(plot (function hazard)
      #:x-min -5.0 #:x-max 10.0
      #:y-min 0.0 #:y-max 10.0
      #:title "Hazard Function, hazard(x)")

;;; First-Order Exponential Integral

(plot (function expint-E1)
      #:x-min -4.0 #:x-max 4.0
      #:y-min -10.0 #:y-max 10.0
      #:title "Exponential Integral, E1(x)")

;;; Second-Order Exponential Integral

(plot (function expint-E2)
      #:x-min -4.0 #:x-max 4.0
      #:y-min -10.0 #:y-max 10.0
      #:title "Exponential Integral, E2(x)")

;;; General Exponential Integral

(plot (function expint-Ei)
      #:x-min -4.0 #:x-max 4.0
      #:y-min -10.0 #:y-max 10.0
      #:title "Exponential Integral, Ei(x)")

;;; Gamma Function

(plot (function gamma)
      #:x-min 0.001 #:x-max 6.0
      #:y-min 0.0 #:y-max 120.0
      #:title "Gamma Function, gamma(x)")

(plot (function gamma)
      #:x-min -0.999 #:x-max -0.001
      #:y-min -120.0 #:y-max 0.0
      #:title "Gamma Function, gamma(x)")

;;; Log Gamma Function

(plot (function lngamma)
      #:x-min 0.001 #:x-max 6.0
      #:y-min -1.0 #:y-max 5.0
      #:title "Log Gamma Function, lngamma(x)")

;;; Regulated Gamma Function

(plot (function gammastar)
      #:x-min 0.001 #:x-max 4.0
      #:y-min 0.0 #:y-max 10.0
      #:title "Regulated Gamma Function, gammastar(x)")

;;; Psi (Digamma) Function

(plot (function psi)
      #:x-min 0.001 #:x-max 5.0
      #:y-min -5.0 #:y-max 5.0
      #:title "Psi (Digamma) Function, psi(x)")

;;; Psi-1 (Trigamma) Function

(plot (function psi-1)
      #:x-min 0.001 #:x-max 5.0
      #:y-min 0.0 #:y-max 5.0
      #:title "Psi-1 (Trigamma) Function, psi-1(x)")

;;; Psi-n (Polygamma) Function

(plot (function (lambda (x) (psi-n 3 x)))
      #:x-min 0.001 #:x-max 5.0
      #:y-min 0.0 #:y-max 10.0
      #:title "Psi-n (Polygamma) Function, psi-n(3, x)")

;;; Riemann Zeta Function

(plot (function zeta)
      #:x-min -5.0 #:x-max 5.0
      #:y-min -5.0 #:y-max 5.0
      #:title "Riemann Zeta Function, zeta(x)")

;;; Hurwitz Zeta Function

(plot (function (lambda (x) (hzeta x 2.0)))
      #:x-min 1.001 #:x-max 5.0
      #:y-min 0.0 #:y-max 5.0
      #:title "Hurwitz Zeta Function, hzeta(x, 2.0)")

;;; Eta Function

(plot (function eta)
      #:x-min -10.0 #:x-max 10.0
      #:y-min -5.0 #:y-max 5.0
      #:title "Eta Function, eta(x)")

