#lang racket
;;; Science Collection
;;; radix2-dif-fft-example.rkt
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

(require plot
         (planet williams/science/math)
         (planet williams/science/statistics)
         (planet williams/science/fft))

(printf "Radix 2 Complex FFT - Decimation in Frequency~n")

;;; Data
(define data (build-vector 4096 (lambda (t) (+ (sin (* t (/ 2*pi 500))) 0.0))))
(define data-points (for/list ((j (in-vector data))
                               (i (in-naturals)))
                      (vector i j)))
(plot (points data-points #:size 3 #:color "blue")
      #:title "Data"
      #:x-min 0
      #:x-max (length data-points)
      #:y-min -1.0
      #:y-max 1.0)

;;; Radix 2 Forward FFT
(fft-complex-radix2-dif-forward data)
(define data-max (for/fold ((max-magnitude 0.0))
                            ((x (in-vector data)))
                    (max (magnitude x) max-magnitude)))
(define data-points-forward (for/list ((j (in-vector data))
                                        (i (in-naturals)))
                               (vector i (magnitude j))))
(plot (points data-points-forward #:size 3 #:color "red")
      #:title "Radix 2 Complex FFT (Forward)"
      #:x-min 0
      #:x-max (length data-points-forward)
      #:y-max data-max)

;;; Radix 2 Inverse FFT
(fft-complex-radix2-dif-inverse data)
(define data-points-inverse (for/list ((j (in-vector data))
                                        (i (in-naturals)))
                               (vector i (real-part j))))
(plot (points data-points-inverse #:size 3 #:color "blue")
      #:title "Radix 2 Complex FFT (Inverse)"
      #:x-min 0
      #:x-max (length data-points-inverse)
      #:y-min -1.0
      #:y-max 1.0)
