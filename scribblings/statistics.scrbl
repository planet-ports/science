#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribblings/icons
          (for-label racket
                     (planet williams/science/science-with-graphics)))

@title[#:tag "statistics"]{Statistics}

@local-table-of-contents[]

This chapter describes the statistical functions provided by the Science Collection.  The basic statistical functions include functions to compute the mean, variance, and standard deviation, More advanced functions allow you to calculate absolute deviation, skewness, and kurtosis, as well as the median and arbitrary percentiles.  The algorithms use recurrance relations to compute average quantities in a stable way, without large intermediate values that might overflow.

The functions described in this chapter are defined in the @filepath{statistics.rkt} file in the Science Collection and are made available using the form:

@defmodule[(planet williams/science/statistics)]

@;----------
@; Running Statistics
@;----------

@section{Running Statistics}

A running statistics object accumulates a minimal set of statistics (n, min, max, mean, variance, and standard deviation) for a set of data values. A running statistics object does not require that a sequence (e.g., list or vector) of the data value be maintained.

@defproc[(statistics? (x any/c)) boolean?]{
Returns @racket[#t] is @racket[x] is a running statistics object.}

@defproc[(make-statistics) statistics?]{
Returns a new, empty running statistics object.}

@defproc[(statistics-reset! (s statistics?)) void?]{
Resets the running statistics object @racket[s].}

@defproc[(statistics-tally! (s statistics?) (x real?)) void?]{
Updates the running statistice object @racket[s] with the value of @racket[x].}

@defproc[(statistics-n (s statistics?)) exact-nonnegative-integer?]{
Returns the number of values that have been added to the running statistics object @racket[s]. This value is zero initially and after a call to @racket[statistics-reset!].}

@defproc[(statistics-min (s statistics?)) real?]{
Returns the minimum value that has been added to the running statistics object @racket[s]. This value is @racket[+inf.0] initially and after a call to @racket[statistics-reset!].}

@defproc[(statistics-max (s statistics?)) real?]{
Returns the maximum value that has been added to the running statistics object @racket[s]. This value is @racket[-inf.0] initially and after a call to @racket[statistics-reset!].}

@defproc[(statistics-mean (s statistics?)) real?]{
Returns the arithmetic mean of the values that have been added to the running statistics object @racket[s]. This value is zero initially and after a call to @racket[statistics-reset!].}

@defproc[(statistics-variance (s statistics?)) real?]{
Returns the estimated, or sample, variance of the values that have been added to the running statistics object @racket[s]. This value is zero initially and after a call to @racket[statistics-reset!].}

@defproc[(statistics-standard-deviation (s statistics?)) real?]{
Returns the standard deviation of the values that have been added to the running statistics object @racket[s]. This is the square root of the value returned by @racket[statistics-variance].}

@section{Running Statistics Example}

This example generated 100 random numbers between 0.0 and 10.0 and maintains running statistics on the values.

@racketmod[
racket
(require (planet williams/science/statistics)
         (planet williams/science/random-distributions))

(define (test)
  (let ((stat (make-statistics)))
    (for ((i (in-range 100)))
      (statistics-tally! stat (random-flat 0.0 10.0)))
    (printf "Running Statistics Example~n")
    (printf "                 n = ~a~n" (statistics-n stat))
    (printf "               min = ~a~n" (statistics-min stat))
    (printf "               max = ~a~n" (statistics-max stat))
    (printf "              mean = ~a~n" (statistics-mean stat))
    (printf "          variance = ~a~n" (statistics-variance stat))
    (printf "standard deviation = ~a~n" (statistics-standard-deviation stat))))

(test)
]

Produces the following results.

@verbatim{
Running Statistics Example
                 n = 100
               min = 0.11100957474903939
               max = 9.938914540059452
              mean = 5.466640451797567
          variance = 8.677003172428925
standard deviation = 2.945675333846031
}

@;----------
@; Mean, Standard Deviation, and Variance
@;----------

@section{Mean, Standard Deviation, and Variance}

@defproc*[(((mean (data sequence-of-real?)) real?)
           ((unchecked-mean (data sequence-of-real?)) real?))]{
Returns the arithmetic mean of @racket{data}.}

@defproc*[(((mean-and-variance (data sequence-of-real?)) (values real? (>=/c 0.0)))
           ((unchecked-mean-and-variance (data sequence-of-real?)) (values real? (>=/c 0.0))))]{
Returns the aritnmetic mean and the estimated, or sample, variance of @racket[data] as multiple values. These values are computed in a single pass through @racket[data].}

@defproc*[(((variance (data sequence-of-real?) (mean real?)) (>=/c 0.0))
           ((unchecked-variance (data sequence-of-real?) (mean real?)) (>=/c 0.0))
           ((variance (data sequence-of-real?)) (>=/c 0.0))
           ((unchecked-variance (data sequence-of-real?)) (>=/c 0.0)))]{
Returns the estimated, or sample, variance of @racket[data] relative to the given value of @racket[mean].  If @racket[mean] is not provided, the variance is relative to the arithmetic mean and is computed in a single pass through @racket[data].}

@defproc*[(((standard-deviation (data sequence-of-real?) (mean real?)) (>=/c 0.0))
           ((unchecked-standard-deviation (data sequence-of-real?) (mean real?)) (>=/c 0.0))
           ((standard-deviation (data sequence-of-real?)) (>=/c 0.0))
           ((unchecked-standard-deviation (data sequence-of-real?)) (>=/c 0.0)))]{
Returns the estimated, or sample, standard deviation of @racket[data]relative to the given value of @racket[mean].  If @racket[mean] is not provided, the standard deviation is relative to the arithmetic mean and is computed in a single pass through @racket[data].  The @italic{standard deviation} is defined as the square root of the variance.}

@defproc*[(((sum-of-squares (data sequence-of-real?) (mean real?)) (>=/c 0.0))
           ((unchecked-sum-of-squares (data sequence-of-real?) (mean real?)) (>=/c 0.0))
           ((sum-of-squares (data sequence-of-real?)) (>=/c 0.0))
           ((unchecked-sum-of-squares (data sequence-of-real?)) (>=/c 0.0)))]{
Returns the total sum of squates of @racket[data] aout the mean.  If @racket[mean] is not provided, it is calculated by a call to @racket[(mean data)].}

@defproc*[(((variance-with-fixed-mean (data sequence-of-real?) (mean real?)) (>=/c 0.0))
           ((unchecked-variance-with-fixed-mean (data sequence-of-real?) (mean real?)) (>=/c 0.0)))]{
Returns an unbiased estimate of the variance of @racket[data] when the population mean, @racket[mean], of the underlying distribution is known @italic{a priori}.}

@defproc*[(((standard-deviation-with-fixed-mean (data sequence-of-real?) (mean real?)) (>=/c 0.0))
           ((unchecked-standard-deviation-with-fixed-mean (data sequence-of-real?) (mean real?)) (>=/c 0.0)))]{
Returns the standard deviation of @racket[data] for a fixed population mean, @racket[mean].  The result is the square root of the  @racket[variance-with-fixed-mean] function.}

@;----------
@; Absolute Deviation
@;----------

@section{Absolute Deviation}

@defproc*[(((absolute-deviation (data sequence-of-real?) (mean real?)) (>=/c 0.0))
           ((unchecked-absolute-deviation (data sequence-of-real?) (mean real?)) (>=/c 0.0))
           ((absolute-deviation (data sequence-of-real?)) (>=/c 0.0))
           ((unchecked-absolute-deviation (data sequence-of-real?)) (>=/c 0.0)))]{
Returns the absolute devistion of @racket[data] relative to the given value of the mean, @racket[mean].  If @racket[mean] is not provided, it is calculated by a call to @racket[(mean data)].  This function is also useful if you want to calculate the absolute deviation to any value other than the mean, such as zero or the median.}

@;----------
@; Higher Moments (Skewness and Kurtosis)
@;----------

@section{Higher Moments (Skewness and Kurtosis)}

@defproc*[(((skew (data sequence-of-real?) (mean real?) (sd (>=/c 0.0))) real?)
           ((unchecked-skew (data sequence-of-real?) (mean real?) (sd (>=/c 0.0))) real?)
           ((skew (data sequence-of-real?)) real?)
           ((unchecked-skew (data sequence-of-real?)) real?))]{
Returns the skewness of @racket[data] using the given values of the mean, @racket[mean], and standard deviation, @racket[sd].  The @italic{skewness} measures the symmetry of the tails of a distribution.  If @racket[mean] and @racket[sd] are not provided, they are calculated by a call to @racket[mean-and-variance].}

@defproc*[(((kurtosis (data sequence-of-real?) (mean real?) (sd (>=/c 0.0))) real?)
           ((unchecked-kurtosis (data sequence-of-real?) (mean real?) (sd (>=/c 0.0))) real?)
           ((kurtosis (data sequence-of-real?)) real?)
           ((unchecked-kurtosis (data sequence-of-real?)) real?))]{
Returns the kurtosis of @racket[data] using the given values of the mean, @racket[mean], and standard deviation, @racket[sd].  The @italic{kurtosis} measures how sharply peaked a distribution is relative to its width.  If @racket[mean] and @racket[sd] are not provided, they are calculated by a call to @racket[mean-and-variance].}

@;----------
@; Autocorrelation
@;----------

@section{Autocorrelation}

@defproc*[(((lag-1-autocorrelation (data nonempty-sequence-of-real?)
                                   (mean real?))
            real?)
           ((unchecked-lag-1-autocorrelation (data nonempty-sequence-of-real?)
                                   (mean real?))
            real?)
           ((lag-1-autocorrelation (data nonempty-sequence-of-real?))
            real?)
           ((unchecked-lag-1-autocorrelation (data nonempty-sequence-of-real?))
            real?))]{
Returns the lag-1 autocorrelation of @racket[data] using the given value of the mean, @racket[mean].  If @racket[mean] is not provided, it is calculated by a call to @racket[(mean data)].}

@;----------
@; Covariance
@;----------

@section{Covariance}

@defproc*[(((covariance (data1 nonempty-sequence-of-real?)
                        (data2 nonempty-sequence-of-real?)
                        (mean1 real?)
                        (mean2 real?))
            real?)
           ((unchecked-covariance (data1 nonempty-sequence-of-real?)
                        (data2 nonempty-sequence-of-real?)
                        (mean1 real?)
                        (mean2 real?))
            real?)
           ((covariance (data1 nonempty-sequence-of-real?)
                        (data2 nonempty-sequence-of-real?))
            real?)
           ((unchecked-covariance (data1 nonempty-sequence-of-real?)
                                  (data2 nonempty-sequence-of-real?))
            real?))]{
Returns the covariance of @racket[data1] and @racket[data2] using the given values of @racket[mean1] and @racket[mean2].  If the values of @racket[mean1] and @racket[mean2] are not given, they are calculated using calls to @racket[(mean data1)] and @racket[(mean data2)], respectively.}
                    
@;----------
@; Correlation
@;----------

@section{Correlation}

@defproc*[(((correlation (data1 nonempty-sequence-of-real?)
                         (data2 nonempty-sequence-of-real?)) real?)
           ((unchecked-correlation (data1 nonempty-sequence-of-real?)
                                   (data2 nonempty-sequence-of-real?)) real?))]{
Returns the Pearson correlation coefficient between @racket[data1] and @racket[data2].}

@;----------
@; Wighted Samples
@;----------

@section{Weighted Samples}

@defproc*[(((weighted-mean (weights sequence-of-real?)
                           (data sequence-of-real?))
            real?)
           ((unchecked-weighted-mean (weights sequence-of-real?)
                                     (data sequence-of-real?))
            real?))]{
Returns the weighted mean of @racket[data] using weights, @racket[weights].}
               
@defproc*[(((weighted-variance (weights sequence-of-real?)
                               (data sequence-of-real?)
                               (wmean real?))
            (>=/c 0.0))
           ((unchecked-weighted-variance (weights sequence-of-real?)
                                         (data sequence-of-real?)
                                         (wmean real?))
            (>=/c 0.0))
           ((weighted-variance (weights sequence-of-real?)
                               (data sequence-of-real?))
            (>=/c 0.0))
           ((unchecked-weighted-variance (weights sequence-of-real?)
                                         (data sequence-of-real?))
            (>=/c 0.0)))]{
Returns the weighted variance of @racket[data] using weights, @racket[weights], and the given weighted mean, @racket[wmean].  If @racket[wmean] is not provided, it is calculated by a call to @racket[(weighted-mean weights data)].}
                         
@defproc*[(((weighted-standard-deviation (weights sequence-of-real?)
                                         (data sequence-of-real?)
                                         (wmean real?))
            (>=/c 0.0))
           ((unchecked-weighted-standard-deviation (weights sequence-of-real?)
                                                   (data sequence-of-real?)
                                                   (wmean real?))
            (>=/c 0.0))
           ((weighted-standard-deviation (weights sequence-of-real?)
                                         (data sequence-of-real?))
            (>=/c 0.0))
           ((unchecked-weighted-standard-deviation (weights sequence-of-real?)
                                                   (data sequence-of-real?))
            (>=/c 0.0)))]{
Returns the weighted standard deviation of @racket[data] using weights, @racket[weights].  The @italic{standard deviation} is defined as the square root of the variance.  The result is the square root of the corresponding @racket[weighted-variance] function.}
               
@defproc*[(((weighted-variance-with-fixed-mean (weights sequence-of-real?)
                                               (data sequence-of-reals?)
                                               (wmean real?))
            (>=/c 0.0))
           ((unchecked-weighted-variance-with-fixed-mean (weights sequence-of-real?)
                                                         (data sequence-of-reals?)
                                                         (wmean real?))
            (>=/c 0.0)))]{
Returns an unbiased estimate of the weighted variance of @racket[data] using weights, @racket[weights], when the weighted population mean, @racket[wmean], of the underlying population is known @italic{a priori}.}
               
@defproc*[(((weighted-standard-deviation-with-fixed-mean (weights sequence-of-real?)
                                                         (data sequence-of-real?)
                                                         (wmean real?))
            (>=/c 0.0))
           ((unchecked-weighted-standard-deviation-with-fixed-mean (weights sequence-of-real?)
                                                                   (data sequence-of-real?)
                                                                   (wmean real?))
            (>=/c 0.0)))]{
Returns the weighted standard deviation of @racket[data] using weights, @racket[weights], with a fixed population mean, @racket[wmean].  The result is the square root of the  @racket[weighted-variance-with-fixed-mean] function.}
               
@defproc*[(((weighted-absolute-deviation (weights sequence-of-real?)
                                         (data sequence-of-real?)
                                         (wmean real?))
            (>=/c 0.0))
           ((unchecked-weighted-absolute-deviation (weights sequence-of-real?)
                                                   (data sequence-of-real?)
                                                   (wmean real?))
            (>=/c 0.0))
           ((weighted-absolute-deviation (weights sequence-of-real?)
                                         (data sequence-of-real?))
            (>=/c 0.0))
           ((unchecked-weighted-absolute-deviation (weights sequence-of-real?)
                                                   (data sequence-of-real?))
            (>=/c 0.0)))]{
Returns the weighted absolute devistion of @racket[data] using weights, @racket[weights], relative to the given value of the weighted mean, @racket[wmean].  If @racket[wmean] is not provided, it is calculated by a call to @racket[(weighted-mean weights data)].  This function is also useful if you want to calculate the weighted absolute deviation to any value other than the mean, such as zero or the weighted median.}

@defproc*[(((weighted-skew (weights sequence-of-real?)
                           (data sequence-of-real?)
                           (wmean real?)
                           (wsd (>=/c 0.0)))
            (>=/c 0.0))
           ((unchecked-weighted-skew (weights sequence-of-real?)
                                     (data sequence-of-real?)
                                     (wmean real?)
                                     (wsd (>=/c 0.0)))
            (>=/c 0.0))
           ((weighted-skew (weights sequence-of-real?)
                           (data sequence-of-real?))
            (>=/c 0.0))
           ((unchecked-weighted-skew (weights sequence-of-real?)
                                     (data sequence-of-real?))
            (>=/c 0.0)))]{
Returns the weighted skewness of @racket[data] using weights, @racket[weights], using the given values of the weighted mean, @racket[wmean], and weighted standard deviation, @racket[wsd].  The @italic{skewness}  measures the symmetry of the tails of a distribution.  If @racket[wmean] and @racket[wsd] are not provided, they are calculated by calls to @racket[(weighted-mean weights data)] and @racket[(weighted-standard-deviation weights data wmean)].}

@defproc*[(((weighted-kurtosis (weights sequence-of-real?)
                               (data sequence-of-real?)
                               (wmean real?)
                               (wsd (>=/c 0.0)))
            (>=/c 0.0))
           ((unchecked-weighted-kurtosis (weights sequence-of-real?)
                                         (data sequence-of-real?)
                                         (wmean real?)
                                         (wsd (>=/c 0.0)))
            (>=/c 0.0))
           ((weighted-kurtosis (weights sequence-of-real?)
                               (data sequence-of-real?))
            (>=/c 0.0))
           ((unchecked-weighted-kurtosis (weights sequence-of-real?)
                                         (data sequence-of-real?))
            (>=/c 0.0)))]{
Returns the weighted kurtosis of @racket[data] using weights, @racket[weights], using the given values of the weighted mean, @racket[wmean], and weighted standard deviation, @racket[wsd].  The @italic{kurtosis} measures how sharply peaked a distribution is relative to its width.  If @racket[wmean] and @racket[wsd] are not provided, they are calculated by calls to @racket[(weighted-mean weights data)] and @racket[(weighted-standard-deviation weights data wmean)].}

@;----------
@; Maximum and Minimum
@;----------

@section{Maximum and Minimum}

@defproc*[(((maximum (data nonempty-sequence-of-real?))
            real?)
           ((unchecked-maximum (data nonempty-sequence-of-real?))
            real?))]{
Returns the maximum value in @racket[data].}

@defproc*[(((minimum (data nonempty-sequence-of-real?))
            real?)
           ((unchecked-minimum (data nonempty-sequence-of-real?))
            real?))]{
Returns the minimum value in @racket[data].}

@defproc*[(((minimum-maximum (data nonempty-sequence-of-real?))
            (values real? real?))
           ((unchecked-minimum-maximum (data nonempty-sequence-of-real?))
            (values real? real?)))]{
Returns the minimum and maximum values on @racket[data] as multiple values.}

@defproc*[(((maximum-index (data nonempty-sequence-of-real?))
            exact-nonnegative-integer?)
           ((unchecked-maximum-index (data nonempty-sequence-of-real?))
            exact-nonnegative-integer?))]{
Returns the index of the maximum value in @racket[data].  When there are several equal maximum elements, the index of the first one is chosen.}

@defproc*[(((minimum-index (data nonempty-sequence-of-real?))
            exact-nonnegative-integer?)
           ((unchecked-minimum-index (data nonempty-sequence-of-real?))
            exact-nonnegative-integer?))]{
Returns the index of the minimum value in @racket[data].  When there are several equal minimum elements, the index of the first one is chosen.}

@defproc*[(((minimum-maximum-index (data nonempty-sequence-of-real?))
            (values exact-nonnegative-ineger? exact-nonnegative-integer?))
           ((unchecked-minimum-maximum-index (data nonempty-sequence-of-real?))
            (values exact-nonnegative-ineger? exact-nonnegative-integer?)))]{
Returns the indices of the minimum and maximum values in @racket[data] as multiple values.  When there are several equal minimum or maximum elements, the index of the first ones are chosen.}

@;----------
@; Median and Quantiles
@;----------

@section{Median and Quantiles}

Thw median and quantile functions described in this section operate on sorted data.  The contracts for these functions enforce this.  Also, for convenience we use quantiles measured on a scale of 0 to 1 instead of percentiles, which use a scale of 0 to 100).

@defproc*[(((median-from-sorted-data (sorted-data nonempty-sorted-vector-of-real?))
            real?)
           ((unchecked-median-from-sorted-data (sorted-data nonempty-sorted-vector-of-real?))
            real?))]{
Returns the median value of @racket[sorted-data].  When the dataset has an odd number of elements, the median is the value of element @math{(n - 1)/2}.  When the dataset has an even number of elements, the median is the mean of the two nearest middle values, elements @math{(n - 1)/2} and @math{n/2}.}

@defproc*[(((quantile-from-sorted-data (sorted-data nonempty-sorted-vector-of-real?)
                                       (f (real-in 0.0 1.0)))
            real?)
           ((unchecked-quantile-from-sorted-data (sorted-data nonempty-sorted-vector-of-real?)
                                                 (f (real-in 0.0 1.0)))
            real?))]{
Returns a quantile value of @racket[sorted-data].  The quantile is determined by the value @racket[f], a fraction between 0 and 1.  For example to compute the 75@superscript{th} percentile, @racket[f] should have the value 0.75.

The quantile is found by interpolation using the formula:

@math{quantile = 1 - delta(x[i]) + delta(x(i + 1))}
               
where @math{i} is @math{floor((n - 1) × f)} and @math{delta} is @math{(n - 1) × f - 1}.}

@;----------
@; Statistics Example
@;----------

@section{Statistics Example}

This example generates two vectors from a unit Gaussian distribution and a vector of conse squared weighting data.  All of the vectors are of length 1,000.  Thes data are used to test all of the statistics functions.

@racketmod[
racket
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
      (w     (make-vector 1000)))
  (for ((i (in-range 1000)))
    ;; Random data from unit gaussian
    (vector-set! data1 i (random-unit-gaussian))
    (vector-set! data2 i (random-unit-gaussian))
    ;; Cos^2 weighting
    (vector-set! w i
      (expt (cos (- (* 2.0 pi (/ i 1000.0)) pi)) 2)))
  (printf "Statistics Example~n")
  (printf "                                mean = ~a~n"
          (mean data1))
  (printf "                            variance = ~a~n"
          (variance data1))
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
  (printf "                                skew = ~a~n"
          (skew data1))
  (printf "                            kurtosis = ~a~n"
          (kurtosis data1))
  (printf "               lag-1 autocorrelation = ~a~n"
          (lag-1-autocorrelation data1))
  (printf "                          covariance = ~a~n"
          (covariance data1 data2))
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
          (quantile-from-sorted-data data1 .9)))]

Produces the following output:

@verbatim{
Statistics Example
                                mean = 0.03457693091555611
                            variance = 1.0285343857083435
                  standard deviation = 1.0141668431320083
                   variance from 0.0 = 1.028701415474174
         standard deviation from 0.0 = 1.014249188056946
                  absolute deviation = 0.7987180852601665
         absolute deviation from 0.0 = 0.7987898146946209
                                skew = 0.04340293467117837
                            kurtosis = 0.17722452271702993
               lag-1 autocorrelation = 0.0029930889831972143
                          covariance = 0.005782911085590894
                       weighted mean = 0.05096139259270008
                   weighted variance = 1.0500293763787367
         weighted standard deviation = 1.0247094107007786
          weighted variance from 0.0 = 1.0510513958491579
weighted standard deviation from 0.0 = 1.0252079768755011
         weighted absolute deviation = 0.8054378524718832
weighted absolute deviation from 0.0 = 0.8052440544958938
                       weighted skew = 0.046448729539282155
                   weighted kurtosis = 0.3050060704791675
                             maximum = 3.731148814104969
                             minimum = -3.327265864298485
              index of maximum value = 502
              index of minimum value = 476
                              median = 0.019281803306206644
                        10% quantile = -1.243869878615807
                        20% quantile = -0.7816243947573505
                        30% quantile = -0.4708703241429585
                        40% quantile = -0.2299309332835332
                        50% quantile = 0.019281803306206644
                        60% quantile = 0.30022966479982344
                        70% quantile = 0.5317978807508836
                        80% quantile = 0.832291888537874
                        90% quantile = 1.3061151234700463
}