#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribblings/icons
          (for-label racket
                     (planet williams/science/science-with-graphics)))

@title[#:tag "chebyshev"]{Chebyshev Approximations}

@local-table-of-contents[]

This chapter describes the routines for computing Chebyshev approximations to univariate functions provided by the Science Collection.  A Chebyshev approximation is a truncation of the series,

@;@math{f(x) = Σc_nT_n(x)}
@image["scribblings/images/chebyshev-equation-1.png"]

where the Chebyshev polynomials @math{T_n(x) = cos(n arccos x)} provides an orthogonal basis of polynomials in the interval [-1, 1] with the weight function @math{1/(1 - x^2)@superscript{½}}.  The first few Chebyshev polynomials are @math{T_0(x) = 1}, @math{T_1(x) = x}, @math{T_2(x) = 2x^2 -1}.  For more information, see Abramowitz and Stegun @cite["Abramowitz64"], Chapter 22.

The functions described in this chapter are defined in the @filepath{chebyshev.rkt} file in the Science Collection and are made available using the form:

@defmodule[(planet williams/science/chebyshev)]

@section{The @racket[chebyshev-series] Structure}

A Chebyshev series is represented by a structure @racket[chebyshev-series] that has the following fields:

@itemize{
         
  @item{@racket[coefficients]---a vector of length @racket[order] containing the coefficients for the Chebyshev series.}
       
  @item{@racket[order]---the order of the Chebyshev series.}
  
  @item{@racket[lower]---the lower bound on the interval over which the Chebyshev series is defined.}
  
  @item{@racket[upper]---the upper bound on the interval over which the Chebyshev series is defined.}
  
  }

The approximations are made over the range [@racket[lower], @racket[upper]] using @racket[order] + 1 terms, including @racket[coefficient]@subscript{0}.  The series is computed using the following convention:

@image["scribblings/images/chebyshev-equation-2.png"]

which is needed when accessing the coefficients directly.

@section{Creation and Calculation of Chebyshev Series}

@defproc[(chebyshev-series? (x any/c)) boolean?]{
Returns true, @racket[#t], if @racket[x] is a Chebyshev series.}                                                 

@defproc*[(((make-chebyshev-series (order exact-nonnegative-integer?))
            chebyshev-series?)
           ((make-chebyshev-series (coeffs-or-func (or/c (vectorof real?) (-> real? real?)))
                                   (order exact-nonnegative-integer?)
                                   (lower real?)
                                   (upper real?))
            chebyshev-series?))]{
Returns a newly created Chebyshev series with the given @racket[order]. If @racket[coeffs-or-func] is supplied and is a vector of reals, these are used as the coefficients. If @racket[coeffs-or-func] is supplied and is a function, the Chebyshev approximation @racket[cs] for the function @racket[func] over the range (@racket[a], @racket[b]) to the previously specified order is computed.  The computation of the Chebyshev approximation is an O(@math{n^2}) process that requires @math{n} function evaluations.}

@defproc[(make-chebyshev-series-order (order exact-nonnegative-integer?)) chebyshev-series?]{
Returns a newly created Chebyshev series with the given @racket[order].}

@defproc[(chebyshev-series-coefficients (cs chebyshev-series?)) (vectorof real?)]{
Returns the @racket[coefficients] field of the Chebyshev series @racket[cs].}

@defproc[(chebyshev-series-order (cs chebyshev-series?)) exact-nonnegative-integer?]{
Returns the @racket[order] field of the Chebyshev series @racket[cs].}

@defproc[(chebyshev-series-lower (cs chebyshev-series?)) real?]{
Returns the @racket[lower] field of the Chebyshev series @racket[cs].}

@defproc[(chebyshev-series-upper (cs chebyshev-series?)) real?]{
Returns the @racket[upper] field of the Chebyshev series @racket[cs].}

@defproc[(chebyshev-series-init (cs chebyshev-series?)
                                (func (-> real? real?))
                                (a real?)
                                (b (>/c a)))
         void?]{
Computes the Chebyshev approximation @racket[cs] for the function @racket[func] over the range (@racket[a], @racket[b]) to the previously specified order.  The computation of the Chebyshev approximation is an O(@math{n^2}) process that requires @math{n} function evaluations.}
               
@section{Chebyshev Series Evaluations}

@defproc[(chebyshev-eval (cs chebyshev-series?) (x real?)) real?]{
Evaluates the Chebyshev series @racket[cs] at the given point @racket[x].}

@defproc[(chebyshev-eval-n (cs chebyshev-series?) (n exact-nonnegative-integer?) (x real?)) real?]{
Evaluates the Chebyshev series @racket[cs] at the given point @racket[x] to (at most) the given order @racket[n].}

@section{Derivatives and Integrals}

The following functions allow a Chebyshev series to be differentiated or integrated, producing a new Chebyshev series.

@defproc[(make-chebyshev-series-derivative (series chebyshev-series?)) chebyshev-series?]{
Returns a new Chebyshev series object that is the derivative of @racket[series]. The returned series has the same order and range as @racket[series].}

@defproc[(make-chebyshev-series-integral (series chebyshev-series?)) chebyshev-series?]{
Returns a new Chebyshev series object that is the integral of @racket[series]. The returned series has the same order and range as @racket[series].}

@section{Chebyshev Approximation Examples}

@bold{Example:} The following program computes Chebyshev approximations to a step function.  This is an extremely difficult approximation to make due to the discontinuity and was chosen as an example where approximation error is visible.  For smooth functions the Chebyshev approximation converges extremely rapidly and errors would not be visible.

@racketmod[
racket
(require (planet williams/science/chebyshev))
(require plot/plot)

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
    (printf "~a~n" (plot (mix (points y-values)
                              (points y-cs-10-values))
                         #:x-min 0 #:x-max 1
                         #:y-min 0 #:y-max 1
                         #:title "Chebyshev Series Order 10"))
    (printf "~a~n" (plot (mix (points y-values)
                              (points y-cs-40-values))
                         #:x-min 0 #:x-max 1
                         #:y-min 0 #:y-max 1
                         #:title "Chebyshev Series Order 40"))))

(chebyshev-example 100)
]

The following figures show the resulting plots.

@image["scribblings/images/cheb-cs-10-plot.png"]

@image["scribblings/images/cheb-cs-40-plot.png"]
