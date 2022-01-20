#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribblings/icons
          (for-label racket
                     (planet williams/science/science-with-graphics)))

@title[#:tag "random-distributions"]{Random Number Distributions}

@local-table-of-contents[]

This chapter describes the functions for generating random variates and computing their probability densities provided by the Science Collection/

The functions described in this chapter are defined in the @racketfont{random-distributions} sub-collection of the Science Collection.  All of the modules in the @racketfont{random-distributions} sub-collection can be made available using the form:

@;@defmodule[(planet williams/science/random-distributions)]
@racket[(require williams/science/random-distributions)]

The random distribution graphics are provided as separate modules.  To also include the random distribution graphics routines, use the following form:

@;@defmodule[(planet williams/science/random-distributions-with-graphics)]
@racket[(require williams/science/random-distributions-with-graphics)]

The individual modules in the @racketfont{random-distributions} sub-collection can also be made available as described in the sections below.

@;----------
@;  The Beta Distribution
@;----------

@section{The Beta Distribution}

@(margin-note magnify @link["http://mathworld.wolfram.com/BetaDistribution.html"]{Beta Distribution} " from Wolfram MathWorld.")

The beta distribution functions are defined in the @filepath{beta.rkt} file in the @racketfont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/beta)]

@subsection{Random Variates from the Beta Distribution}

@defproc*[(((random-beta (s random-source?) (a real?) (b real?)) (real-in 0.0 1.0))
           ((unchecked-random-beta (s random-source?) (a real?) (b real?)) (real-in 0.0 1.0))
           ((random-beta (a real?) (b real?)) (real-in 0.0 1.0))
           ((unchecked-random-beta (a real?) (b real?)) (real-in 0.0 1.0)))]{
Returns a random variate from the beta distribution with parameters @racket[a] and @racket[b] using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.}

@bold{Example:} Histogram of random variates from the beta distribution with parameters @math{a = 2.0} and @math{b = 3.0}.

@racketmod[
racket
(require (planet williams/science/random-distributions/beta)
         (planet williams/science/histogram-with-graphics))

(let ((h (make-histogram-with-ranges-uniform 40 0.0 1.0)))
  (for ((i (in-range 10000)))
    (histogram-increment! h (random-beta 2.0 3.0)))
  (histogram-plot h "Histogram of the Beta Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/beta-hist.png"]

@subsection{Beta Distribution Density Functions}

@defproc*[(((beta-pdf (x real?) (a real?) (b real?)) (>=/c 0.0))
           ((unchecked-beta-pdf (x real?) (a real?) (b real?)) (>=/c 0.0)))]{
Computes the probability density, @math{p(x)}, at @racket[x] for the beta distribution with parameters @racket[a] and @racket[b].}

@defproc*[(((beta-cdf (x real?) (a real?) (b real?)) (real-in 0.0 1.0))
           ((unchecked-beta-cdf (x real?) (a real?) (b real?)) (real-in 0.0 1.0)))]{
Computes the cumulative density, @math{d(x)}, at @racket[x] for the beta distribution with parameters @racket[a] and @racket[b].}

@subsection{Beta Distribution Graphics}

The beta distribution graphics are defined in the @filepath{beta-graphics.rkt} file in the @racketfont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/beta-graphics)]

@defproc[(beta-plot (a real?) (b real?)) any]{
Returns a plot of the probability density and cumulative density of the beta distribution with parameters @racket[a] and @racket[b].  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the beta distribution with parameters @math{a = 2.0} and @math{b = 3.0}.

@racketmod[
racket
(require (planet williams/science/random-distributions/beta-graphics))

(beta-plot 2.0 3.0)]

The following figure shows the resulting plot:

@image["scribblings/images/beta.png"]

@;----------
@;  The Bivariate Gaussian Distribution
@;----------

@section{The Bivariate Gaussian Distribution}

@(margin-note magnify @link["http://mathworld.wolfram.com/BivariateNormalDistribution.html"]{Bivariate Normal Distribution} " from Wolfram MathWorld.")

The bivariate Gaussian distribution functions are defined in the @filepath{bivariate-gaussian.rkt} file in the @racketfont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/bivariate-gaussian)]

@subsection{Random Variates from the Bivariate Haussian Distribution}

@defproc*[(((random-bivariate-gaussian (s random-source?) (sigma-x (>=/c 0.0)) (sigma-y (>=/c 0.0)) (rho (real-in -1.0 1.0)))
            (values real? real?))
           ((unchecked-random-bivariate-gaussian (s random-source?) (sigma-x (>=/c 0.0)) (sigma-y (>=/c 0.0)) (rho (real-in -1.0 1.0)))
            (values real? real?))
           ((random-bivariate-gaussian (sigma-x (>=/c 0.0)) (sigma-y (>=/c 0.0)) (rho (real-in -1.0 1.0)))
            (values real? real?))
           ((unchecked-random-bivariate-gaussian (sigma-x (>=/c 0.0)) (sigma-y (>=/c 0.0)) (rho (real-in -1.0 1.0)))
            (values real? real?)))]{
Returns a pair of correlated Gaussian variates, with mean 0, correlation coefficient @racket[rho], and standard deviations @racket[sigma-x] and @racket[sigma-y] in the @math{x} and @math{y} directions using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.  The correlation coefficient @racket[rho] must lie between -1 and 1.}

@bold{Example:} 2D histogram of random variates from the bivariate Gaussian distribution with standard deviation 1.0 in both the @math{x} and @math{y} direction and correlation coefficient 0.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/bivariate)
         (planet williams/science/histogram-2d-with-graphics))

(let ((h (make-histogram-2d-with-ranges-uniform
          20 20 -3.0 3.0 -3.0 3.0)))
  (for ((i (in-range 10000)))
    (let-values (((x y) (random-bivariate-gaussian 1.0 1.0 0.0)))
      (histogram-2d-increment! h x y)))
  (histogram-2d-plot h "Histogram of the Bivariate Gaussian Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/bivariate-gaussian-hist.png"]

@subsection{Bivariate Gaussian Distribution Density Functions}

@defproc*[(((bivariate-gaussian-pdf (x real?) (y real?) (sigma-x (>=/c 0.0)) (sigma-y (>=/c 0.0)) (rho (real-in -1.0 1.0)))
            (>=/c 0.0))
           ((unchecked-bivariate-gaussian-pdf (x real?) (y real?) (sigma-x (>=/c 0.0)) (sigma-y (>=/c 0.0)) (rho (real-in -1.0 1.0)))
            (>=/c 0.0)))]{
Computes the probability density, @math{p(x, y)}, at @math{(x, y)} for the bivariate gaussian distribution with mean 0, correlation coefficient @racket[rho], and standard deviations @racket[sigma-x] and @racket[sigma-y] in the @math{x} and @math{y} directions.}

@subsection{Bivariate Gaussian Distribution Graphics}

The bivariate Gaussian distribution graphics are defined in the @filepath{bivariate-gaussian-graphics.rkt} file in the @racketfont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/bivariate-gaussian-graphics)]

@defproc[(bivariate-gaussian-plot (sigma-x (>=/c 0.0)) (sigma-y (>=/c 0.0)) (rho (real-in -1.0 1.0))) any]{
Returns a plot of the probability density and cumulative density of the bivariate Gaussian distribution with mean 0, correlation coefficient @racket[rho], and standard deviations @racket[sigma-x] and @racket[sigma-y] in the @math{x} and @math{y} directions.  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the bivariate Gaussian distribution mean 0, correlation coefficient 0.0, and standard deviations 1.0 and 1.0 in the @math{x} and @math{y} directions.

@racketmod[
racket
(require (planet williams/science/random-distributions/binomial-gaussian-graphics))

(bivariate-gaussian-plot 1.0 1.0 0.0)]

The following figure shows the resulting plot:

@image["scribblings/images/bivariate-gaussian.png"]

@;----------
@;  The Chi-Squared Distribution
@;----------

@section{The Chi-Squared Distribution}

@(margin-note magnify @link["http://mathworld.wolfram.com/Chi-SquaredDistribution.html"]{Chi-Squared Distribution} " from Wolfram MathWorld.")

The chi-squared distribution functions are defined in the @filepath{chi-squared.rkt} file in the @racketfont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/chi-squared)]

@subsection{Random Variates from the Chi-Squared Distribution}

@defproc*[(((random-chi-squared (s random-source?) (nu real?)) (>=/c 0.0))
           ((unchecked-random-chi-squared (s random-source?) (nu real?)) (>=/c 0.0))
           ((random-chi-squared (nu real?)) (>=/c 0.0))
           ((unchecked-random-chi-squared (nu real?)) (>=/c 0.0)))]{
Returns a random variate from the chi-squared distribution with @racket[nu] degrees of freedom using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.}

@bold{Example:} Histogram of random variates from the chi-squared distribution with 3.0 degrees of freedom.

@racketmod[
racket
(require (planet williams/science/random-distributions/chi-squared)
         (planet williams/science/histogram-with-graphics))

(let ((h (make-histogram-with-ranges-uniform 40 0.0 10.0)))
  (for ((i (in-range 10000)))
    (histogram-increment! h (random-chi-squared 3.0)))
  (histogram-plot h "Histogram of the Chi-Squared Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/chi-squared-hist.png"]

@subsection{Chi-Squared Distribution Density Functions}

@defproc*[(((chi-squared-pdf (x real?) (nu real?)) (>=/c 0.0))
           ((unchecked-chi-squared-pdf (x real?) (nu real?)) (>=/c 0.0)))]{
Computes the probability density, @math{p(x)}, at @racket[x] for the chi-squared distribution with @racket[nu] degrees of freedom.}

@defproc*[(((chi-squared-cdf (x real?) (nu real?)) (real-in 0.0 1.0))
           ((unchecked-chi-squared-cdf (x real?) (nu real?)) (real-in 0.0 1.0)))]{
Computes the cumulative density, @math{d(x)}, at @racket[x] for the chi-squared distribution with @racket[nu] degrees of freedom.}

@subsection{Chi-Squared Distribution Graphics}

The chi-squared distribution graphics are defined in the @filepath{chi-squared-graphics.rkt} file in the @racketfont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/chi-squared-graphics)]

@defproc[(chi-squared-plot (nu real?)) any]{
Returns a plot of the probability density and cumulative density of the chi-squared distribution with @racket[nu] degrees of freedom.  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the chi-squared distribution with 3.0 degrees of freedom.

@racketmod[
racket
(require (planet williams/science/random-distributions/chi-squared-graphics))

(chi-squared-plot 3.0)]

The following figure shows the resulting plot:

@image["scribblings/images/chi-squared.png"]

@;----------
@;  The Exponential Distribution
@;----------

@section{The Exponential Distribution}

@(margin-note magnify @link["http://mathworld.wolfram.com/ExponentialDistribution.html"]{Exponential Distribution} " from Wolfram MathWorld.")

The exponential distribution functions are defined in the @filepath{exponential.rkt} file in the @schemefont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/exponential)]

@subsection{Random Variates from the Exponential Distribution}

@defproc*[(((random-exponential (s random-source?) (mu (>/c 0.0))) (>=/c 0.0))
           ((unchecked-random-exponential (s random-source?) (mu (>/c 0.0))) (>=/c 0.0))
           ((random-exponential (mu (>/c 0.0))) (>=/c 0.0))
           ((unchecked-random-exponential (mu (>/c 0.0))) (>=/c 0.0)))]{
Returns a random variate from the exponential distribution with mean @racket[mu] using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.}

@bold{Example:} Histogram of random variates from the exponential distribution with mean 1.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/exponential)
         (planet williams/science/histogram-with-graphics))

(let ((h (make-histogram-with-ranges-uniform 40 0.0 8.0)))
  (for ((i (in-range 10000)))
    (histogram-increment! h (random-exponential 1.0)))
  (histogram-plot h "Histogram of the Exponential Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/exponential-hist.png"]

@subsection{Exponential Distribution Density Functions}

@defproc*[(((exponential-pdf (x real?) (mu (>/c 0.0))) (>=/c 0.0))
           ((unchecked-exponential-pdf (x real?) (mu (>/c 0.0))) (>=/c 0.0)))]{
Computes the probability density, @math{p(x)}, at @racket[x] for the exponential distribution with mean @racket[mu].}

@defproc*[(((exponential-cdf (x real?) (mu (>/c 0.0))) (real-in 0.0 1.0))
           ((unchecked-exponential-cdf (x real?) (mu (>/c 0.0))) (real-in 0.0 1.0)))]{
Computes the cumulative density, @math{d(x)}, at @racket[x] for the exponential distribution with mean @racket[mu].}

@subsection{Exponential Distribution Graphics}

The exponential distribution graphics are defined in the @filepath{exponential-graphics.rkt} file in the @racketfont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/exponential-graphics)]

@defproc[(exponential-plot (mu (>/c 0.0))) any]{
Returns a plot of the probability density and cumulative density of the exponential distribution with mean @racket[mu].  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the exponential distribution with mean 3.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/exponential-graphics))

(exponential-plot 3.0)]

The following figure shows the resulting plot:

@image["scribblings/images/exponential.png"]

@;----------
@;  The F-Distribution
@;----------

@section{The F-Distribution}

@(margin-note magnify @link["http://mathworld.wolfram.com/F-Distribution.html"]{F-Distribution} " from Wolfram MathWorld.")

The F-distribution functions are defined in the @filepath{f-distribution.rkt} file in the @racketfont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/f-distribution)]

@subsection{Random Variates from the F-Distribution}

@defproc*[(((random-f-distribution (s random-source?) (nu1 real?) (nu2 real?)) (>=/c 0.0))
           ((unchecked-random-f-distribution (s random-source?) (nu1 real?) (nu2 real?)) (>=/c 0.0))
           ((random-f-distribution (nu1 real?) (nu2 real?)) (>=/c 0.0))
           ((unchecked-random-f-distribution (nu1 real?) (nu2 real?)) (>=/c 0.0)))]{
Returns a random variate from the F-distribution with @racket[nu1] and @racket[nu2] degrees of freedom using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.}

@bold{Example:} Histogram of random variates from the F-distribution with 2.0 and 3.0 degrees of freedom.

@racketmod[
racket
(require (planet williams/science/random-distributions/f-distribution)(planet williams/science/histogram-with-graphics))

(let ((h (make-histogram-with-ranges-uniform 40 0.0 10.0)))
  (for ((i (in-range 10000)))
    (histogram-increment! h (random-f-distribution 2.0 3.0)))
  (histogram-plot h "Histogram of the F-Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/f-distribution-hist.png"]

@subsection{F-Distribution Density Functions}

@defproc*[(((f-distribution-pdf (x real?) (nu1 real?) (nu2 real?)) (>=/c 0.0))
           ((unchecked-f-distribution-pdf (x real?) (nu1 real?) (nu2 real?)) (>=/c 0.0)))]{
Computes the probability density, @math{p(x)}, at @racket[x] for the F-distribution with @racket[nu1] and @racket[nu2] degrees of freedom.}

@defproc*[(((f-distribution-cdf (x real?) (nu1 real?) (nu2 real?)) (real-in 0.0 1.0))
           ((unchecked-f-distribution-cdf (x real?) (nu1 real?) (nu2 real?)) (real-in 0.0 1.0)))]{
Computes the cumulative density, @math{d(x)}, at @racket[x] for the F-distribution with @racket[nu1] and @racket[nu2] degrees of freedom.}

@subsection{F-Distribution Graphics}

The F-distribution graphics are defined in the @filepath{f-distribution-graphics.rkt} file in the @schemefont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/f-distribution-graphics)]

@defproc[(f-distribution-plot (nu1 real?) (nu2 real?)) any]{
Returns a plot of the probability density and cumulative density of the F-distribution with @racket[nu1] and @racket[nu2] degrees of freedom.  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the Fdistribution with 2.0 and 3.0 degrees of freedom.

@racketmod[
racket
(require (planet williams/science/random-distributions/f-distribution-graphics))

(f-distribution-plot 2.0 3.0)]

The following figure shows the resulting plot:

@image["scribblings/images/f-distribution.png"]

@;----------
@;  The Flat (Uniform) Distribution
@;----------

@section{The Flat (Uniform) Distribution}

@(margin-note magnify @link["http://mathworld.wolfram.com/UniformDistribution.html"]{Uniform Distribution} " from Wolfram MathWorld.")

The flat (uniform) distribution functions are defined in the @filepath{flat.rkt} file in the @racketfont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/flat)]

Note that the name flat is used because uniform is already used for the more primitive random number functions in SRFI 27.  Note that also matches the convention in the GNU Scientific Library @cite["GSL-RM"].

@subsection{Random Variates from the Flat (Uniform) Distribution}

@defproc*[(((random-flat (s random-source?) (a real?) (b (>/c a))) real?)
           ((unichecked-random-flat (s random-source?) (a real?) (b (>/c a))) real?)
           ((random-flat (a real?) (b (>/c a))) real?)
           ((unchecked-random-flat (a real?) (b (>/c a))) real?))]{
Returns a random variate from the flat (uniform) distribution from @racket[a] to @racket[b] using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.}

@bold{Example:} Histogram of random variates from the flat (uniform) distribution from 1.0 to 4.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/flat)
         (planet williams/science/histogram-with-graphics))

(let ((h (make-histogram-with-ranges-uniform 40 1.0 4.0)))
  (for ((i (in-range 10000)))
    (histogram-increment! h (random-flat 1.0 4.0)))
  (histogram-plot h "Histogram of the Flat (Uniform) Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/flat-hist.png"]

@subsection{Flat (Uniform) Distribution Density Functions}

@defproc*[(((flat-pdf (x real?) (a real?) (b (>/c a))) (>=/c 0.0))
           ((unchecked-flat-pdf (x real?) (a real?) (b (>/c a))) (>=/c 0.0)))]{
Computes the probability density, @math{p(x)}, at @racket[x] for the flat (uniform) distribution from @racket[a] to @racket[b].}

@defproc*[(((flat-cdf (x real?) (a real?) (b (>/c a))) (real-in 0.0 1.0))
           ((unchecked-flat-cdf (x real?) (a real?) (b (>/c a))) (real-in 0.0 1.0)))]{
Computes the cumulative density, @math{d(x)}, at @racket[x] for the flat (uniform) distribution from @racket[a] to @racket[b].}

@subsection{Flat (Uniform) Distribution Graphics}

The flat (uniform) distribution graphics are defined in the @filepath{flat-graphics.rkt} file in the @racketfont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/flat-graphics)]

@defproc[(flat-plot (a real?) (b (>/c a))) any]{
Returns a plot of the probability density and cumulative density of the flat (uniform) distribution from @racket[a] to @racket[b].  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the flat (uniform) distribution from 1.0 to 4.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/flat-graphics))

(flat-plot 1.0 4.0)]

The following figure shows the resulting plot:

@image["scribblings/images/flat.png"]

@;----------
@;  The Gamma Distribution
@;----------

@section{The Gamma Distribution}

@(margin-note magnify @link["http://mathworld.wolfram.com/GammaDistribution.html"]{Gamma Distribution} " from Wolfram MathWorld.")

The gamma distribution functions are defined in the @filepath{gamma.rkt} file in the @racketfont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/gamma)]

@subsection{Random Variates from the Gamma Distribution}

@defproc*[(((random-gamma (s random-source?) (a (>/c 0.0)) (b real?)) (>=/c 0.0))
           ((unchecked-random-gamma (s random-source?) (a (>/c 0.0)) (b real?)) (>=/c 0.0))
           ((random-gamma (a (>/c 0.0)) (b real?)) (>=/c 0.0))
           ((unchecked-random-gamma (a (>/c 0.0)) (b real?)) (>=/c 0.0)))]{
Returns a random variate from the gamma distribution with parameters @racket[a] and @racket[b] using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.}

@bold{Example:} Histogram of random variates from the gamma distribution with parameters 3.0 and 3.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/gamma)(planet williams/science/histogram-with-graphics))

(let ((h (make-histogram-with-ranges-uniform 40 0.0 24.0)))
  (for ((i (in-range 10000)))
    (histogram-increment! h (random-gamma 3.0 3.0)))
  (histogram-plot h "Histogram of the Gamma Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/gamma-hist.png"]

@subsection{Gamma Distribution Density Functions}

@defproc*[(((gamma-pdf (x real?) (a (>=/c 0.0)) (b real?)) (>=/c 0.0))
           ((unchecked-gamma-pdf (x real?) (a (>=/c 0.0)) (b real?)) (>=/c 0.0)))]{
Computes the probability density, @math{p(x)}, at @racket[x] for the gamma distribution with parameters @racket[a] and @racket[b].}

@defproc*[(((gamma-cdf (x real?) (a (>=/c 0.0)) (b real?)) (real-in 0.0 1.0))
           ((unchecked-gamma-cdf (x real?) (a (>=/c 0.0)) (b real?)) (real-in 0.0 1.0)))]{
Computes the cumulative density, @math{d(x)}, at @racket[x] for the gamma distribution with parameters @racket[a] and @racket[b].}

@subsection{Gamma Distribution Graphics}

The gamma distribution graphics are defined in the @filepath{gamma-graphics.rkt} file in the @racketfont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/gamma-graphics)]

@defproc[(gamma-plot (a (>=/c 0.0)) (b real?)) any]{
Returns a plot of the probability density and cumulative density of the gamma distribution with parameters @racket[a] and @racket[b].  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the gamma distribution with parameters 3.0 and 3.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/gamma-graphics))

(gamma-plot 3.0 3.0)]

The following figure shows the resulting plot:

@image["scribblings/images/gamma-dist.png"]

@;----------
@;  The Gaussian (Normal) Distribution
@;----------

@section{The Gaussian (Normal) Distribution}

@(margin-note magnify @link["http://mathworld.wolfram.com/NormalDistribution.html"]{Normal Distribution} " from Wolfram MathWorld.")

The Gaussian (normal) distribution functions are defined in the @filepath{gaussian.rkt} file in the @racketfont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/gaussian)]

@subsection{Random Variates from the Gaussian (Normal) Distribution}

@defproc*[(((random-gaussian (s random-source?) (mu real?) (sigma (>=/c 0.0))) real?)
           ((unchecked-random-gaussian (s random-source?) (mu real?) (sigma (>=/c 0.0))) real?)
           ((random-gaussian (mu real?) (sigma (>=/c 0.0))) real?)
           ((unchecked-random-gaussian (mu real?) (sigma (>=/c 0.0))) real?))]{
Returns a random variate from the Gaussian (normal) distribution with mean @racket[mu] and standard deviation @racket[sigma] using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.  This function uses the Box-Mueller algorithm that requires two calls to the random source @racket[s].}

@bold{Example:} Histogram of random variates from the Gaussian (normal) distribution with mean 10.0 and standard deviation 2.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/gaussian)
         (planet williams/science/histogram-with-graphics))

(let ((h (make-histogram-with-ranges-uniform 40 4.0 16.0)))
  (for ((i (in-range 10000)))
    (histogram-increment! h (random-gaussian 10.0 2.0)))
  (histogram-plot h "Histogram of the Gaussian (Normal) Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/gaussian-hist.png"]

@defproc*[(((random-unit-gaussian (s random-source?)) real?)
           ((unchecked-random-unit-gaussian (s random-source?)) real?)
           ((random-unit-gaussian) real?)
           ((unchecked-random-unit-gaussian) real?))]{
Returns a random variate from the Gaussian (normal) distribution with mean @math{0.0} and standard deviation @math{1.0} using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.  This function uses the Box-Mueller algorithm that requires two calls to the random source @racket[s].}

@bold{Example:} Histogram of random variates from the unit Gaussian (normal) distribution.

@racketmod[
racket
(require (planet williams/science/random-distributions/gaussian)
         (planet williams/science/histogram-with-graphics))

(let ((h (make-histogram-with-ranges-uniform 40 -3.0 3.0)))
  (for ((i (in-range 10000)))
    (histogram-increment! h (random-unit-gaussian)))
  (histogram-plot h "Histogram of the Unit Gaussian (Normal) Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/unit-gaussian-hist.png"]

@defproc*[(((random-gaussian-ratio-method (s random-source?) (mu real?) (sigma (>=/c 0.0))) real?)
           ((unchecked-random-gaussian-ratio-method (s random-source?) (mu real?) (sigma (>=/c 0.0))) real?)
           ((random-gaussian-ratio-method (mu real?) (sigma (>=/c 0.0))) real?)
           ((unchecked-random-gaussian-ratio-method (mu real?) (sigma (>=/c 0.0))) real?))]{
Returns a random variate from the Gaussian (normal) distribution with mean @racket[mu] and standard deviation @racket[sigma] using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.  This function uses the Kinderman-Monahan ratio method.}

@defproc*[(((random-unit-gaussian-ratio-method (s random-source?)) real?)
           ((unchecked-random-unit-gaussian-ratio-method (s random-source?)) real?)
           ((random-unit-gaussian-ratio-method) real?)
           ((unchecked-random-unit-gaussian-ratio-method) real?))]{
Returns a random variate from the Gaussian (normal) distribution with mean @math{0.0} and standard deviation @math{1.0} using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.  This function uses the Kinderman-Monahan ratio method.}

@subsection{Gaussian (Normal) Distribution Density Functions}

@defproc*[(((gaussian-pdf (x real?) (mu real?) (sigma (>=/c 0.0))) (>=/c 0.0))
           ((unchecked-gaussian-pdf (x real?) (mu real?) (sigma (>=/c 0.0))) (>=/c 0.0)))]{
Computes the probability density, @math{p(x)}, at @racket[x] for the Gaussian (normal) distribution with mean @racket[mu] and standard deviation @racket[sigma].}

@defproc*[(((gaussian-cdf (x real?) (mu real?) (sigma (>=/c 0.0))) (real-in 0.0 1.0))
           ((unchecked-gaussian-cdf (x real?) (mu real?) (sigma (>=/c 0.0))) (real-in 0.0 1.0)))]{
Computes the cumulative density, @math{d(x)}, at @racket[x] for the Gaussian (normal) distribution with mean @racket[mu] and standard deviation @racket[sigma].}

@defproc*[(((unit-gaussian-pdf (x real?)) (>=/c 0.0))
           ((unchecked-unit-gaussian-pdf (x real?)) (>=/c 0.0)))]{
Computes the probability density, @math{p(x)}, at @racket[x] for the unit Gaussian (normal) distribution.}

@defproc*[(((unit-gaussian-cdf (x real?)) (real-in 0.0 1.0))
           ((unchecked-unit-gaussian-cdf (x real?)) (real-in 0.0 1.0)))]{
Computes the cumulative density, @math{d(x)}, at @racket[x] for the unit Gaussian (normal) distribution.}

@subsection{Gaussian (Normal) Distribution Graphics}

The Gaussian (normal) distribution graphics are defined in the @filepath{gaussian-graphics.rkt} file in the @racketfont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/gaussian-graphics)]

@defproc[(gaussian-plot (mu real?) (sigma (>=/c 0.0))) any]{
Returns a plot of the probability density and cumulative density of the Gaussian (normal) distribution with mean @racket[mu] and standard deviation @racket[sigma].  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the Gaussian (normal) distribution with parameters mean 10.0 and standard deviation 2.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/gaussian-graphics))

(gaussian-plot 10.0 2.0)]

The following figure shows the resulting plot:

@image["scribblings/images/gaussian.png"]

@defproc[(unit-gaussian-plot) any]{
Returns a plot of the probability density and cumulative density of the unit Gaussian (normal) distribution.  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the unit Gaussian (normal) distribution.

@racketmod[
racket
(require (planet williams/science/random-distributions/gaussian-graphics))

(unit-gaussian-plot)]

The following figure shows the resulting plot:

@image["scribblings/images/unit-gaussian.png"]

@;----------
@;  The Gaussian Tail Distribution
@;----------

@section{The Gaussian Tail Distribution}

The Gaussian tail distribution functions are defined in the @filepath{gaussian-tail.rkt} file in the @racketfont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/gaussian-tail)]

@subsection{Random Variates from the Gaussian Tail Distribution}

@defproc*[(((random-gaussian-tail (s random-source?) (a real?) (mu real?) (sigma (>=/c 0.0))) real?)
           ((unchecked-random-gaussian-tail (s random-source?) (a real?) (mu real?) (sigma (>=/c 0.0))) real?)
           ((random-gaussian-tail (a real?) (mu real?) (sigma (>=/c 0.0))) real?)
           ((unchecked-random-gaussian-tail (a real?) (mu real?) (sigma (>=/c 0.0))) real?))]{
Returns a random variate from the upper tail of the Gaussian distribution with mean @racket[mu] and standard deviation @racket[sigma] using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.  The value returned is larger than the lower limit @racket[a], which must be greater than the mean @racket[mu].}

@bold{Example:} Histogram of random variates from the upper tail greater than 16.0 of the Gaussian distribution with mean 10.0 and standard deviation 2.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/gaussian)
         (planet williams/science/histogram-with-graphics))

(let ((h (make-histogram-with-ranges-uniform 40 16.0 22.0)))
  (for ((i (in-range 10000)))
    (histogram-increment! h (random-gaussian-tail 16.0 10.0 2.0)))
  (histogram-plot h "Histogram of the Gaussian Tail Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/gaussian-tail-hist.png"]

@defproc*[(((random-unit-gaussian-tail (s random-source?) (a (>/c 0.0))) real?)
           ((unchecked-random-unit-gaussian-tail (s random-source?) (a (>/c 0.0))) real?)
           ((random-unit-gaussian-tail (a (>/c 0.0))) real?)
           ((unchecked-random-unit-gaussian-tail (a (>/c 0.0))) real?))]{
Returns a random variate from the upper tail of the Gaussian distribution with mean @math{0} and standard deviation @math{1} using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.  The value returned is larger than the lower limit @racket[a], which must be greater than the mean @racket[mu].}

@subsection{Gaussian Tail Distribution Density Functions}

@defproc*[(((gaussian-tail-pdf (x real?) (a real?) (mu real?) (sigma (>=/c 0.0))) (>=/c 0.0))
           ((unchecked-gaussian-tail-pdf (x real?) (a real?) (mu real?) (sigma (>=/c 0.0))) (>=/c 0.0)))]{
Computes the probability density, @math{p(x)}, at @racket[x] for the upper tail greater than @racket[a] of the Gaussian distribution with mean @racket[mu] and standard deviation @racket[sigma].}

@defproc*[(((unit-gaussian-tail-pdf (x real?) (a (>/c 0.0))) (>=/c 0.0))
           ((unit-gaussian-tail-pdf (x real?) (a (>/c 0.0))) (>=/c 0.0)))]{
Computes the probability density, @math{p(x)}, at @racket[x] for the upper tail greater than @racket[a] of the unit Gaussian distribution.}

@subsection{Gaussian Tail Distribution Graphics}

The Gaussian tail distribution graphics are defined in the @filepath{gaussian-tail-graphics.rkt} file in the @schemefont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/gaussian-tail-graphics)]

@defproc[(gaussian-tail-plot (a real?) (mu real?) (sigma (>=/c 0.0))) any]{
Returns a plot of the probability density and cumulative density of the upper tail greater than @racket[a] of the Gaussian distribution with mean @racket[mu] and standard deviation @racket[sigma].  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the upper tail greater than 16.0 of the Gaussian distribution with parameters mean 10.0 and standard deviation 2.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/gaussian-tail-graphics))

(gaussian-plot 16.0 10.0 2.0)]

The following figure shows the resulting plot:

@image["scribblings/images/gaussian-tail.png"]

@defproc[(unit-gaussian-tail-plot (a (>/c 0.0))) any]{
Returns a plot of the probability density and cumulative density of the upper tail greater than @racket[a] of the unit Gaussian distribution.  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the upper tail greater than 3.0 of the unit Gaussian (normal) distribution.

@racketmod[
racket
(require (planet williams/science/random-distributions/gaussian-tail-graphics))

(unit-gaussian-tail-plot 3.0)]

The following figure shows the resulting plot:

@image["scribblings/images/unit-gaussian-tail.png"]

@;----------
@;  The Log Normal Distribution
@;----------

@section{The Log Normal Distribution}

@(margin-note magnify @link["http://mathworld.wolfram.com/LogNormalDistribution.html"]{Log Normal Distribution} " from Wolfram MathWorld.")

The log normal distribution functions are defined in the @filepath{lognormal.rkt} file in the @racketfont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/lognormal)]

@subsection{Random Variates from the Log Normal Distribution}

@defproc*[(((random-lognormal (s random-source?) (mu real?) (sigma (>=/c 0.0))) real?)
           ((unchecked-random-lognormal (s random-source?) (mu real?) (sigma (>=/c 0.0))) real?)
           ((random-lognormal (mu real?) (sigma (>=/c 0.0))) real?)
           ((unchecked-random-lognormal (mu real?) (sigma (>=/c 0.0))) real?))]{
Returns a random variate from the log normal distribution with mean @racket[mu] and standard deviation @racket[sigma] using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.}

@bold{Example:} Histogram of random variates from the log normal distribution with parameters mean 0.0 and standard deviation 1.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/lognormal)
         (planet williams/science/histogram-with-graphics))

(let ((h (make-histogram-with-ranges-uniform 40 0.0 6.0)))
  (for ((i (in-range 10000)))
    (histogram-increment! h (random-log-normal 0.0 1.0)))
  (histogram-plot h "Histogram of the Log Normal Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/lognormal-hist.png"]

@subsection{Log Normal Distribution Density Functions}

@defproc*[(((lognormal-pdf (x real?) (mu real?) (sigma (>=/c 0.0))) (>=/c 0.0))
           ((unchecked-lognormal-pdf (x real?) (mu real?) (sigma (>=/c 0.0))) (>=/c 0.0)))]{
Computes the probability density, @math{p(x)}, at @racket[x] for the log normal distribution with mean @racket[mu] and standard deviation @racket[sigma].}

@defproc*[(((lognormal-cdf (x real?) (mu real?) (sigma (>=/c 0.0))) (real-in 0.0 1.0))
           ((unchecked-lognormal-cdf (x real?) (mu real?) (sigma (>=/c 0.0))) (real-in 0.0 1.0)))]{
Computes the cumulative density, @math{d(x)}, at @racket[x] for the log normal distribution with mean @racket[mu] and standard deviation @racket[sigma].}

@subsection{Log Normal Distribution Graphics}

The log normal distribution graphics are defined in the @filepath{lognormal-graphics.rkt} file in the @racketfont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/lognormal-graphics)]

@defproc[(lognormal-plot (mu real?) (sigma (>=/c 0.0))) any]{
Returns a plot of the probability density and cumulative density of the log normal distribution with mean @racket[mu] and standard deviation @racket[sigma].  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the log normal distribution with mean 0.0 and standard deviation 1.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/lognormal-graphics))

(lognormal-plot 0.0 1.0)]

The following figure shows the resulting plot:

@image["scribblings/images/lognormal.png"]

@;----------
@;  The Pareto Distribution
@;----------

@section{The Pareto Distribution}

@(margin-note magnify @link["http://mathworld.wolfram.com/ParetoDistribution.html"]{Pareto Distribution} " from Wolfram MathWorld.")

The Pareto distribution functions are defined in the @filepath{pareto.rkt} file in the @racketfont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/pareto)]

@subsection{Random Variates from the Pareto Distribution}

@defproc*[(((random-pareto (s random-source?) (a real?) (b real?)) real?)
           ((unchecked-random-pareto (s random-source?) (a real?) (b real?)) real?)
           ((random-pareto (a real?) (b real?)) real?)
           ((unchecked-random-pareto (a real?) (b real?)) real?))]{
Returns a random variate from the Pareto distribution with parameters @racket[a] and @racket[b] using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.}

@bold{Example:} Histogram of random variates from the Pareto distribution with parameters @math{a = 2.0} and @math{b = 3.0}.

@racketmod[
racket
(require (planet williams/science/random-distributions/pareto)(planet williams/science/histogram-with-graphics))

(let ((h (make-histogram-with-ranges-uniform 40 1.0 21.0)))
  (for ((i (in-range 10000)))
    (histogram-increment! h (random-pareto 1.0 1.0)))
  (histogram-plot h "Histogram of the Pareto Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/pareto-hist.png"]

@subsection{Pareto Distribution Density Functions}

@defproc*[(((pareto-pdf (x real?) (a real?) (b real?)) (>=/c 0.0))
           ((unchecked-pareto-pdf (x real?) (a real?) (b real?)) (>=/c 0.0)))]{
Computes the probability density, @math{p(x)}, at @racket[x] for the Pareto distribution with parameters @racket[a] and @racket[b].}

@defproc*[(((pareto-cdf (x real?) (a real?) (b real?)) (real-in 0.0 1.0))
           ((unchecked-pareto-cdf (x real?) (a real?) (b real?)) (real-in 0.0 1.0)))]{
Computes the cumulative density, @math{d(x)}, at @racket[x] for the Pareto distribution with parameters @racket[a] and @racket[b].}

@subsection{Pareto Distribution Graphics}

The Pareto distribution graphics are defined in the @filepath{pareto-graphics.rkt} file in the @racketfont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/pareto-graphics)]

@defproc[(pareto-plot (a real?) (b real?)) any]{
Returns a plot of the probability density and cumulative density of the Pareto distribution with parameters @racket[a] and @racket[b].  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the Pareto distribution with parameters @math{a = 1.0} and @math{b = 1.0}.

@racketmod[
racket
(require (planet williams/science/random-distributions/pareto-graphics))

(beta-plot 1.0 1.0)]

The following figure shows the resulting plot:

@image["scribblings/images/pareto.png"]

@;----------
@;  The t-Distribution
@;----------

@section{The t-Distribution}

@(margin-note magnify @link["http://mathworld.wolfram.com/StudentstDistribution.html"]{Student's t-Distribution} " from Wolfram MathWorld.")

The t-distribution functions are defined in the @filepath{t-distribution.rkt} file in the @racketfont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/t-distribution)]

@subsection{Random Variates from the t-Distribution}

@defproc*[(((random-t-distribution (s random-source?) (nu real?)) real?)
           ((unchecked-random-t-distribution (s random-source?) (nu real?)) real?)
           ((random-t-distribution (nu real?)) real?)
           ((unchecked-random-t-distribution (nu real?)) real?))]{
Returns a random variate from the t-distribution with @racket[nu] degrees of freedom using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.}

@bold{Example:} Histogram of random variates from the t-distribution with 1.0 degrees of freedom.

@racketmod[
racket
(require (planet williams/science/random-distributions/t-distribution)
         (planet williams/science/histogram-with-graphics))

(let ((h (make-histogram-with-ranges-uniform 40 -6.0 6.0)))
  (for ((i (in-range 10000)))
    (histogram-increment! h (random-t-distribution 1.0)))
  (histogram-plot h "Histogram of the t-Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/t-distribution-hist.png"]

@subsection{t-Distribution Density Functions}

@defproc*[(((t-distribution-pdf (x real?) (nu real?)) (>=/c 0.0))
           ((unchecked-t-distribution-pdf (x real?) (nu real?)) (>=/c 0.0)))]{
Computes the probability density, @math{p(x)}, at @racket[x] for the t-distribution with @racket[nu] degrees of freedom.}

@defproc*[(((t-distribution-cdf (x real?) (nu real?)) (real-in 0.0 1.0))
           ((unchecked-t-distribution-cdf (x real?) (nu real?)) (real-in 0.0 1.0)))]{
Computes the cumulative density, @math{d(x)}, at @racket[x] for the t-distribution with @racket[nu] degrees of freedom.}

@subsection{t-Distribution Graphics}

The t-distribution graphics are defined in the @filepath{t-distribution-graphics.rkt} file in the @racketfont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/t-distribution-graphics)]

@defproc[(t-distribution-plot (nu real?)) any]{
Returns a plot of the probability density and cumulative density of the t-distribution with @racket[nu] degrees of freedom.  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the t-distribution with 1.0 degrees of freedom.

@racketmod[
racket
(require (planet williams/science/random-distributions/t-distribution-graphics))

(t-distribution-plot 1.0)]

The following figure shows the resulting plot:

@image["scribblings/images/t-distribution.png"]

@;----------
@;  The Triangular Distribution
@;----------

@section{The Triangular Distribution}

@(margin-note magnify @link["http://mathworld.wolfram.com/TriangularDistribution.html"]{Triangular Distribution} " from Wolfram MathWorld.")

The triangular distribution functions are defined in the @filepath{triangular.rkt} file in the @racketfont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/triangular)]

@subsection{Random Variates from the Triangular Distribution}

@defproc*[(((random-triangular (s random-source?) (a real?) (b (>/c a)) (c (real-in a b))) real?)
           ((unchecked-random-triangular (s random-source?) (a real?) (b (>/c a)) (c (real-in a b))) real?)
           ((random-triangular (a real?) (b (>/c a)) (c (real-in a b))) real?)
           ((unchecked-random-triangular (a real?) (b (>/c a)) (c (real-in a b))) real?))]{
Returns a random variate from the  triangular distribution with minimum value @racket[a], maximum value @racket[b], and most likely value @racket[c] using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.}

@bold{Example:} Histogram of random variates from the triangular distribution with minimum value 1.0, maximum value 4.0, and most likely value 2.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/triangular)
         (planet williams/science/histogram-with-graphics))

(let ((h (make-histogram-with-ranges-uniform 40 1.0 4.0)))
  (for ((i (in-range 10000)))
    (histogram-increment! h (random-traingular 1.0 4.0 2.0)))
  (histogram-plot h "Histogram of the Triangular Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/triangular-hist.png"]

@subsection{Triangular Distribution Density Functions}

@defproc*[(((triangular-pdf (x real?) (a real?) (b (>/c a)) (c (real-in a b))) (>=/c 0.0))
           ((unchecked-triangular-pdf (x real?) (a real?) (b (>/c a)) (c (real-in a b))) (>=/c 0.0)))]{
Computes the probability density, @math{p(x)}, at @racket[x] for the  triangular distribution with minimum value @racket[a], maximum value @racket[b], and most likely value @racket[c].}

@defproc*[(((triangular-cdf (x real?) (a real?) (b (>/c a)) (c (real-in a b))) (real-in 0.0 1.0))
           ((unchecked-triangular-cdf (x real?) (a real?) (b (>/c a)) (c (real-in a b))) (real-in 0.0 1.0)))]{
Computes the cumulative density, @math{d(x)}, at @racket[x] for the triangular distribution with minimum value @racket[a], maximum value @racket[b], and most likely value @racket[c].}

@subsection{Triangular Distribution Graphics}

The triangular distribution graphics are defined in the @filepath{triangular-graphics.rkt} file in the @racketfont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/triangular-graphics)]

@defproc[(triangular-plot (a real?) (b (>/c a)) (c (real-in a b))) any]{
Returns a plot of the probability density and cumulative density of the triangular distribution with minimum value @racket[a], maximum value @racket[b], and most likely value @racket[c].  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the triangular distribution with minimum value 1.0, maximum value 4.0, and most likely value 2.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/triangular-graphics))

(triangular-plot 1.0)]

The following figure shows the resulting plot:

@image["scribblings/images/triangular.png"]

@;----------
@;  The Bernoulli Distribution
@;----------

@section{The Bernoulli Distribution}

@(margin-note magnify @link["http://mathworld.wolfram.com/BernoulliDistribution.html"]{Bernoulli Distribution} " from Wolfram MathWorld.")

The Bernoulli distribution functions are defined in the @filepath{bernoulli.rkt} file in the @racketfont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/bernoulli)]

@subsection{Random Variates from the Bernoulli Distribution}

@defproc*[(((random-bernoulli (s random-source?) (p (real-in 0.0 1.0))) (integer-in 0 1))
           ((unchecked-random-bernoulli (s random-source?) (p (real-in 0.0 1.0))) (integer-in 0 1))
           ((random-bernoulli (p (real-in 0.0 1.0))) (integer-in 0 1))
           ((unchecked-random-bernoulli (p (real-in 0.0 1.0))) (integer-in 0 1)))]{
Returns a random variate from the  Bernoulli distribution with probability @racket[p] using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.}

@bold{Example:} Histogram of random variates from the Bernoulli distribution with probability 0.6.

@racketmod[
racket
(require (planet williams/science/random-distributions/bernoulli)
         (planet williams/science/discrete-histogram-with-graphics))

(let ((h (make-discrete-histogram)))
  (for ((i (in-range 10000)))
    (discrete-histogram-increment! h (random-bernoulli 0.6)))
  (histogram-plot h "Histogram of the Bernoulli Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/bernoulli-hist.png"]

@subsection{Bernoulli Distribution Density Functions}

@defproc*[(((bernoulli-pdf (k integer?) (p (real-in 0.0 1.0))) (>=/c 0.0))
           ((unchecked-bernoulli-pdf (k integer?) (p (real-in 0.0 1.0))) (>=/c 0.0)))]{
Computes the probability density, @math{p(k)}, at @racket[k] for the Bernoulli distribution with probability @racket[p].}

@defproc*[(((bernoulli-cdf (k integer?) (p (real-in 0.0 1.0))) (real-in 0.0 1.0))
           ((unchecked-bernoulli-cdf (k integer?) (p (real-in 0.0 1.0))) (real-in 0.0 1.0)))]{
Computes the cumulative density, @math{d(k)}, at @racket[k] for the Bernoulli distribution with probability @racket[p].}

@subsection{Bernoulli Distribution Graphics}

The Bernoulli distribution graphics are defined in the @filepath{bernoulli-graphics.rkt} file in the @racketfont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/bernoulli-graphics)]

@defproc[(bernoulli-plot (p (real-in 0.0 1.0))) any]{
Returns a plot of the probability density and cumulative density of the Bernoulli distribution with probability @racket[p].  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the Bernoulli distribution with probability 0.6.

@racketmod[
racket
(require (planet williams/science/random-distributions/bernoulli-graphics))

(bernoulli-plot 0.6)]

The following figure shows the resulting plot:

@image["scribblings/images/bernoulli.png"]

@;----------
@;  The Binomial Distribution
@;----------

@section{The Binomial Distribution}

@(margin-note magnify @link["http://mathworld.wolfram.com/BinomialDistribution.html"]{Binomial Distribution} " from Wolfram MathWorld.")

The binomial distribution functions are defined in the @filepath{binomial.rkt} file in the @racketfont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/binomial)]

@subsection{Random Variates from the Binomial Distribution}

@defproc*[(((random-binomial (s random-source?) (p (real-in 0.0 1.0)) (n natural-number/c)) natural-number/c)
           ((unchecked-random-binomial (s random-source?) (p (real-in 0.0 1.0)) (n natural-number/c)) natural-number/c)
           ((random-binomial (p (real-in 0.0 1.0)) (n natural-number/c)) natural-number/c)
           ((unchecked-random-binomial (p (real-in 0.0 1.0)) (n natural-number/c)) natural-number/c))]{
Returns a random variate from the  binomial distribution with parameters @racket[p] and @racket[n] using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.}

@bold{Example:} Histogram of random variates from the binomial distribution with parameters 0.5 and 20.

@racketmod[
racket
(require (planet williams/science/random-distributions/binomial)
         (planet williams/science/discrete-histogram-with-graphics))

(let ((h (make-discrete-histogram)))
  (for ((i (in-range 10000)))
    (discrete-histogram-increment! h (random-bernoulli 0.5 20)))
  (histogram-plot h "Histogram of the Binomial Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/binomial-hist.png"]

@subsection{Binomial Distribution Density Functions}

@defproc*[(((binomial-pdf (k integer?) (p (real-in 0.0 1.0)) (n natural-number/c)) (>=/c 0.0))
           ((unchecked-binomial-pdf (k integer?) (p (real-in 0.0 1.0)) (n natural-number/c)) (>=/c 0.0)))]{
Computes the probability density, @math{p(k)}, at @racket[k] for the binomial distribution with parameters @racket[p] and @racket[n].}

@subsection{Binomial Distribution Graphics}

The binomial distribution graphics are defined in the @filepath{binomial-graphics.rkt} file in the @racketfont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/binomial-graphics)]

@defproc[(binomial-plot (p (real-in 0.0 1.0)) (n natural-number/c)) any]{
Returns a plot of the probability density and cumulative density of the binomial distribution with parameters @racket[p] and @racket[n].  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the binomial distribution with parameters 0.5 and 20.

@racketmod[
racket
(require (planet williams/science/random-distributions/binomial-graphics))

(bernoulli-plot 0.5 20)]

The following figure shows the resulting plot:

@image["scribblings/images/binomial.png"]

@;----------
@;  The Geometric Distribution
@;----------

@section{The Geometric Distribution}

@(margin-note magnify @link["http://mathworld.wolfram.com/GeometricDistribution.html"]{Geometric Distribution} " from Wolfram MathWorld.")

The geometric distribution functions are defined in the @filepath{geometric.rkt} file in the @racketfont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/geometric)]

@subsection{Random Variates from the Geometric Distribution}

@defproc*[(((random-geometric (s random-source?) (p (real-in 0.0 1.0))) natural-number/c)
           ((unchecked-random-geometric (s random-source?) (p (real-in 0.0 1.0))) natural-number/c)
           ((random-geometric (p (real-in 0.0 1.0))) natural-number/c)
           ((unchecked-random-geometric (p (real-in 0.0 1.0))) natural-number/c))]{
Returns a random variate from the geometric distribution with probability @racket[p] using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.}

@bold{Example:} Histogram of random variates from the geometric distribution with probability 0.5.

@racketmod[
racket
(require (planet williams/science/random-distributions/geometric)
         (planet williams/science/discrete-histogram-with-graphics))

(let ((h (make-discrete-histogram)))
  (for ((i (in-range 10000)))
    (discrete-histogram-increment! h (random-geometric 0.5)))
  (histogram-plot h "Histogram of the Geometric Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/geometric-hist.png"]

@subsection{Geometric Distribution Density Functions}

@defproc*[(((geometric-pdf (k integer?) (p (real-in 0.0 1.0))) (>=/c 0.0))
           ((unchecked-geometric-pdf (k integer?) (p (real-in 0.0 1.0))) (>=/c 0.0)))]{
Computes the probability density, @math{p(k)}, at @racket[k] for the geometric distribution with probability @racket[p].}

@subsection{Geometric Distribution Graphics}

The geometric distribution graphics are defined in the @filepath{geometric-graphics.rkt} file in the @racketfont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/geometric-graphics)]

@defproc[(geometric-plot (p (real-in 0.0 1.0))) any]{
Returns a plot of the probability density and cumulative density of the geometric distribution with probability @racket[p].  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the geometric distribution with probability 0.5.

@racketmod[
racket
(require (planet williams/science/random-distributions/geometric-graphics))

(geometric-plot 0.5)]

The following figure shows the resulting plot:

@image["scribblings/images/geometric.png"]

@;----------
@;  The Logarithmic Distribution
@;----------

@section{The Logarithmic Distribution}

@(margin-note "Note that the logarithmic distribution in the GSL, and as implemented in the Science Collection, is a discrete version of the exponential distribution.")

The logarithmic distribution functions are defined in the @filepath{logarithmic.rkt} file in the @racketfont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/logarithmic)]

@subsection{Random Variates from the Logarithmic Distribution}

@defproc*[(((random-logarithmic (s random-source?) (p (real-in 0.0 1.0))) natural-number/c)
           ((unchecked-random-logarithmic (s random-source?) (p (real-in 0.0 1.0))) natural-number/c)
           ((random-logarithmic (p (real-in 0.0 1.0))) natural-number/c)
           ((unchecked-random-logarithmic (p (real-in 0.0 1.0))) natural-number/c))]{
Returns a random variate from the logarithmic distribution with probability @racket[p] using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.}

@bold{Example:} Histogram of random variates from the logarithmic distribution with probability 0.5.

@racketmod[
racket
(require (planet williams/science/random-distributions/logarithmic)
         (planet williams/science/discrete-histogram-with-graphics))

(let ((h (make-discrete-histogram)))
  (for ((i (in-range 10000)))
    (discrete-histogram-increment! h (random-logarithmic 0.5)))
  (histogram-plot h "Histogram of the Logarithmic Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/logarithmic-hist.png"]

@subsection{Logarithmic Distribution Density Functions}

@defproc*[(((logarithmic-pdf (k integer?) (p (real-in 0.0 1.0))) (>=/c 0.0))
           ((unchecked-logarithmic-pdf (k integer?) (p (real-in 0.0 1.0))) (>=/c 0.0)))]{
Computes the probability density, @math{p(k)}, at @racket[k] for the logarithmic distribution with probability @racket[p].}

@subsection{Logarithmic Distribution Graphics}

The logarithmic distribution graphics are defined in the @filepath{logarithmic-graphics.rkt} file in the @racketfont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/logarithmic-graphics)]

@defproc[(logarithmic-plot (p (real-in 0.0 1.0))) any]{
Returns a plot of the probability density and cumulative density of the logarithmic distribution with probability @racket[p].  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the logarithmic distribution with probability 0.5.

@racketmod[
racket
(require (planet williams/science/random-distributions/logarithmic-graphics))

(logarithmic-plot 0.5)]

The following figure shows the resulting plot:

@image["scribblings/images/logarithmic.png"]

@;----------
@;  The Poisson Distribution
@;----------

@section{The Poisson Distribution}

@(margin-note magnify @link["http://mathworld.wolfram.com/PoissonDistribution.html"]{Poisson Distribution} " from Wolfram MathWorld.")

The Poisson distribution functions are defined in the @filepath{poisson.rkt} file in the @racketfont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/poisson)]

@subsection{Random Variates from the Poisson Distribution}

@defproc*[(((random-poisson (s random-source?) (mu real?)) natural-number/c)
           ((unchecked-random-poisson (s random-source?) (mu real?)) natural-number/c)
           ((random-poisson (mu real?)) natural-number/c)
           ((unchecked-random-poisson (mu real?)) natural-number/c))]{
Returns a random variate from the Poisson distribution with mean @racket[mu] using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.}

@bold{Example:} Histogram of random variates from the Poisson distribution with mean 10.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/poisson)(planet williams/science/discrete-histogram-with-graphics))

(let ((h (make-discrete-histogram)))
  (for ((i (in-range 10000)))
    (discrete-histogram-increment! h (random-poisson 10.0)))
  (histogram-plot h "Histogram of the Poisson Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/poisson-hist.png"]

@subsection{Poisson Distribution Density Functions}

@defproc*[(((poisson-pdf (k integer?) (mu real?)) (>=/c 0.0))
           ((unchecked-poisson-pdf (k integer?) (mu real?)) (>=/c 0.0)))]{
Computes the probability density, @math{p(k)}, at @racket[k] for the Poisson distribution with mean @racket[mu].}

@subsection{Poisson Distribution Graphics}

The Poisson distribution graphics are defined in the @filepath{poisson-graphics.rkt} file in the @racketfont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/poisson-graphics)]

@defproc[(poisson-plot (mu real?)) any]{
Returns a plot of the probability density and cumulative density of the Poisson distribution with mean @racket[mu].  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of the Poisson distribution with mean 10.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/poisson-graphics))

(poisson-plot 10.0)]

The following figure shows the resulting plot:

@image["scribblings/images/poisson.png"]

@;----------
@;  General Discrete Distributions
@;----------

@section{General Discrete Distributions}

The discrete distribution functions are defined in the @filepath{discrete.rkt} file in the @racketfont{random-distributions} subcollection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/discrete)]

@subsection{Creating Discrete Distributions}

@defproc[(discrete? (x any/c)) boolean?]{
Returns true, @racket[#t], if @racket[x] is a discrete distribution.}

@defproc[(make-discrete (weights (vector-of real?))) discrete?]{
Returns a discrete distribution whose probability density is given by the specified @racket[weights].  Note that the @racket[weights] do not have to sum to one.}

@subsection{Random Variates from a Discrete Distribution}

@defproc*[(((random-discrete (s random-source?) (d discrete?)) integer?)
           ((unchecked-random-discrete (s random-source?) (d discrete?)) integer?)
           ((random-discrete (d discrete?)) integer?)
           ((unchecked-random-discrete (d discrete?)) integer?))]{
Returns a random variate from a general discrete distribution @racket[d] using the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not provided.}

@bold{Example:} Histogram of random variates from a discrete distribution with weights 0.1, 0.4, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, and 0.1.

@racketmod[
racket
(require (planet williams/science/random-distributions/discrete)
         (planet williams/science/discrete-histogram-with-graphics))

(let ((h (make-discrete-histogram))
      (d (make-discrete #(.1 .4 .9 .8 .7 .6 .5 .4 .3 .2 .1))))
  (for ((i (in-range 10000)))
    (discrete-histogram-increment! h (random-discrete d)))
  (histogram-plot h "Histogram of a Discrete Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/discrete-hist.png"]

@subsection{Discrete Distribution Density Functions}

@defproc*[(((discrete-pdf (d discrete?) (k integer?)) (real-in 0.0 1.0))
           ((unchecked-discrete-pdf (d discrete?) (k integer?)) (real-in 0.0 1.0)))]{
Computes the probability density, @math{p(k)}, at @racket[k] for a discrete distribution @racket[d].}

@defproc*[(((discrete-cdf (d discrete?) (k integer?)) (real-in 0.0 1.0))
           ((unchecked-discrete-cdf (d discrete?) (k integer?)) (real-in 0.0 1.0)))]{
Computes the cumulative density, @math{d(k)}, at @racket[k] for a discrete distribution @racket[d].}

@subsection{Discrete Distribution Graphics}

The discrete distribution graphics are defined in the @filepath{discrete-graphics.rkt} file in the @racketfont{random-distributions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-distributions/discrete-graphics)]

@defproc[(discrete-plot (d discrete?)) any]{
Returns a plot of the probability density and cumulative density of a discrete distribution @racket[d].  The plot is produced by the plot collection provided with Racket.}

@bold{Example:} Plot of the probability density and cumulative density of a discrete distribution with weights 0.1, 0.4, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, and 0.1.

@racketmod[
racket
(require (planet williams/science/random-distributions/discrete-graphics))

(let ((d (make-discrete #(.1 .4 .9 .8 .7 .6 .5 .4 .3 .2 .1))))
  (discrete-plot d))]

The following figure shows the resulting plot:

@image["scribblings/images/discrete-plot.png"]
