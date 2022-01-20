#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribblings/icons
          (for-label racket
                     (planet williams/science/science-with-graphics)))

@title[#:tag "special-functions"]{Special Functions}

@local-table-of-contents[]

This chapter describes the special functions provided by the Science Collection.

The functions described in this chapter are defined in the @racketfont{special-functions} sub-collection of the Science Collection.  The entire @racketfont{special-functions} sub-collection can be made available using the form:

@defmodule[(planet williams/science/special-functions)]

The individual modules in the @racketfont{special-functions} sub-collection can also be made available as describes in the sections below.

@section{Error Functions}

The @italic{error function} is described in Abramowitz and Stegun @cite["Abramowitz64"], Chapter 7.  The functions are defined in the @filepath{error.rkt} file in the @racketfont{special-functions} sub-collection of the science collection and are made available using the form:

@defmodule[(planet williams/science/special-functions/error)]

@subsection{Error Function}

@(margin-note magnify @link["http://mathworld.wolfram.com/Erf.html"]{Erf} " from Wolfram MathWorld.")

@defproc*[(((erf (x real?)) (real-in -1.0 1.0))
           ((unchecked-erf (x real?)) (real-in -1.0 1.0)))]{
Computes the error function:
         
@image["scribblings/images/erf-equation.png"]{erf equation}.}

@bold{Example:} Plot of @racket[(erf x)] on the interval [-4, 4].

@racketmod[
racket
(require (planet williams/science/special-functions/error)
         plot)

(plot (line erf)
      #:x-min -4.0 #:x-max 4.0
      #:y-min -1.0 #:y-max 1.0
      #:title "Error Function, erf(x)")]

The following figure shows the resulting plot:

@image["scribblings/images/erf.png"]{erf plot}

@subsection{Complementary Error Function}

@(margin-note magnify @link["http://mathworld.wolfram.com/Erfc.html"]{Erfc} " from Wolfram MathWorld.")

@defproc*[(((erfc (x real?)) (real-in 0.0 2.0))
           ((unchecked-erfc (x real?)) (real-in 0.0 2.0)))]{
Computes the complementary error function:
         
@image["scribblings/images/erfc-equation.png"]{erfc equation}.}

@defproc*[(((log-erfc (x real?)) real?)
           ((unchecked-log-erfc (x real?)) real?))]{
Computes the log of the complementary error function.}

@bold{Example:} Plot of @racket[(erfc x)] on the interval [-4, 4].

@racketmod[
racket
(require (planet williams/science/special-functions/error)
         plot)

(plot (line erfc)
      #:x-min -4.0 #:x-max 4.0
      #:y-min 0.0 #:y-max 2.0
      #:title "Complementary Error Function, erfc(x)")]

The following figure shows the resulting plot:

@image["scribblings/images/erfc.png"]{erfc plot}

@subsection{Hazard Function}

@(margin-note magnify @link["http://mathworld.wolfram.com/HazardFunction.html"]{Hazard Function} " from Wolfram MathWorld.")

The @italic{hazard function} for the normal distribution, also known as the inverse Mill's ratio, is the ratio of the probability function, @math{P(x)}, to the survival function, @math{S(x)}, and is defined as:

@image["scribblings/images/hazard-equation.png"]{hazard equation}

@defproc*[(((hazard (x real?)) (>=/c 0.0))
           ((unchecked-hazard (x real?)) (>=/c 0.0)))]{
Computes the hazard function for the normal distribution.}

@bold{Example:} Plot of @racket[(hazard x)] on the interval [-5, 10].

@racketmod[
racket
(require (planet williams/science/special-functions/error)
         plot)

(plot (line hazard)
      #:x-min -5.0 #:x-max 10.0
      #:y-min 0.0 #:y-max 10.0
      #:title "Hazard Function, hazard(x)")]

The following figure shows the resulting plot:

@image["scribblings/images/hazard.png"]{hazard plot}

@section{Exponential Integral Functions}

@(margin-note magnify @link["http://mathworld.wolfram.com/ExponentialIntegral.html"]{Exponential Intgral} " from Wolfram MathWorld.")

Information on the exponential integral functions can be found in Abramowitz and Stegun @cite["Abramowitz64"], Chapter 5.  The functions are defined in the @filepath{exponential-integral.rkt} file in the @racketfont{special-functions} sub-collection of the science collection are made available using the form:

@defmodule[(planet williams/science/special-functions/exponential-integral)]

@subsection{First-Order Exponential Integral}

@defproc*[(((expint-E1 (x real?)) real?)
           ((unchecked-expint-E1 (x real?)) real?)
           ((expint-E1-scaled (x real?)) real?)
           ((unchecked-expint-E1-scaled (x real?)) real?))]{
Computes the exponential integral @math{E_1(x)}:
         
@image["scribblings/images/expint-E1-equation.png"].}

@bold{Example:} Plot of @scheme[(expint-E1 x)] on the interval [-4, 4].

@racketmod[
racket
(require (planet williams/science/special-functions/exponential-integral)
         plot)

(plot (line expint-E1)
      #:x-min -4.0 #:x-max 4.0
      #:y-min -10.0 #:y-max 10.0
      #:title "Exponential Integral, E1(x)")]

The following figure shows the resulting plot:

@image["scribblings/images/expint-E1.png"]

@subsection{Second-Order Exponential Integral}

@defproc*[(((expint-E2 (x real?)) real?)
           ((unchecked-expint-E2 (x real?)) real?)
           ((expint-E2-scaled (x real?)) real?)
           ((unchecked-expint-E2-scaled (x real?)) real?))]{
Computes the second-order exponential integral @math{E_2(x)}:
         
@image["scribblings/images/expint-E2-equation.png"].}

@bold{Example:} Plot of @racket[(expint-E2 x)] on the interval [-4, 4].

@racketmod[
racket
(require (planet williams/science/special-functions/exponential-integral)
         plot)

(plot (line expint-E2)
      #:x-min -4.0 #:x-max 4.0
      #:y-min -10.0 #:y-max 10.0
      #:title "Exponential Integral, E2(x)")]

The following figure shows the resulting plot:

@image["scribblings/images/expint-E2.png"]

@subsection{General Exponential Integral}

@defproc*[(((expint-Ei (x real?)) real?)
           ((unchecked-expint-Ei (x real?)) real?)
           ((expint-Ei-scaled (x real?)) real?)
           ((unchecked-expint-Ei-scaled (x real?)) real?))]{
Computes the exponential integral @math{E_i(x)}:
         
@image["scribblings/images/expint-Ei-equation.png"].}

@bold{Example:} Plot of @racket[(expint-Ei x)] on the interval [-4, 4].

@racketmod[
racket
(require (planet williams/science/special-functions/exponential-integral)
         plot)

(plot (line expint-Ei)
      #:x-min -4.0 #:x-max 4.0
      #:y-min -10.0 #:y-max 10.0
      #:title "Exponential Integral, Ei(x)")]

The following figure shows the resulting plot:

@image["scribblings/images/expint-Ei.png"]

@section{GammaFunctions}

The gamma functions are defined in the @filepath{gamma.rkt} file in the @racketfont{special-functions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/special-functions/gamma)]

Note that the gamma functions (Section 5.3), psi functions (Section 5.4), and the zeta functions (Section 5.5) are defined in the same module, @filepath{gamma.rkt}.  This is because their definitions are interdependent and Racket does not allow circular module dependencies.

@subsection{Gamma Function}

@(margin-note magnify @link["http://mathworld.wolfram.com/GammaFunction.html"]{Gamma Function} " from Wolfram MathWorld.")

The @italic{gamma function} is defined by the integral:

@image["scribblings/images/gamma-equation.png"]

It is related to the factorial function by @math{Γ(n) = (n - 1)!} for positive integer @math{n}.  Further information on the gamma function can be found in Abramowitz and Stegun @cite["Abramowitz64"], Chapter 6.

@defproc*[(((gamma (x real?)) real?)
           ((unchecked-gamma (x real?)) real?))]{
Computes the gamma function, @math{Γ(x)}, subject to @racket[x] not being a negative integer.  This function is computed using the real Lanczos method.  The maximum value of @racket[x] such that @math{Γ(x)} is not considered an overflow is given by the constant @racket[gamma-xmax] and is 171.0.}

@bold{Example:} Plot of @racket[(gamma x)] on the interval (0, 6].

@racketmod[
racket
(require (planet williams/science/special-functions/gamma)
         plot)

(plot (line gamma)
      #:x-min 0.001 #:x-max 6.0
      #:y-min 0.0 #:y-max 120.0
      #:title "Gamma Function, Gamma(x)")]

The following figure shows the resulting plot:

@image["scribblings/images/gamma.png"]

@bold{Example:} Plot of @racket[(gamma x)] on the interval (-1, 0).

@racketmod[
racket
(require (planet williams/science/special-functions/gamma)
         plot)

(plot (line gamma)
      #:x-min -0.999 #:x-max -0.001
      #:y-min -120.0 #:y-max 0.0
      #:title "Gamma Function, Gamma(x)")]

The following figure shows the resulting plot:

@image["scribblings/images/gamma-1.png"]

@(margin-note magnify @link["http://mathworld.wolfram.com/LogGammaFunction.html"]{Log Gamma Function} " from Wolfram MathWorld.")

@defproc*[(((lngamma (x real?)) real?)
           ((unchecked-lngamma (x real?)) real?))]{
Computes the logarithm of the gamma function, @math{log Γ(x)}, subject to @racket[x] not being a negative integer.  For @math{x < 0}, the real part of @math{log Γ(x)} is returned, which is equivalent to @math{log |Γ(x)|}.  The function is computed using the real Lanczos method.}

@defproc*[(((lngamma-sgn (x real?)) (values real? (integer-in -1 1)))
           ((unchecked-lngamma-sgn (x real?)) (values real? (integer-in -1 1))))]{
Computes the logarithm of the magnitude of the gamma function and its sign, subject to @racket[x] not being a negative integer, and returns them as multiple values.  The function is computed using the real Lanczos method.  The value of the gamma function can be reconstructed using the relation @math{Γ(x) = sgn × exp(resultlg)}, where @math{resultlg} and @math{sgn} are the returned values.}

@bold{Example:} Plot of @racket[(lngamma x)] on the interval (0, 6].

@racketmod[
racket
(require (planet williams/science/special-functions/gamma)
         plot)

(plot (line lngamma)
      #:x-min 0.001 #:x-max 6.0
      #:y-min 0.0 #:y-max 5.0
      #:title "Log Gamma Function, log Gamma(x)")]

The following figure shows the resulting plot:

@image["scribblings/images/lngamma.png"]

@defproc*[(((gammainv (x real?)) real?)
           ((unchecked-gammainv (x real?)) real?))]{
Computes the reciprocal of the gamma function, @math{1/Γ(x)}, using the real Lanczos method.}

@subsection{Regulated Gamma Function}

The @italic{regulated gamma function} is given by

@image["scribblings/images/gammastar-equation.png"]

@defproc*[(((gammastar (x (>/c 0.0))) real?)
           ((gamma* (x (>/c 0.0))) real?)
           ((unchecked-gammastar (x (>/c 0.0))) real?)
           ((unchecked-gamma* (x (>/c 0.0))) real?))]{
Computes the regulated gamma function, @math{Γ*(x)}, for @math{x > 0}.}

@bold{Example:} Plot of @racket[(gammastar x)] on the interval (0, 4].

@racketmod[
racket
(require (planet williams/science/special-functions/gamma)
         plot)

(plot (line gammastar)
      #:x-min 0.001 #:x-max 4.0
      #:y-min 0.0 #:y-max 10.0
      #:title "Regulated Gamma Function, Gamma*(x)")]

The following figure shows the resulting plot:

@image["scribblings/images/gammastar.png"]

@subsection{Incomplete Gamma Function}

@(margin-note magnify @link["http://mathworld.wolfram.com/IncompleteGammaFunction.html"]{Incomplete Gamma Function} " from Wolfram MathWorld.")

@defproc*[(((gamma-inc-Q (a (>/c 0.0)) (x (>=/c 0.0))) real?)
           ((unchecked-gamma-inc-Q (a (>/c 0.0)) (x (>=/c 0.0))) real?))]{
Computes the normalized incomplete gamma function,

@image["scribblings/images/gamma-inc-Q-equation.png"]

for @math{a > 0} and @math{x ≥ 0}.}

@defproc*[(((gamma-inc-P (a (>/c 0.0)) (x (>=/c 0.0))) real?)
           ((unchecked-gamma-inc-P (a (>/c 0.0)) (x (>=/c 0.0))) real?))]{
Computes the complementary normalized incomplete gamma function,

@image["scribblings/images/gamma-inc-P-equation.png"]

for @math{a > 0} and @math{x ≥ 0}.}

@defproc*[(((gamma-inc (a real?) (x (>=/c 0.0))) real?)
           ((unchecked-gamma-inc (a real?) (x (>=/c 0.0))) real?))]{
Computes the unnormalized incomplete gamma function,

@image["scribblings/images/gamma-inc-equation.png"]

for @racket[a] real and @math{x ≥ 0}.}

@subsection{Factorial Function}

The factorial of a positive integer @math{n}, @math{n!}, is defined as @math{n! = n × (n - 1) × ... × 2 × 1}.  By definition, @math{0! = 1}.  The related function is related to the gamma function by @math{Γ(n) = (n - 1)!}.

@defproc*[(((fact (n natural-number/c)) (>=/c 1.0))
           ((unchecked-fact (n natural-number/c)) (>=/c 1.0)))]{
Computes the factorial of @scheme[n], @math{n!}.}

@defproc*[(((lnfact (n natural-number/c)) (>=/c 0.0))
           ((unchecked-lnfact (n natural-number/c)) (>=/c 0.0)))]{
Computes the logarithm of the factorial of @racket[n], @math{log n!}.  The algorithm is faster than computing @math{ln Γ(n + 1)} via @racket[lngamma] for @math{n < 170}, but defers for larger @math{n}.}

@subsection{Double Factorial Function}

The double factorial of @racket[n], @math{n!!}, is defined as @math{n! = n × (n - 2) × (n - 4) × ...}.  By definition, @math{-1!! = 0!! = 1}.

@defproc*[(((double-fact (n natural-number/c)) (>=/c 1.0))
           ((unchecked-double-fact (n natural-number/c)) (>=/c 1.0)))]{
Computes the double factorial of @racket[n], @math{n!!}.}

@defproc*[(((lndouble-fact (n natural-number/c)) (>=/c 0.0))
           ((unchecked-lndouble-fact (n natural-number/c)) (>=/c 0.0)))]{
Computes the logarithm of the double factorial of @racket[n], @math{log n!!}.}

@subsection{Binomial Coefficient Function}

The binomial coefficient, @math{n choose m}, is defined as:

@image["scribblings/images/choose-equation.png"]

@defproc*[(((choose (n natural-number/c) (m natural-number/c)) (>=/c 1.0))
           ((unchecked-choose (n natural-number/c) (m natural-number/c)) (>=/c 1.0)))]{
Computes the binomial coefficient for @racket[n] and @racket[m], @math{n choose m}.}

@defproc*[(((lnchoose (n natural-number/c) (m natural-number/c)) (>=/c 0.0))
           ((unchecked-lnchoose (n natural-number/c) (m natural-number/c)) (>=/c 0.0)))]{
Computes the logarithm of the binomial coefficient for @racket[n] and @racket[m], @math{log (n choose m)}.}

@section{Psi Functions}

The psi functions are defined in the @filepath{gamma.rkt} file in the @schemefont{special-functions} sub-collection of the science collection and are made available using the form:

@;@defmodule[(planet williams/science/special-functions/gamma)]
@racket[(require (planet williams/science/special-functions/gamma))]

Note that the gamma functions (Section 5.3), psi functions (Section 5.4), and the zeta functions (Section 5.5) are defined in the same module, @filepath{gamma.rkt}.  This is because their definitions are interdependent and Racket does not allow circular module dependencies.

@subsection{Psi (Digamma) Functions}

@(margin-note magnify @link["http://mathworld.wolfram.com/DigammaFunction.html"]{Digamma Function} " from Wolfram MathWorld.")

@defproc*[(((psi-int (n (integer-in 1 +inf.0))) real?)
           ((unchecked-psi-int (n (integer-in 1 +inf.0))) real?))]{
Computes the digamma function, @math{Ψ(n)}, for positive integer @racket[n].  The digamma function is also called the Psi function.}

@defproc*[(((psi (x real?)) real?)
           ((unchecked-psi (x real?)) real?))]{
Returns the digamma function, @math{Ψ(x)}, for general @racket[x], @math{x ≠ 0}.}

@defproc*[(((psi-1piy (y real?)) real?)
           ((unchecked-psi-1piy (y real?)) real?))]{
Computes the real part of the digamma function on the line @math{1 + iy}, @math{Re Ψ(1 + iy)}.}

@bold{Example:} Plot of @racket[(psi x)] on the interval (0, 5].

@racketmod[
racket
(require (planet williams/science/special-functions/gamma)
         plot)

(plot (line psi)
      #:x-min 0.001 #:x-max 5.0
      #:y-min -5.0 #:y-max 5.0
      #:title "Psi (Digamma) Function, Psi(x)")]

The following figure shows the resulting plot:

@image["scribblings/images/psi.png"]

@subsection{Psi-1 (Trigamma) Functions}

@(margin-note magnify @link["http://mathworld.wolfram.com/TrigammaFunction.html"]{Trigamma Function} " from Wolfram MathWorld.")

@defproc*[(((psi-1-int (n (integer-in 1 +inf.0))) real?)
           ((unchecked-psi-1-int (n (integer-in 1 +inf.0))) real?))]{
Computes the trigamma function, @math{Ψ'(n)}, for positive integer @racket[n].}

@defproc*[(((psi-1 (x real?)) real?)
           ((unchecked-psi-1 (x real?)) real?))]{
Computes the trigamma function, @math{Ψ'(x)}, for general @racket[x].}

@bold{Example:} Plot of @racket[(psi-1 x)] on the interval (0, 5].

@racketmod[
racket
(require (planet williams/science/special-functions/gamma)
         plot)

(plot (line psi-1)
      #:x-min 0.001 #:x-max 5.0
      #:y-min 0.0 #:y-max 5.0
      #:title "Psi-1 (Trigamma) Function, Psi-1(x)")]

The following figure shows the resulting plot:

@image["scribblings/images/psi-1.png"]

@subsection{Psi-n (Polygamma) Functions}

@(margin-note magnify @link["http://mathworld.wolfram.com/PolygammaFunction.html"]{Polygamma Function} " from Wolfram MathWorld.")

@defproc*[(((psi-n (n natural-number/c) (x (>/c 0.0))) real?)
           ((unchecked-psi-n (n natural-number/c) (x (>/c 0.0))) real?))]{
Computes the polygamma function, @math{Ψ^m(x)}, for @math{m ≥ 0}, @math{x > 0}.}

@bold{Example:} Plot of @racket[(psi-n n x)] for @math{n = 3} on the interval (0, 5].

@racketmod[
racket
(require (planet williams/science/special-functions/gamma)
         plot)

(plot (line (lambda (x) (psi-n 3 x)))
      #:x-min 0.001 #:x-max 5.0
      #:y-min 0.0 #:y-max 10.0
      #:title "Psi-n (Polygamma) Function, Psi-n(3, x)")]

The following figure shows the resulting plot:

@image["scribblings/images/psi-n.png"]

@section{Zeta Functions}

The Riemann zeta function is defined in Abramowitz and Stegun @cite["Abramowitz64"], Section 23.3. The zeta functions are defined in the @filepath{gamma.rkt} file in the @racketfont{special-functions} subcollection of the science collection are are made available using the form:

@;@defmodule[(planet williams/science/special-functions/gamma)]
@racket[(require (planet williams/science/special-functions/gamma))]

Note that the gamma functions (Section 5.3), psi functions (Section 5.4), and the zeta functions (Section 5.5) are defined in the same module, @filepath{gamma.rkt}.  This is because their definitions are interdependent and Racket does not allow circular module dependencies.

@subsection{Riemann Zeta Functions}

@(margin-note magnify @link["http://mathworld.wolfram.com/RiemannZetaFunction.html"]{Riemann Zeta Function} " from Wolfram MathWorld.")

The Riemann zeta function is defined by the infinite sum:

@image["scribblings/images/zeta-equation.png"]

@defproc*[(((zeta-int (n integer?)) real?)
           ((unchecked-zeta-int (n integer?)) real?))]{
Computes the Reimann zeta function, @math{ζ(n)}, for integer @racket[n], @math{n ≠ 1}.}

@defproc*[(((zeta (s real?)) real?)
           ((unchecked-zeta (s real?)) real?))]{
Computes the Riemann zeta function, @math{ζ(s)}, for arbitrary @racket[s], @math{s ≠ 1}.}

@bold{Example:} Plot of @racket[(zeta x)] on the interval [-5, 5].

@racketmod[
racket
(require (planet williams/science/special-functions/gamma)
         plot)

(plot (line zeta)
      #:x-min -5.0 #:x-max 5.0
      #:y-min -5.0 #:y-max 5.0
      #:title "Riemann Zeta Function, zeta(x)")]

The following figure shows the resulting plot:

@image["scribblings/images/zeta.png"]

@subsection{Riemann Zeta Functions Minus One}

For large positive arguments, the Riemann zeta function approached one.  In this region the fractional part is interesting and, therefore, we need a function to evaluate it explicitly.

@defproc*[(((zetam1-int (n integer?)) real?)
           ((unchecked-zetam1-int (n integer?)) real?))]{
Computes @math{ζ(n) - 1} for integer @racket[n],  @math{n ≠ 1}.}

@defproc*[(((zetam1 (s real?)) real?)
           ((unchecked-zetam1 (s real?)) real?))]{
Computes @math{ζ(s) - 1} for argitrary @racket[s],  @math{s ≠ 1}.}

@subsection{Hutwitz Zeta Function}

@(margin-note magnify @link["http://mathworld.wolfram.com/HurwitzZetaFunction.html"]{Hurwitz Zeta Function} " from Wolfram MathWorld.")

The Hurwitz zeta function is defined by:

@image["scribblings/images/hzeta-equation.png"]

@defproc*[(((hzeta (s (>/c 1.0)) (q (>/c 0.0))) real?)
           ((unchecked-hzeta (s (>/c 1.0)) (q (>/c 0.0))) real?))]{
Computes the Hurwitz zeta function, @math{ζ(s, q)}, for @math{s > 1}, @math{q > 0}.}

@bold{Example:} Plot of @racket[(hzeta x 2.0)] on the interval (1, 5].

@racketmod[
racket
(require (planet williams/science/special-functions/gamma)
         plot)

(plot (line (lambda (x) (hzeta x 2.0)))
      #:x-min 1.001 #:x-max 5.0
      #:y-min 0.0 #:y-max 5.0
      #:title "Hutwitz Zeta Function, hzeta(x, 2.0)")]

The following figure shows the resulting plot:

@image["scribblings/images/hzeta.png"]

@subsection{Eta Functions}

@(margin-note magnify @link["http://mathworld.wolfram.com/DirichletEtaFunction.html"]{Dirichlet Eta Function} " from Wolfram MathWorld.")

The eta function is defined by:

@image["scribblings/images/eta-equation.png"]

@defproc*[(((eta-int (n integer?)) real?)
           ((unchecked-eta-int (n integer?)) real?))]{
Computes the eta function, @math{η(n)}, for integer @racket[n].}

@defproc*[(((eta (s real?)) real?)
           ((unchecked-eta (s real?)) real?))]{
Computes the eta function, @math{η(s)}, for arbitrary @racket[s].}

@bold{Example:} Plot of @racket[(eta x)] on the interval [-10, 10].

@racketmod[
racket
(require (planet williams/science/special-functions/gamma)
         plot)

(plot (line eta)
      #:x-min -10.0 #:x-max 10.0
      #:y-min -5.0 #:y-max 5.0
      #:title "Eta Function, eta(x)")]

The following figure shows the resulting plot:

@image["scribblings/images/eta.png"]

@section{Beta Functions}

The beta functions are defined in the @filepath{beta.rkt} file in the @racketfont{special-functions} sub-collection of the Science Collection and are made available using the form:

@defmodule[(planet williams/science/special-functions/beta)]

@(margin-note magnify @link["http://mathworld.wolfram.com/BetaFunction.html"]{Beta Function} " from Wolfram MathWorld.")

@defproc*[(((beta (a (>/c 0.0)) (b (>/c 0.0))) real?)
           ((unchecked-beta (a (>/c 0.0)) (b (>/c 0.0))) real?))]{
Computes the beta function,
         
@image["scribblings/images/beta-equation.png"]

for @math{a > 0} and @math{b > 0}.}

@defproc*[(((lnbeta (a (>/c 0.0)) (b (>/c 0.0))) real?)
           ((unchecked-lnbeta (a (>/c 0.0)) (b (>/c 0.0))) real?))]{
Computes the logarithm of the beta function, @math{log Β(a, b)}, for @math{a > 0} and @math{b > 0}.}
