#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribblings/icons
          (for-label racket
                     (planet williams/science/science-with-graphics)))

@title[#:tag "mathematical-functions"]{Mathematical Functions}

@local-table-of-contents[]

This chapter describes the basic mathematical constants and functions provided by the Science Collection.

The constants and functions described in this chapter are defined in the @filepath{math.ss} file in the Science Collection and are made available using the form:

@defmodule[(planet williams/science/math)]

@section{Mathematical Constants}

The following are the mathematical constants defined by the Science Collection:

@defidform[e]{The base of the exponentials, @math{e}}
@defidform[log2e]{The base two logarithm of @math{e}, @math{log_2 e}}
@defidform[log10e]{The base ten logarithm of @math{e}, @math{log_10 e}}
@defidform[sqrt2]{The square root of two, @math{√2}}
@defidform[sqrt1/2]{The square root of one half, @math{√½}}
@defidform[sqrt3]{The square root of three, @math{√3}}
@(margin-note magnify "The constant " @racket[pi] " was added to " @racketfont{racket/math} ", which is included in the " @racketfont{racket} " language. Therefore, it has been removed from the Science Collection.")
@defidform[2*pi]{Pi time two, @math{2π}}
@defidform[4*pi]{Pi time four, @math{4π}}
@defidform[pi/2]{Pi divided by two, @math{π/2}}
@defidform[pi/4]{Pi divided by four, @math{π/4}}
@defidform[sqrtpi]{The square root of pi, @math{√π}})
@defidform[2/sqrtpi]{Two divided by the square root of pi, @math{2/√π}}
@defidform[1/pi]{The reciprocal of pi, @math{1/π}}
@defidform[2/pi]{Twice the reciprocal of pi, @math{2/π}}
@defidform[ln10]{The natural log of ten, @math{ln 10} or @math{log_e 10}}
@defidform[1/ln10]{The inverse of the natural log of ten, @math{1/ln 10}}
@defidform[ln2]{The natural log of two, @math{ln 2} or @math{log_e 2}}
@defidform[1/ln2]{The inverse of the natural log of two, @math{1/ln 2}}
@defidform[lnpi]{The natural log of pi, @math{ln π} or @math{log_e π}}
@defidform[euler]{Euler's constant, @math{γ}}

@section{Testing for Infinities and Not-a-Number}

Racket provides @racket[+inf.0] (positive infinity), @racket[-inf.0] (negative infinity), @racket[+nan.0] (not a number), and @racket[-nan.0] (same as @racket[+nan.0]) as inexact numerical constants.  The following functions are provided as a convenience for checking for infinities and not-a-number.

Racket now provides the predicate functions @racket[nan?] and @racket[infinite?]. These have been removed from the Science Collection.

@;@defproc[(nan? (x any/c)) boolean?]{
@;Returns true, @racket[#t], if @racket[x] is not-a-number (i.e., equivalent to @racket[+nan.0] or, equivalently, @racket[-nan.0]), and false, @racket[#f], otherwise.  Note that this is not the same as @racket[(not (number? x))], which is true if @racket[x] is not a number.}

@defproc[(infinite (x any/c)) (or/c -1 #f 1)]{
Returns 1 if @racket[x] is positive infinity (i.e., equivalent to @racket[+inf.0]), -1 if @racket[x] is negative infinity (i.e., equivalent to @racket[-inf.0]), and false, @racket[#f], otherwise.  Note that @racket[(finite? x)] is not equivalent to @racket[(not (infinite? x))], since both @racket[finite?] and @racket[infinite?] return false, @racket[#f] for anything that is not a real number.}

@;@defproc[(infinite? (x any/c)) boolean?]{
@;Returns true, @racket[#t], if @racket[x] is infinite---either @racket[+inf.0] or @racket[-inf.0]---and false, @racket[#f], otherwise. Note that @racket[(finite? x)] is not equivalent to @racket[(not (infinite? x))], since both @racket[finite?] and @racket[infinite?] return false, @racket[#f] for anything that is not a real number.}

@defproc[(finite? (x any/c)) boolean?]{
Returns true, @racket[#t], if @racket[x] is a finite real number and false, @racket[#f], otherwise.  Note that @racket[(finite? x)] is not equivalent to @racket[(not (infinite? x))], since both @racket[finite?] and @racket[infinite?] return false, @racket[#f] for anything that is not a real number.}

@section{Elementary Functions}

The following functions provide some elementary mathematical functions that are not provide by Racket.

@defproc[(log1p (x real?)) number?]{
Computes the value of @math{log(1 + x)} in a way that is accurate for small @racket[x].}

@defproc[(expm1 (x real?)) real?]{
Computes the value of @math{exp(x - 1)} in a way that is accurate for small @racket[x].}

@defproc[(hypot (x real?) (y real?)) real?]{
Computes the value of @math{(x^2 + y^2)}@superscript{½} in a way that avoids overflow.}

@defproc[(acosh (x real?)) real?]{
Computes the value of the hyperbolic arccosine, @math{arccosh}, of @racket[x].}

@defproc[(asinh (x real?)) real?]{
Computes the value of the hyperbolic arcsine, @math{arcsinh}, of @racket[x].}

@defproc[(atahh (x real?)) real?]{
Computes the value of the hyperbolic arctangent, @math{arctanh}, of @racket[x].}

@defproc[(ldexp (x real?) (e integer?)) real?]{
Computes the value of @math{x × 2^e}.}

@defproc[(frexp (x real?)) (values real? integer?)]{
Splits the real number @racket[x] into a normalized fraction @math{f} and exponent @math{e} such that @math{x = f × 2^e} and @math{0.5 ≤ f < 1}.  The function returns @math{f} and @math{e} as multiple values.  If @racket[x] is zero, both @math{f} and @math{e} are returned as zero.}

@section{Testing the Sign of Numbers}

@defproc[(sign (x real?)) (integer-in -1 1)]{
Returns the sign of @racket[x] as 1 if @math{x ≥ 0} and -1 if @math{x < 0}.  Note that the sign of zero is positive, regardless of its floating-point sign bit.}

@section{Approximate Comparisons of Real Numbers}

It is sometimes useful to be able to compare two real (in particular, floating-point) numbers approximately to allow for rounding and truncation errors.  The following functions implements the approximate floating-point comparison algorithm proposed by D.E. Knuth in Section 4.2.2 of @italic{Seminumerical Algorithms} (3rd edition) @cite["Knuth"].

@defproc[(fcmp (x real?) (y real?) (epsilon real?)) (integer-in -1 1)]{
Determines whether @racket[x] and @racket[y] are approximately equal to within a relative accuracy, @racket[epsilon].  The relative accuracy is measured using an interval of @math{2 × delta}, where @math{delta = 2^k × epsilon} and @math{k} is the maximum base 2 exponent of @racket[x] and @racket[y] as computed by the function @racket[frexp].  If @racket[x] and @racket[y] lie within this interval, they are considered equal and the function returns 0.  Otherwise, if @math{x < y}, the function returns -1, or if @math{x > y>}, the function returns 1.}

The implementation of this function is based on the packege @racketfont{fcmp} by T.C. Belding.

@section{Log10 and Decibels (dB)}

@defproc[(log10 (x real?)) real?]{
Returns the log base 10 of @racket[x], @math{log_10(x)}.}

@defproc[(dB (x real?)) real?]{
Returns the value of @racket[x] in decibels, @math{10*log_10(x)}.}
