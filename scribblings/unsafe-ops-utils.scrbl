#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribblings/icons
          (for-label racket
                     (planet williams/science/science-with-graphics)))

@title[#:tag "unsafe-ops-utils"]{Unsafe Operations Utilities}

All functions and forms provided by @racket[racket] check their arguments to ensure that the arguments conform to contracts and other constraints. For example, @racket[vector-ref] checks its arguments to ensure that the first argument is a vector, that the second argument is an exact integer, and that the second argument is between @math{0} and one less than the vector’s length, inclusive.

Functions provided by @racket[racket/unsafe/ops] are unsafe. They have certain constraints, but the constraints are not checked, which allows the system to generate and execute faster code. If arguments violate an unsafe function’s constraints, the function’s behavior and result is unpredictable, and the entire system can crash or become corrupted.

For efficiency, the Science Collection often uses unsafe operations internally --- particularly for computationally intensive operations. However, the combination unchecked operations, which bypass contract checks, and unsafe operations could result in the consequences described above. The Science Collection protects its uses of unsafe arithmetic operations using the functions and macros described in this chapter. They are described here so that user code can, in turn, use similar protections.

The functions and macros described in the chapter can be made available using the form:

@defmodule[(planet williams/science/unsafe-ops-utils)]

These routines are used to assure that a real variable (or vector) is represented as a floating-point number --- called an inexact-real in Racket. This can be used to protect code that uses the new unsafe floating-point operations.

@defproc[(real->float (x real?)) inexact-real?]{
Returns an inexact real (i.e., a float) given a real @racket[x]. Raises an error if @racket[x] is not a real. This can be used to insure a real value is a float, even in unsafe code.}

@defproc[(real-vector->float-vector (v (vector-of real?))) (vector-of inexact-real?)]{
Returns a vector of inexact reals (i.e., floats) given a vector of reals, @racket[v]. Raises an error if any element of @racket[v] is not a real.}

@defproc[(real-vector->flvector (v (vector-of real?))) flvector?]{
Returns an flvector (i.e., floats) given a vector of reals, @racket[v]. Raises an error if any element of @racket[v] is not a real.}

@defform[(with-fixed (x ...)
           expr ...)]{
Executes the @racket[expr]s with each @racket[x] guaranteed to the a fixed integer. All of the @racket[x]s must be identifiers.}

@defform[(with-float (x ...)
           expr ...)]{
Executes the @racket[expr]s with each @racket[x] guaranteed to the a float. All of the @racket[x]s must be identifiers.}
                     