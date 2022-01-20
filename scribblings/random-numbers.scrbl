#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribblings/icons
          (for-label racket
                     (planet williams/science/science-with-graphics)))

@title[#:tag "random-numbers"]{Random Number Generation}

@local-table-of-contents[]

The Science Collection provides additional functionality to the Racket implementation of SRFI 27 by Sabastian Egner, which, in turn, is a 54-bit implementation of Pierre L'Ecuyer's MRG32k3a pseudo-random number generator.

The functions described in this chapter are defined in the @filepath{random-source.rkt} file in the Science Collection and are made available using the form:

@defmodule[(planet williams/science/random-source)]

@section{The SRFI 27 Specification}

The following functions are defined in the SRFI specification and are, in purn, provided by the @racketfont{random-source} module.  The contract shows here are for documentation purposes only--the Racket implementation of SRFI does not define contracts for its functions.

@defproc[(random-integer (n (integer-in 1 +inf.0))) natural-number/c]{
Returns the next integer in @math{{0, ..., n - 1}} obtained from the @racket[default-random-source].  Subsequent results of this function appear to be independent uniformly distributed over the range @math{{0, ..., n - 1}}.  The argument @racket[n] must be a positive integer, ptherwise an error is signaled.}

@defproc[(random-real) (real-in 0.0 1.0)]{
Returns the next number, @math{x}, @math{0 < x < 1}, obtained from @racket[default-random-source].  Subsequent results of this procedure  appear to be independently uniformly distributed.}

@defidform[default-random-source]{
Defines the random source from which @racket[random-integer] and @racket[random-real] have been derived using @racket[random-source-make-integers] and @racket[random-source-make-reals].  Note that an assignment to @racket[default-random-source] does not change the behavior of @racket[random-real] or @racket[random-integer]; it is strongly recommended not to assign a new value to @racket[default-random-source].}

@defproc[(make-random-source) random-source?]{
Creates a new random source.  A random source created with @racket[make-random-source] represents a deterministic stream of random bits.  Each random stream obtained as @racket[make-random-source] generates the same stream of values unless the state is modified with one of the functions below.}

@defproc[(random-source? (x any/c)) boolean?]{
Returns true, @racket[#t], if @racket[x] is a random source, otherwise false, @racket[#f], is returned.}

@defproc*[(((random-source-state-ref (s random-source?)) any)
           ((random-source-state-set! (s random-source?) (state any/c)) any))]{
Get and set the current state of the random source @racket[s].  The implementation of the internal state of a random source is not defined.}

@defproc[(random-source-randomize! (s random-state?)) any]{
Makes an effort to set the state of the random source @racket[s] to a truly random state.}

@defproc[(random-source-pseudo-randomize! (s random-source?) (i natural-number/c) (j natural-number/c)) any]{
Changes the state of the random source @racket[s] into the initial state of the (@racket[i], @racket[j])@superscript{th} independent random source, where @racket[i] and @racket[j] are non-negative integers.  This procedure provides a mechanism to obtain a large number of independent random sources, indexed by two integers.  In contract to @racket[random-source-randomize!], this procedure is entirely deterministic.}

@defproc[(random-source-make-integers (s random-source)) (-> (integer-in 1 +inf.0) natural-number/c)]{
Returns a procedure to generate random integers using the random source @racket[s].  The resulting argument takes a single argument @racket[n], which must be a positive integer, and returns the next independent uniformly distributed integer from the interval @math{{0, ..., n - 1}} by advancing the state of the random source @racket[s].}

@defproc*[(((random-source-make-reals (s random-source?)) (-> (real-in 0.0 1.0)))
           ((random-source-make-reals (s random-source?) (unit (real-in 0.0 1.0))) (-> (real-in 0.0 1.0))))]{
Obtains a procedure @math{rand} to generate random real numbers @math{0 < x < 1} using the random source @racket[s]. The procedure @math{rand} is called without arguments.
        
The optional parameter @racket[unit] determines the type of numbers being produced by @math{rand} and the quantization of the output. @racket[unit] must be a number such that @math{0 < unit < 1}. The numbers created by @math{rand} are of the same numerical type as @racket[unit] and the potential output values are spaced by at most @racket[unit]. One can imagine @math{rand} to create numbers as @math{x*unit} where @math{x} is a random integer in @math{{1, ..., floor(1/unit)-1}}. Note, however, that this need not be the way the values are actually created and that the actual resolution of @math{rand} can be much higher than @racket[unit]. In case @racket[unit] is absent it defaults to a reasonably small value (related to the width of the mantissa of an efficient number format).}
                                                                                                            
@section{Additional Random Number Functionality}

The Science Collection provides additional functionality to that provided by SRFI 27.

@subsection{The @racket[current-random-source] parameter}

The main additional functionality is to define a parameter, @racket[current-random-source], that provides a separate random source reference for each thread.  The default value for this random source reference is @racket[default-random-source].

The use of the @racket[current-random-source] parameter overcomes the difficulty with assignment to @racket[default-random-source].  However, the routines @racket[random-integer] and @racket[random-real] use the @racket[default-random-source] variable and are unaware of the @racket[current-random-source] parameter.

@defparam[current-random-source s random-source?]{
Gets or sets the current random source.  A guard procedure ensures that the value of @racket[current-random-source] is indeed a random source, as determined by @racket[random-source?], otherwise a type error is raised.}

@defform[(with-random-source s
           body ...+)]{
Evaluates the body with @racket[current-random-source] bound to the random source @racket[s].}

@defform[(with-new-random-source
           body ...+)]{
Evaluates the body with @racket[current-random-source] bound to a newly created random source.}

@subsection{Uniform Random Numbers}

The Science Collection provides alternatives to the @racket[random-integer] and @racket[random-real] functions that are aware of the @racket[current-random-source] parameter.  The also provide a more convenient interface than @racket[random-source-make-integers] and @racket[random-source-make-reals].

@defproc*[(((random-uniform-int (s random-source?) (n (integer-in 1 +inf.0))) natural-number?)
           ((random-uniform-int (n (integer-in 1 +inf.0))) natural-number?))]{
Returns the next integer in @math{{0, ..., n - 1}} obtained from the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not specified.  Subsequent results of this function appear to be independently uniformly distributed over the range @math{{0, ..., n - 1}}.  The argument @racket[n] must be a positive integer.}

@defproc*[(((random-uniform (s random-source?)) (real-in 0.0 1.0))
           ((random-uniform) (real-in 0.0 1.0)))]{

Returns the next number @math{x}, @math{0 < x < 1}, obtained from the random source @racket[s] or @racket[(current-random-source)] if @racket[s] is not specified.  Subsequent results of this function appear to be independently uniformly distributed.}

@subsection{Miscellaneous Functions}

These functions provide an alternative set of functions to get or set the state of a random-state.  These functions match the conventions for structures in Racket.

@defproc[(random-source-state (s random-source?)) any]{
The same as @racket[random-source-state-ref].}

@defproc[(set-random-source-state! (s random-state?) (state any/c)) any]{
The same as @racket[random-source-state-set!].}

@subsection{Random Source Vectors}

These functions provide a convenient method for generating a vector of repeatable random sources.

@defproc*[(((make-random-source-vector (n natural-number/c) (i natural-number/c)) (vectorof random-source?))
           ((make-random-source-vector (n natural-number/c)) (vector-of random-source?)))]{
Returns a vector of random sources of length @racket[n].  If @racket[i] is provided, the @math{j}@superscript{th} random source is initialized using @racket[(random-source-pseudo-randomize! s i j)].  If @racket[i] is not provided, the @math{i}@superscript{th} random source is initialized using @racket[(random-source-pseudo-randomize! s i 0)].  Note that this is not the same as having @racket[i] default to 0.}

@section{Random Number Examples}

@bold{Example:} Histogram of uniform random numbers.

@racketmod[
racket
(require (planet williams/science/random-source)
         (planet williams/science/histogram-with-graphics))

(let ((h (make-histogram-with-ranges-uniform 40 0 1))
      (s (make-random-source)))
  (random-source-randomize! s)
  (with-random-source s
    (for ((i (in-range 10000)))
      (histogram-increment! h (random-uniform))))
  (histogram-plot h "Histogram of Uniform Random Numbers"))]

The following figure shows an example of the resulting histogram.

@image["scribblings/images/random-uniform-hist.png"]

@bold{Example:} Histogram of uniform random integers.

@racketmod[
racket
(require (planet williams/science/random-source)
         (planet williams/science/histogram-with-graphics))

(let ((h (make-discrete-histogram))
      (s (make-random-source)))
  (random-source-randomize! s)
  (with-random-source s
    (for ((i (in-range 10000)))
      (discrete-histogram-increment! h (random-uniform-int 10))))
  (discrete-histogram-plot
   h "Histogram of Uniform Random Integers"))]

The following figure shows an example of the resulting histogram.

@image["scribblings/images/random-uniform-int-hist.png"]
