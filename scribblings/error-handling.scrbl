#lang scribble/doc
@(require scribble/manual
          scribblings/icons
          (for-label racket
                     (planet williams/science/science-with-graphics)))

@title[#:tag "error-handling"]{Error Handling}

@local-table-of-contents[]

This chapter describes how error handling is performed in the Science Collection and its error handling conventions.

@section{Contracts}

@(margin-note magnify "For more information, see " @link["http://docs.racket-lang.org/reference/contracts.html"]{Contracts} " in the Racket Reference Manual or " @link["http://docs.racket-lang.org/guide/contracts.html"]{Contracts} " in the Racket Guide.")

The Science Collection uses contracts to define and enforce the interfaces for all of the functions provided by the modules in the collection.  This ensures that all calls to these functions are checked for proper number and type of arguments (and results), as well as range checking and interargument constraints, where practical.

All of the function definitions in the manual include a specification of the contract for the function.

The following examples show some of the different contract specifications for functions and how to interpret them.  All of these examples are from the statistics module (@filepath{statistics.rkt}).

@subsection{Fixed Number of Arguments with a Single Result}

@bold{Example:} @racket[mean] from the statistics module.

@racketblock[
(provide/contract
 (mean
  (-> (vectorof real?) real?)))
]
This contract specifies a function with one argument that must match the contract @racket[(vectorof real?)] and returns a single result that must match the contract @racket[real?].

@verbatim{
> (require (planet williams/science/statistics))
> (mean #(1 2 3 4))
2.5
> (mean #(1 2 5 a))
top-level broke the contract (-> (vectorof real?) real?) on mean; expected <(vectorof real?)>, given: #(1 2 5 a)
}

@subsection{Multiple Lambda Forms (@racket[case-lambda])}

@bold{Example:} @racket[variance] from the statistics module.

@(margin-note "PLT Scheme 4.0 --- Racket was formerly known as PLT Scheme --- introduced direct support of optional arguments in " @racket[lambda]".  At some point, the uses of multiple lambda forms to specify optional arguments will be converted to single " @racket[lambda] " forms with optional arguments.")

@racketblock[
(provide/contract
 (variance
  (case-> (-> (vectorof real?) real? (>=/c 0.0))
          (-> (vectorof real?) (>=/c 0.0)))))
]
The contract specifies multiple lambda forms using @racket[case->].  The first case specifies a function with two arguments, @racket[data] and @racket[mu], that must match the contracts @racket[(vectorof real?)] and @racket[real?] and returns a single value that must match the contract @racket[(>=/c 0.0)].  The second case specifies a function with a single argument, @racket[data] that muct match the contract @racket[(vectorof real?)] and returns a single result that must match the contract @racket[(>=/c 0.0)].

@verbatim{
> (require (planet williams/science/statistics))
> (variance #(1 2 3 4))
1.6666666666666665
> (variance #(1 2 3 4) 2.5)
1.6666666666666665
> (variance #(1 2 3 4) 'a)
top-level broke the contract 
  (case->
   (-> (vectorof real?) real? (>=/c 0.0))
   (-> (vectorof real?) (>=/c 0.0)))
on variance; expected <real?>, given: a
}

@subsection{Interparameter Constraints}

@bold{Example:} @racket[weighted-mean] from the statistics module.

@racketblock[
(provide/contract
 (weighted-mean
  (-> ((w (vectorof real?))
       (data (and/c (vectorof real?)
                    (lambda (x)
                      (= (vector-length w)
                         (vector-length data)))))))))
]
The contract specified a function that takes two arguments.  The first argument, @racket[w] must be a vector of real numbers.  The second argument, @racket[data], must be a vector of real numbers that must be the same length as @racket[w].  The function returns one result that must match the contract @racket[real?].
    
@verbatim{
> (require (planet williams/science/statistics))
> (weighted-mean #(1 2 3 4) #(4 3 2 1))
2.0
> (weighted-mean #(1 2 3 4) #(4 3 2))
top-level broke the contract 
  (->r ((w ...) (data ...)) ...)
on weighted-mean; expected <(and/c (vectorof real?) ...\0\statistics.ss:152:21)>, given: #(4 3 2)
}

@subsection{Unchecked Operations}

Constraint checking does incur an execution-time penalty that the user may wish to avoid. For example, the algorithm for the @racket[median-from-sorted-data] function has a computation complexity of @math{O(1)}, but the constraint check to insure the input is indeed a sorted sequence has a computational complexity of @math{O(n)}. Many of the routines in the Science Collection have a unchecked variant that avoids the constraint check. These functions have "unchecked-" prepended to the corresponding function name. For example, @racket[unchecked-median-from-sorted-data] does not check its argument's constraints.

In general, constraint checking is a good thing. The unchecked versions of functions should only be called when it is known that the arguments do indeed meet their constraints. As of Version 4.0 of the Science Collection, some operations internally use 'unsafe' operations for efficiency. Arguments that don't match their constraints may result in crashing the run-time system.

@section{Infinities and Not-a-Number}

Racket provides @racket[+inf.0] (positive infinity), @racket[-inf.0] (negative infinity), @racket[+nan.0] (not-a-number), and @racket[-nan.0] (same as @racket[+nan.0]).  In general these are contagious and are passed as the result in subsequent numerical computations.  However, operations with infinities and zero (both exact and inecact) can give non-intuitive results.  For example:

@itemize{
         
  @item{@racket[(* 0 +inf.0)] → 0}
  @item{@racket[(/ 0 +inf.0)] → 0}
  @item{@racket[(/ 0 -inf.0)] → 0}
  @item{@racket[(* 0.0 +inf.0)] → @racket[+nan.0]}
  @item{@racket[(/ 0.0 -inf.0)] → -0.0}
  
}

Note that some of these may break naive algorithms.

The Science Collection uses infinities to represent overflow---@racket[+inf.0] for positive overflow and @racket[-inf.0] for negative overflow.  This is used in cases where the arguments to the function are within the range of the function, but the (absolute value of) result is too large to be represented.  For example, @racket[(gamma 200.0)] → @racket[+inf.0].

The Science Collection uses inexact zero to represent underflow---@racket[0.0] for positive underflow and @racket[-0.0] for negative underflow.  This is used in cases where the arguments to the function are within the range of the function, but the (absolute value of) the result is too small to be represented.

The Science Collection uses @racket[+nan.0] for domain errors---where the arguments match the contract, but the value cannot be computed.  For example @racket[(gamma 0.0)] → @racket[+nan.0].

@section{Exceptions}

In Racket, an @deftech{exception} is a change in control flow, typically as the result of an error.

The Science Collection attempts to catch as many errors as possible using contracts.  Contracts raise exceptions when violated.

The Science Collection may raise exceptions for errors other than underflow, overflow, and domain errors.  Also, the underlying functions and/or modules used by the Science Collection may raise exceptions, as may errors in the implementation.
