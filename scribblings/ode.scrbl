#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribblings/icons
          (for-label racket
                     (planet williams/science/science-with-graphics)))

@title[#:tag "ode"]{Ordinary Differential Equations}

@local-table-of-contents[]

This chapter describes the functions for solving ordinary differential equation (ODE) initial value problems provided by the Science Collection.  The functions include a variety of low-level methods, such as Runge-Kutta and Bulirsch-Stoer routines, and higher level components for adaptive step-size control.  The components can be combined by the user to achieve the desired solution with full access to any intermediate steps, as needed.  

The functions described in this chapter are defined in the @filepath{ode-initval.rkt} file in the Science Collection and are made available using the form:

@defmodule[(planet williams/science/ode-initval)]

@section{Defining the ODE System}

The routines solve the general n-dimensional first-order system:

@math{dy_i(t)/dt = f_i(t, y_1(t), ..., y_n(t))}

for @math{i = 1, ..., n}.  The stepping functions rely on the vector of derivatives @math{f_i} and the Jacobian matrix @math{J_ij = df_i(t, y(t))/dy_j}.  A system of equations is defined using the @racket[ode-system] structure.

@defstruct[ode-system ([function (-> real? real? (vectorof real?) (listof any/c) any)]
                       [jacobian (or/c (-> real?
                                           real?
                                           (vectorof real?)
                                           (vectorof real?)
                                           (listof any/c)
                                           any)
                                       false/c)]
                       [dimension exact-positive-integer?]
                       [params (listof any/c)])]{
Defines a general ODE system with arbitrary parameters.
        
@itemize{
         
  @item{@racket[function]---a function @racket[(lambda (t y dydt params) ...)].  This function should store the elements of @math{f_i(t, y, params)} in the vector @racket[dydt], for arguments @racket[t] and @racket[y] and parameters @racket[params].}
  
  @item{@racket[jacobian]---a function @racket[(lambda (t y dfdy dfdt params) ...)].  This function should store the elements of @math{df_i(t, y, params)/dt} in the vector @racket[dfdt] and the Jacobian matrix @math{J_ij} in the vector @racket[dfdy] as a row-major matrix @math{J_ij = dfdy(i * dimension + j)}, where @math{dimension} is the dimension of the system.  Some of the simpler solver algorithms do not make use of the Jacobian matrix, so it is not always strictly necessary to provide it.  The @racket[jacobian] field of the structure can be @racket[#f] for those algorithms.  However, it is useful to provide the Jacobian to allow the solver algorithms to be interchanged.  The best algorithms make use of the Jacobian.}
  
  @item{@racket[dimension]---the dimension of the system of equations.}
  
  @item{@racket[params]---a list if the arbitrary parameters of the system.}
  
  }}

@defproc*[(((ode-system-function-eval (ode-system ode-system?)
                                      (t real?)
                                      (y (vectorof real?))
                                      (dydt (vectorof real?)))
            void?)
           ((unchecked-ode-system-function-eval (ode-system ode-system?)
                                                (t real?)
                                                (y (vectorof real?))
                                                (dydt (vectorof real?)))
            void?))]{
Evaluates the system function for @racket[ode-system]. This function should store the elements of @math{f_i(t, y, params)} in the vector @racket[dydt], for arguments @racket[t] and @racket[y] and parameters @racket[params].}
                    
@defproc*[(((ode-system-jacobian-eval (ode-system ode-system?)
                                      (t real?)
                                      (y (vectorof real?))
                                      (dydt (vectorof real?))
                                      (dfdt (vectorof real?)))
            void?)
           ((unchecked-ode-system-jacobian-eval (ode-system ode-system?)
                                                (t real?)
                                                (y (vectorof real?))
                                                (dydt (vectorof real?))
                                                (dfdt (vectorof real?)))
            void?))]{
Evaluates the Jacobian function for @racket[ode-system]. This function should store the elements of @math{df_i(t, y, params)/dt} in the vector @racket[dfdt] and the Jacobian matrix @math{J_ij} in the vector @racket[dfdy] as a row-major matrix @math{J_ij = dfdy(i * dimension + j)}, where @math{dimension} is the dimension of the system.}

@subsection{Stepping Functions}

The lowest level components are the @italic{stepping functions} that advance a solution from time @math{t} to @math{t + h} for a fixed step size @math{h} and estimate the resulting local error.

@defproc[(ode-step-type? (x any/c)) boolean?]{
Returns true, @racket[#t], if @racket[x] is an ODE step type and false, @racket[#f], otherwise.  ODE step types are defined by the different ODE solvers.}

@defproc[(ode-step? (x any/c)) boolean?]{
Returns true, @racket[#t], if @racket[x] is an ODE step and false, @racket[#f], otherwise.}

@defproc[(make-ode-step (step-type ode-step-type?) (dim exact-non-negative-integer?)) ode-step?]{
Returns a newly created instance of a stepping function of type @racket[step-type] for a system of @racket[dim] dimensions.}

@defproc*[(((ode-step-reset (step ode-step?)) void?)
           ((unchecked-ode-step-reset (step ode-step?)) void?))]{
Resets the stepping function @racket[step].  It should be used whenever the next use of @racket[step] will not be a continuation of a previous step.}

@defproc[(ode-step-name (step ode-step?)) string?]{
Returns the name of the stepping function @racket[step] as a string.}

@defproc[(ode-step-order (step ode-step?)) natural-number/c]{
Returns the order of the stepping function @racket[step] on theprevious step.  This order can vary of the stepping function itself is adaptive.}

@defproc*[(((ode-step-apply (step ode-step?)
                            (t real?)
                            (h real?)
                            (y (vector-of real?))
                            (y-err (vector-of real?))
                            (dydt-in (vector-of real?))
                            (dydt-out (vector-of real?))
                            (dydt ode-system?))
            void?)
           ((unchecked-ode-step-apply (step ode-step?)
                                      (t real?)
                                      (h real?)
                                      (y (vector-of real?))
                                      (y-err (vector-of real?))
                                      (dydt-in (vector-of real?))
                                      (dydt-out (vector-of real?))
                                      (dydt ode-system?))
            void?))]{
Applies the stepping function @racket[step] to the system of equations defined by @racket[dydt] using the step size @racket[h] to advance the system from time @racket[t] and state @racket[y] to time @math{t + h}.  The new state of the system is stored in @racket[y] on output with an estimate of the absolute error in each component stored in @racket[y-err].  If the argument @racket[dydt-in] is not @racket[#f], it should be a vector containing the derivatives for the system at time @racket[t] on input.  This is optional as the derivatives will be computed internally if they are not provided, but allows the reuse of existing derivative information.  On output the new derivatives of the system at time @math{t + h} will be stored in the vector @racket[dydt-out], if it is not @racket[#f].}

The following stepping algorithms are available.

@defidform[rk2-ode-type]{
Embedded Runge-Kutta (2, 3) method.}

@defidform[rk4-ode-type]{
4@superscript{th} order (classical) Runge-Kutta.}

@defidform[rkf45-ode-type]{
Embedded (classical) Runge-Kutta-Fehberg (4, 5) method.  This method is a good general-purpose integrator.}

@section{Adaptive Step-Size Control}

The control function examines the proposed changes to the solution and its error estimate produced by a stepping function and attempts to determine the optimal step size for a user-specified level of error.

@defproc[(standard-control-new (eps-abs real?)
                               (eps-rel real?)
                               (a-y real?)
                               (a-dydt real?))
         ode-control?]{
Creates a new standard control object.  The standard control object is a four parameter heuristic based on absolute and relative errors @racket[eps-abs] and @racket[eps-rel] and scaling factors @racket[a-y] and @racket[a-dydt] for the system state @math{y{t}} and derivatives @math{y'(t)}, respectively.
    
The step size adjustment procedure for this method begins by computing the desired error level @math{D_i}, for each component

@math{D_i = eps@subscript{abs} + eps@subscript{rel} × (a_y|y_i| + a@subscript{dydt}h|y'_i|)}

and comparing it with the observed error @math{E_i = |yerr_i|}.  If the observed error @math{E} exceeds the desired error level @math{D} by more than 10% for any component, then the method reduces the step size by an appropriate factor

@math{h@subscript{new} = h@subscript{old} × S × (E/D)@superscript{-1/q}}

where @math{q} is the consistency order of the method (e.g., @math{q = 4} for 4(5) embedded RK), and @math{S} is a safety factor of 0.9.  The ratio @math{E/D} is taken to be the maximim of the ratios @math{E_i/D_i}.

If the observed error @math{E} is less than 50% of the desired level @math{D} for the maximum ratio @math{E_i/D_i}, then the algorithm takes the opportunity to increase the step size to bring the error in line with the desired level

@math{h@subscript{new} = h@subscript{old} × S × (E/D)@superscript{-1/(q+1)}}.

This emcompasses all the standard scaling methods.  To avoid uncontrolled changes in the step size, the overall scaling factor is limited to the range ⅕ to 5.}
                      
@defproc[(control-y-new (eps-abs real?) (eps-rel real?)) ode-control?]{
Creates a new control object that will keep the local error within an absolute error of @racket[eps-abs] and relative error @racket[eps-rel] with respect to the solution @math{y_i(t)}.  This is equivalent to the standard control object with @racket[a-y] = 1 and @racket[a-dydt] = 0.}
                      
@defproc[(control-yp-new (eps-abs real?) (eps-rel real?)) ode-control?]{
Creates a new control object that will keep the local error within an absolute error of @racket[eps-abs] and relative error @racket[eps-rel] with respect to the derivatives of the solution @math{y'_i(t)}.  This is equivalent to the standard control object with @racket[a-y] = 0 and @racket[a-dydt] = 1.}

@;@defproc[(control-scaled-new (eps-abs real?)
@;                             (eps-rel real?)
@;                             (a-y real?)
@;                             (a-dydt real?)
@;                             (scale-abs (vector-of real?))
@;                             (dim natural-number/c))
@;         ode-control?]{
@;Creates a new control object that uses the same algorithm as @racket[ode-control-standard-new], but with an @;absolute error that is scaled for each component by the array @racket[scale-abs].  The formula for @math{D_i} is
@;        
@;@math{D_i = eps@subscript{abs} × s_i + eps@subscript{rel} × (a_y|y_i| + a@subscript{dydt}h|y'_i|)}
@;
@;where @math{s_i} is the @math{i@superscript{th}} component of the array @racket[scale-abs].  The same error @;control heuristic is used by the Matlab ODE suite.}
                      
@defproc[(make-ode-control (control-type ode-control-type?)) ode-control?]{
Returns a new instance of a control function of type @racket[control-type].  This function is only needed for defining new types of control functions.  For most purposes, the standard control functions describes above should be sufficient.}

@defproc[(ode-control-init (control ode-control?)
                           (eps-abs real?)
                           (eps-rel real?)
                           (a-y real?)
                           (a-dydt real?))
         any]{
Initializes the control function @racket[control] with the parameters @racket[eps-abs] (absolute error), @racket[eps-rel] (relative error), @racket[a-y] (scaling factor for @math{y}), and @racket[a-dydt] (scaling factor for derivatives.)}

@defproc[(ode-control-h-adjust (control ode-control?)
                               (step ode-step?)
                               (y (vector-of real?))
                               (y-err (vector-of real?))
                               (dydt (vector-of real?))
                               (h box?))
         (integer-in -1 1)]{
Adjusts the step size @racket[h] using the control function @racket[control] and the current values of @racket[y], @racket[y-err], and @racket[dydt].  The stepping function @racket[step] is also needed to determine the order of the method.  If the error in the @racket[y] values @racket[y-err] is found to be too large, then the step size @racket[h] is reduced and the function returns -1.  If the error is sufficiently small, then @racket[h] may be increased and 1 is returned.  The function returns 0 if the step size is unchanged.  The goal of the function is to estimate the largest step size that satisfies the user-specified accuracy requirements for the current point.}
                           
@defproc[(ode-control-name (control ode-control?)) string?]{
Returns the name of the control function @racket[control].  For example,
        
@racketblock[
(printf ("control method is '~a'~n"
         (ode-control-name control)))]

would print something like

@verbatim{
control method is 'standard'}
}

@section{Evolution}

The highest-level of the system is the evolution function that combines the results of a stepping function and control function to reliably advance the solution forward over an interval @math{(t_0, t_1)}.  If the control function signals that the step size should be decreased, the evolution function backs out of the current step and tries the proposed smaller step size.  This process is continued until an acceptable step size is found.

@defproc[(ode-evolve? (x any/c)) boolean?]{
Returns true, @racket[#t], if @racket[x] an ODE evolver.}

@defproc[(make-ode-evolve (dim exact-positive-integer?)) ode-evolve?]{
Returns a new instance of an evolution function for a system of @racket[dim] dimensions.}

@defproc[(ode-evolve-count (evolve ode-evolve?)) exact-nonnegative-integer?]{
Returns the number of steps @racket[evolve] has iterated.}

@defproc[(ode-evolve-failed-steps (evolve ode-evolve?)) exact-nonnegative-integer?]{
Returns the number of steps for @racket[evolve] that have failed --- i.e., required adjustment to the time step.}

@defproc*[(((ode-evolve-apply (evolve ode-evolve?)
                              (control ode-control?)
                              (step ode-step?)
                              (system ode-system?)
                              (t box?)
                              (t1 real?)
                              (h box?)
                              (y (vector-of real?)))
            any)
           ((unchecked-ode-evolve-apply (evolve ode-evolve?)
                                        (control ode-control?)
                                        (step ode-step?)
                                        (system ode-system?)
                                        (t box?)
                                        (t1 real?)
                                        (h box?)
                                        (y (vector-of real?)))
            any))]{
Evolves the system from time @racket[t] and position @racket[y] using the stepping function @racket[step].  The new time and position are stored in @racket[t] and @racket[y] on output.  The initial step size is taken as @racket[h], but this may be modified using the control function @racket[control] to achieve the appropriate bound, if necessary.  The routine may make several calls to @racket[step] in order to determine the optimum step size.  If the step size has been changed, the value of @racket[h] will be modified on output.  The maximum time @racket[t1] is guaranteed not to be exceeded by the time step.  On the final time step, the value of @racket[t] will be set to @racket[t1] exactly.}

@defproc*[(((ode-evolve-reset (evolve ode-evolve?)) any)
           ((unchecked-ode-evolve-reset (evolve ode-evolve?)) any))]{
Resets the evolution function @racket[evolve].  It should be used whenever the next use of @racket[evolve] will not me a continuation of a previous step.}

@section{ODE Examples}

@bold{Example:} The following programs solve the second-order nonlinear Van der Pol oscillator equation

@math{x''(t) + μx'(t)(x(t)_2 - 1) + x(t) = 0}.

This can be converted into a first order system suitable for use with the routines described in this chapter by introducing a separate variable for the velocity, @math{y = x'(t)}

@math{x' = y}

@math{y' = -r + μy(1 - x_2)}.

The following example integrates the above system of equations from @math{t = 0.0} to @math{100.0} in increments of @math{0.01} using a 4@superscript{th} order Runge-Kutta stepping function.

@racketmod[
racket
(require (planet williams/science/ode-initval)
         plot)

(define (func t y f params)
  (let ((mu (car params))
        (y0 (vector-ref y 0))
        (y1 (vector-ref y 1)))
    (vector-set! f 0 y1)
    (vector-set! f 1 (- (- y0) (* mu y1 (- (* y0 y0) 1.0))))))

(define (main)
  (let* ((type rk4-ode-type)
         (step (make-ode-step type 2))
         (mu 10.0)
         (system (make-ode-system func #f 2 (list mu)))
         (t 0.0)
         (t1 100.0)
         (h 1.0e-2)
         (y (vector 1.0 0.0))
         (y-err (make-vector 2))
         (dydt-in (make-vector 2))
         (dydt-out (make-vector 2))
         (y0-values '())
         (y1-values '()))
    (ode-system-function-eval system t y dydt-in)
    (let loop ()
      (when (< t t1)
        (ode-step-apply step t h
                        y y-err
                        dydt-in
                        dydt-out
                        system)
        ;(printf "~a ~a ~a~n" t (vector-ref y 0) (vector-ref y 1))
        (set! y0-values (cons (vector t (vector-ref y 0)) y0-values))
        (set! y1-values (cons (vector t (vector-ref y 1)) y1-values))
        (vector-set! dydt-in 0 (vector-ref dydt-out 0))
        (vector-set! dydt-in 1 (vector-ref dydt-out 1))
        (set! t (+ t h))
        (loop)))
    (printf "~a~n" (plot (points y0-values)
                         #:x-min 0.0
                         #:x-max 100.0
                         #:x-label "X"
                         #:y-min -2.0
                         #:y-max 2.0
                         #:y-label "Y"))
    (printf "~a~n" (plot (points y1-values)
                         #:x-min 0.0
                         #:x-max 100.0
                         #:x-label "X"
                         #:y-label "Y"))))
]

The following figures show the resulting plots.

@image["scribblings/images/ode-example-1-y0-plot.png"]

@image["scribblings/images/ode-example-1-y1-plot.png"]

@bold{Example:} The following example evolves the above system of equations from @math{t = 0.0} to @math{100.0} maintaining an error in the @math{y} values of 10@superscript{-6} using a 4@superscript{th} order Runge-Kutta stepping function.

@racketmod[
racket
(require (planet williams/science/ode-initval)
         plot)

(define (func t y f params)
  (let ((mu (car params))
        (y0 (vector-ref y 0))
        (y1 (vector-ref y 1)))
    (vector-set! f 0 y1)
    (vector-set! f 1 (- (- y0) (* mu y1 (- (* y0 y0) 1.0))))))

(define (main)
  (let* ((type rk4-ode-type)
         (step (make-ode-step type 2))
         (control (control-y-new 1.0e-6 0.0))
         (evolve (make-ode-evolve 2))
         (mu 10.0)
         (system (make-ode-system func #f 2 (list mu)))
         (t (box 0.0))
         (t1 100.0)
         (h (box 1.0e-6))
         (y (vector 1.0 0.0))
         (y0-values '())
         (y1-values '()))
    (let loop ()
      (when (< (unbox t) t1)
        (ode-evolve-apply
         evolve control step system
         t t1 h y)
        (set! y0-values (cons (vector (unbox t) (vector-ref y 0)) y0-values))
        (set! y1-values (cons (vector (unbox t) (vector-ref y 1)) y1-values))
        (loop)))
    (printf "Number of iterations   = ~a~n" (ode-evolve-count evolve))
    (printf "Number of failed steps = ~a~n" (ode-evolve-failed-steps evolve))
    (printf "~a~n" (plot (points y0-values)
                         #:x-min 0.0
                         #:x-max 100.0
                         #:x-label "X"
                         #:y-min -2.0
                         #:y-max 2.0
                         #:y-label "Y"))
    (printf "~a~n" (plot (points y1-values)
                         #:x-min 0.0
                         #:x-max 100.0
                         #:x-label "X"
                         #:y-label "Y"))))
]

The following shows the printed output and resulting plots.

@verbatim{
Number of iterations   = 84575
Number of failed steps = 352}

@image["scribblings/images/ode-example-2-y0-plot.png"]

@image["scribblings/images/ode-example-2-y1-plot.png"]
