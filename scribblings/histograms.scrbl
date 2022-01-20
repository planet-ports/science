#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribblings/icons
          (for-label racket
                     (planet williams/science/science-with-graphics)))

@title[#:tag "histograms"]{Histograms}

@local-table-of-contents[]

This chapter describes the functions for creating and using histograms provided by the Science Collection.  Histograms provide a convenient way of summarizing the distribution of a set of data.  A @italic{histogram} contains a vector of bins that count the number of events falling within a given range.  The bins of a histogram can be used to record both integer and non-integer distributions.

The ranges can be either continuous or discrete over a range.  For continuous ranges, the width of these ranges can be either fixed or arbitrary.  Also, for continuous ranges, both one- and two-dimensional histograms are supported.

@;----------
@; Histograms
@;----------

@section{Histograms}

The histogram functions described in this section are defined in the @filepath{histogram.rkt} file in the Science Collection and are made available using the form:

@defmodule[(planet williams/science/histogram)]

@defproc[(histogram? (x any/c)) boolean?]{
Returns true, @racket[#t], if @racket[x] is a histogram and false, @racket[#f], otherwise.}

@subsection{Creating Histograms}

@defproc[(make-histogram (n exact-positive-integer?)) histogram?]{
Returns a new, empty histogram with @racket[n] bins and @math{n + 1} range entries.  The range entries must be set with a subsequent call to @racket[set-histogram-ranges!] or @racket[set-histogram-ranges-uniform!].}

@defproc[(make-histogram-with-ranges-uniform (n exact-positive-integer?)
                                             (x-min real?)
                                             (x-max (>/c x-min)))
         histogram?]{
Returns a new, empty histogram with @racket[n] bins.  The @math{n + 1} range entries are initialized to provide @racket[n] uniform width bins from @racket[x-min] to @racket[x-max].}

@subsection{Updating and Accessing Histogram Elements}

@defproc[(histogram-n (h histogram?)) exact-positive-integer?]{
Returns the number of bins in the histogram @racket[h].}

@defproc[{histogram-ranges (h histogram?)} (vectorof real?)]{
Returns a vector of ranges for the histogram @racket[h].  The length of the vector is equal to the number of bins in @racket[h] plus one.}

@defproc[(set-histogram-ranges! (h histogram?)
                               (ranges (and/c (vectorof real?)
                                              (lambda (x)
                                                (= (vector-length ranges)
                                                   (+ (histogram-n h) 1))))))
        void?]{
Sets the ranges for the histogram @racket[h] according to the given @racket[ranges].  The length of the @racket[ranges] vector must equal the number of bins in @racket[h] plus one.  The bins in @racket[h] are also reset.}
              
@defproc[(set-histogram-ranges-uniform! (h histogram?)
                                        (x-min real?)
                                        (x-max (>/c x-min)))
         void?]{
Sets the ranges for the histogram @racket[h] uniformly from @racket[x-min] to @racket[x-max].  The bins in @racket[h] are also reset.}

@defproc[(histogram-bins (h histogram?)) (vectorof real?)]{
Returns the vector of bins for the histogram @racket[h].}

@defproc*[(((histogram-increment! (h histogram?) (x real?)) void?)
           ((unchecked-histogram-increment! (h histogram?) (x real?)) void?))]{
Increments the bin in the histogram @racket[h] containing @racket[x].  The bin value is incremented by one.}

@defproc*[(((histogram-accumulate! (h histogram?) (x real?) (weight (>=/c 0.0))) void?)
           ((unchecked-histogram-accumulate! (h histogram?) (x real?) (weight (>=/c 0.0))) void?))]{
Increments the bin in the histogram @racket[h] containing @racket[x] by the specified @racket[weight].  Since in this implementation bin values are non-megative, the @racket[weight] must be non-negative.}

@defproc[(histogram-get (h histogram?)
                        (i (and/c exact-nonnegative-integer?
                                  (</c (histogram-n h)))))
         (>=/c 0.0)]{
Returns the contents of the @racket[i]@superscript{th} bin of the histogram @racket[h].}

@defproc[(histogram-get-range (h histogram?)
                              (i (and/c exact-non-negative-integer?
                                        (</c (histogram-n h)))))
         (values real? real?)]{
Returns the lower and upper range limits for the @racket[i]@superscript{th} bin of the histogram @racket[h].  The lower and upper range limits are returned as multiple values.}

@subsection{Histogram Statistics}

@defproc[(histogram-max (h histogram?)) (>=/c 0.0)]{
Returns the maximum bin value for the histogram @racket[h].  Since in this implementation bin values are non-negative, the maximum value is also non-negative.}

@defproc[(histogram-min (h histogram?)) (>=/c 0.0)]{
Returns the minimum bin value for the histogram @racket[h].  Since in this implementation bin values are non-negative, the minimum value is also non-negative.}

@defproc[(histogram-sum (h histogram?)) (>=/c 0.0)]{
Returns the sum of the data in the histogram @racket[h].  Since in this implementation bin values are non-negative, the sum is also non-negative.}

@defproc[(histogram-mean (h histogram?)) (>=/c 0.0)]{
Returns the mean of the data in the histogram @racket[h].  Since in this implementation bin values are non-negative, the mean is also non-negative.}

@defproc[(histogram-sigma (h histogram?)) (>=/c 0.0)]{
Returns the standard deviation of the data in the histogram @racket[h].}

@subsection{Histogram Graphics}

The histogram graphics functions are defined in the file @filepath{histogram-graphics.rkt} in the Science Collection and are made available using the following form:

@defmodule[(planet williams/science/histogram-graphics)]

@defproc[(histogram-plot (h histogram?) (title string? "Histogram")) any]{
This function returns a plot of the histogram @racket[h] with the specified @racket[title].  If @racket[title] is not specified, @racket["Histogram"] is used.  The plot is scaled to the maximum bin value.  The plot is produced by the histogram plotting extension to the plot collection provided with Racket.}

@defproc[(histogram-plot-scaled (h histogram?) (title string? "Histogram")) any]{
This function returns a plot of the histogram @racket[h] with the specified @racket[title].  If @racket[title] is not specified, @racket["Histogram"] is used.  The plot is scaled to the sum of the bin  values.  It is most useful for a small number of bins---generally, twn or less.  The plot is produced by the histogram plotting extension to the plot collection provided with Racket.}

@subsection{Histogram Examples}

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

@bold{Example:} Scaled histogram of random variates from the exponential distribution with mean 1.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/exponential)
         (planet williams/science/histogram-with-graphics))

(let ((h (make-histogram-with-ranges-uniform 10 0.0 8.0)))
  (for ((i (in-range 10000)))
    (histogram-increment! h (random-exponential 1.0)))
  (histogram-plot-scaled h "Histogram of the Exponential Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/histogram-plot-scaled.png"]

@;----------
@; 2D Histograms
@;----------

@section{2D Histograms}

The 2D histogram functions described in this section are defined in the @filepath{histogram-2d.rkt} file in the Science Collection and are made available using the form:

@defmodule[(planet williams/science/histogram-2d)]

@defproc[(histogram-2d? (x any/c)) boolean?]{
Returns true, @racket[#t], if @racket[x] is a 2D histogram and false, @racket[#f], otherwise.}

@subsection{Creating 2D Histograms}

@defproc[(make-histogram-2d (nx exact-positive-integer?)
                            (ny exact-positive-integer?))
         histogram-2d?]{
Returns a new, empty 2D histogram with @racket[nx] bins in the x direction and @racket[ny] bins in the y direction and @math{nx + 1} range entries in the x direction and @math{ny + 1} range entries in the y direction.  The range entries must be set with a subsequent call to @racket[set-histogram-2d-ranges!] or @racket[set-histogram-2d-ranges-uniform!].}

@defproc[(make-histogram-2d-with-ranges-uniform (nx exact-positive-integer?)
                                                (xy exact-positive-integer?)
                                                (x-min real?)
                                                (x-max (>/c x-min))
                                                (y-min real?)
                                                (y-max (>/c y-min)))
         histogram-2d?]{
Returns a new, empty 2D histogram with @racket[nx] bins in the x direction and @racket[ny] bins in the y direction.  The @math{nx + 1} range entries in the x direction are initialized to provide @racket[nx] uniform width bins from @racket[x-min] to @racket[x-max].  The @math{ny + 1} range entries in the y direction are initialized to provide @racket[ny] uniform width bins from @racket[y-min] to @racket[y-max].}

@subsection{Updating and Accessing 2D Histogram Elements}

@defproc[(histogram-2d-nx (h histogram-2d?)) exact-positive-integer?]{
Returns the number of bins in the x direction in the 2D histogram @racket[h].}

@defproc[(histogram-2d-ny (h histogram-2d?)) exact-positive-integer?]{
Returns the number of bins in the y direction in the 2D histogram @racket[h].}

@defproc[{histogram-2d-x-ranges (h histogram-2d?)} (vectorof real?)]{
Returns a vector of ranges in the x direction for the 2D histogram @racket[h].  The length of the vector is equal to the number of bins in the x direction in @racket[h] plus one.}

@defproc[{histogram-2d-y-ranges (h histogram-2d?)} (vectorof real?)]{
Returns a vector of ranges in the y direction for the 2D histogram @racket[h].  The length of the vector is equal to the number of bins in the y direction in @racket[h] plus one.}

@defproc[(set-histogram-2d-ranges! (h histogram?)
                                   (x-ranges (and/c (vectorof real?)
                                                    (lambda (x)
                                                      (= (vector-length x-ranges)
                                                         (+ (histogram-2d-nx h) 1)))))
                                   (y-ranges (and/c (vectorof real?)
                                                    (lambda (x)
                                                      (= (vector-length y-ranges)
                                                         (+ (histogram-2d-ny h) 1))))))
        void?]{
Sets the ranges for the 2D histogram @racket[h] according to the given @racket[x-ranges] and @racket[y-ranges].  The length of the @racket[x-ranges] vector must equal the number of bins in the x direction in @racket[h] plus one.  The length of the @racket[y-ranges] vector must equal the number of bins in the y direction in @racket[h] plus one.  The bins in @racket[h] are also reset.}
              
@defproc[(set-histogram-2d-ranges-uniform! (h histogram?)
                                           (x-min real?)
                                           (x-max (>/c x-min))
                                           (y-min real?)
                                           (y-max (>/c y-min)))
         void?]{
Sets the ranges for the 2D histogram @racket[h] uniformly from @racket[x-min] to @racket[x-max] in the x direction and uniformly from @racket[y-min] to @racket[y-max] in the y direction.  The bins in @racket[h] are also reset.}

@defproc[(histogram-2d-bins (h histogram?)) (vectorof real?)]{
Returns the vector of bins for the 2D histogram @racket[h].  The length of the vector is @math{nx * ny}, where @math{nx} is the number of bins in the x direction in @racket[h] and @math{ny} is the number of bins in the y direction in @racket[h].  The @math{(i, j)}@superscript{th} index is computed as @math{(i * ny) + j}.}

@defproc*[(((histogram-2d-increment! (h histogram?) (x real?) (y real?)) void?)
           ((unchecked-histogram-2d-increment! (h histogram?) (x real?) (y real?)) void?))]{
Increments the bin in the 2D histogram @racket[h] containing (@racket[x], @racket[y]).  The bin value is incremented by one.}

@defproc*[(((histogram-2d-accumulate! (h histogram?) (x real?) (y real?) (weight (>=/c 0.0))) void?)
           ((unchecked-histogram-2d-accumulate! (h histogram?) (x real?) (y real?) (weight (>=/c 0.0))) void?))]{
Increments the bin in the 2D histogram @racket[h] containing (@racket[x], @racket[y]) by the specified @racket[weight].  Since in this implementation bin values are non-megative, the @racket[weight] must be non-negative.}

@defproc[(histogram-2d-get (h histogram?)
                           (i (and/c exact-nonnegative-integer?
                                     (</c (histogram-nx h))))
                           (j (and/c exact-nonnegative-integer?
                                     (</c (histogram-ny h)))))
         (>=/c 0.0)]{
Returns the contents of the (@racket[i], @racket[j])@superscript{th} bin of the histogram @racket[h].}

@defproc[(histogram-2d-get-x-range (h histogram?)
                                   (i (and/c exact-non-negative-integer?
                                             (</c (histogram-nx h))))
                                   (j (and/c exact-non-negative-integer?
                                             (</c (histogram-ny h)))))
         (values real? real?)]{
Returns the lower and upper range limits in the x direction for the (@racket[i], @racket[j])@superscript{th} bin of the histogram @racket[h].  The lower and upper range limits are returned as multiple values.}

@defproc[(histogram-2d-get-y-range (h histogram?)
                                   (i (and/c exact-non-negative-integer?
                                             (</c (histogram-nx h))))
                                   (j (and/c exact-non-negative-integer?
                                             (</c (histogram-ny h)))))
         (values real? real?)]{
Returns the lower and upper range limits in the y direction for the (@racket[i], @racket[j])@superscript{th} bin of the histogram @racket[h].  The lower and upper range limits are returned as multiple values.}

@subsection{2D Histogram Statistics}

@defproc[(histogram-2d-max (h histogram-2d?)) (>=/c 0.0)]{
Returns the maximum bin value for the 2D histogram @racket[h].  Since in this implementation bin values are non-negative, the maximum value is also non-negative.}

@defproc[(histogram-2d-min (h histogram-2d?)) (>=/c 0.0)]{
Returns the minimum bin value for the histogram @racket[h].  Since in this implementation bin values are non-negative, the minimum value is also non-negative.}

@defproc[(histogram-2d-sum (h histogram-2d?)) (>=/c 0.0)]{
Returns the sum of the data in the 2D histogram @racket[h].  Since in this implementation bin values are non-negative, the sum is also non-negative.}

@defproc[(histogram-2d-x-mean (h histogram-2d?)) (>=/c 0.0)]{
Returns the mean of the data in the x direction in the 2D histogram @racket[h].  Since in this implementation bin values are non-negative, the mean is also non-negative.}

@defproc[(histogram-2d-y-mean (h histogram-2d?)) (>=/c 0.0)]{
Returns the mean of the data in the y direction in the 2D histogram @racket[h].  Since in this implementation bin values are non-negative, the mean is also non-negative.}

@defproc[(histogram-2d-x-sigma (h histogram-2d?)) (>=/c 0.0)]{
Returns the standard deviation of the data in the x direction in the 2D histogram @racket[h].}

@defproc[(histogram-2d-y-sigma (h histogram-2d?)) (>=/c 0.0)]{
Returns the standard deviation of the data in the y direction in the 2D histogram @racket[h].}

@defproc[(histogram-2d-covariance (h histogram-2d?)) (>=/c 0.0)]{
Returns the covariance of the data in the 2D histogram @racket[h].}

@subsection{2D Histogram Graphics}

The 2D histogram graphics functions are defined in the file @filepath{histogram-2d-graphics.rkt} in the Science Collection and are made available using the following form:

@defmodule[(planet williams/science/histogram-2d-graphics)]

@defproc[(histogram-2d-plot (h histogram-2d?) (title string? "Histogram")) any]{
This function returns a plot of the 2D histogram @racket[h] with the specified @racket[title].  If @racket[title] is not specified, @racket["Histogram"] is used.  The plot is scaled to the maximum bin value.  The plot is produced by the histogram plotting extension to the plot collection provided with Racket.}

@subsection{2D Histogram Examples}

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

@;----------
@; Discrete Histograms
@;----------

@section{Discrete Histograms}

The discrete histogram functions described in this section are defined in the @filepath{discrete-histogram.rkt} file in the Science Collection and are made available using the form:

@defmodule[(planet williams/science/discrete-histogram)]

@defproc[(discrete-histogram? (x any/c)) boolean?]{
Returns true, @racket[#t], if @racket[x] is a discrete histogram and false, @racket[#f], otherwise.}

@subsection{Creating Discrete Histograms}

@defproc*[(((make-discrete-histogram (n1 integer?)
                                     (n2 (and/c integer? (>=/c n1)))
                                     (dynamic? boolean #t))
            discrete-histogram?)
           ((make-discrete-histogram) discrete-histogram?))]{
Returns a new, empty discrete histogram with range @racket[n1] to @racket[n2].  If @racket[dynamic?] is #t or @racket[make-discrete-histogram] is called with no arguments, the resulting discrete histogram will grow dynamically to accomodate subsequent data points.}

@subsection{Updating and Accessing Discrete Histogram Elements}

@defproc[(discrete-histogram-n1 (h discrete-histogram?)) integer?]{
Returns the lower range of the discrete histogram @racket[h].}

@defproc[(discrete-histogram-n2 (h discrete-histogram?)) integer?]{
Returns the upper range of the discrete histogram @racket[h].}

@defproc[(discrete-histogram-dynamic? (h discrete-histogram?)) boolean?]{
Returns true, @racket[#t], if the discrete histogram @racket[h] is dynamic and false, @racket[#f], otherwise.}

@defproc[(discrete-histogram-bins (h discrete-histogram?)) (vectorof real?)]{
Returns the vector of bins for the discrete histogram @racket[h].}

@defproc*[(((discrete-histogram-increment! (h discrete-histogram?) (i integer?)) void?)
           ((unchecked-discrete-histogram-increment! (h discrete-histogram?) (i integer?)) void?))]{
Increments the bin in the discrete histogram @racket[h] containing @racket[i].  The bin value is incremented by one.}

@defproc*[(((discrete-histogram-accumulate! (h discrete-histogram?) (i integer?) (weight (>=/c 0.0))) void?)
           ((unchecked-discrete-histogram-accumulate! (h discrete-histogram?) (i integer?) (weight (>=/c 0.0))) void?))]{
Increments the bin in the discrete histogram @racket[h] containing @racket[i] by the specified @racket[weight].  Since in this implementation bin values are non-megative, the @racket[weight] must be non-negative.}

@defproc[(discrete-histogram-get (h discrete-histogram?)
                                 (i (and/c integer?
                                           (>=/c (discrete-histogram-n1 h))
                                           (<=/c (discrete-histogram-n2 h)))))
         (>=/c 0.0)]{
Returns the contents of the bin of the discrete histogram @racket[h] containing @racket[i].}

@subsection{Discrete Histogram Statistics}

@defproc[(discrete-histogram-max (h discrete-histogram?)) (>=/c 0.0)]{
Returns the maximum bin value for the discrete histogram @racket[h].  Since in this implementation bin values are non-negative, the maximum value is also non-negative.}

@defproc[(discrete-histogram-min (h discrete-histogram?)) (>=/c 0.0)]{
Returns the minimum bin value for the discrete histogram @racket[h].  Since in this implementation bin values are non-negative, the minimum value is also non-negative.}

@defproc[(discrete-histogram-sum (h discrete-histogram?)) (>=/c 0.0)]{
Returns the sum of the data in the discrete histogram @racket[h].  Since in this implementation bin values are non-negative, the sum is also non-negative.}

@defproc[(discrete-histogram-mean (h discrete-histogram?)) (>=/c 0.0)]{
Returns the mean of the data in the discrete histogram @racket[h].  Since in this implementation bin values are non-negative, the mean is also non-negative.}

@defproc[(discrete-histogram-sigma (h discrete-histogram?)) (>=/c 0.0)]{
Returns the standard deviation of the data in the discrete histogram @racket[h].}

@subsection{Discrete Histogram Graphics}

The discrete histogram graphics functions are defined in the file @filepath{discrete-histogram-graphics.rkt} in the Science Collection and are made available using the following form:

@defmodule[(planet williams/science/discrete-histogram-graphics)]

@defproc[(discrete-histogram-plot (h discrete-histogram?) (title string? "Histogram")) any]{
This function returns a plot of the discrete histogram @racket[h] with the specified @racket[title].  If @racket[title] is not specified, @racket["Histogram"] is used.  The plot is scaled to the maximum bin value.  The plot is produced by the histogram plotting extension to the plot collection provided with Racket.}

@defproc[(discrete-histogram-plot-scaled (h discrete-histogram?) (title string? "Histogram")) any]{
This function returns a plot of the discrete histogram @racket[h] with the specified @racket[title].  If @racket[title] is not specified, @racket["Histogram"] is used.  The plot is scaled to the sum of the bin  values.  It is most useful for a small number of bins---generally, twn or less.  The plot is produced by the histogram plotting extension to the plot collection provided with Racket.}

@subsection{Discrete Histogram Examples}

@bold{Example:} Discrete histogram of random variates from the Poisson distribution with mean 10.0.

@racketmod[
racket
(require (planet williams/science/random-distributions/poisson)
         (planet williams/science/discrete-histogram-with-graphics))

(let ((h (make-discrete-histogram)))
  (for ((i (in-range 10000)))
    (discrete-histogram-increment! h (random-poisson 10.0)))
  (histogram-plot h "Histogram of the Poisson Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/poisson-hist.png"]

@bold{Example:} Scaled discrete histogram of random variates from the logarithmic distribution with probability 0.5.

@racketmod[
racket
(require (planet williams/science/random-distributions/logarithmic)
         (planet williams/science/discrete-histogram-with-graphics))
 
(let ((h (make-discrete-histogram)))
  (for ((i (in-range 10000)))
    (discrete-histogram-increment! h (random-logarithmic 0.5)))
  (discrete-histogram-plot-scaled
   h "Histogram of the Logarithmic Distribution"))]

The following figure shows the resulting histogram:

@image["scribblings/images/discrete-histogram-plot-scaled.png"]
