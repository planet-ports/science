#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribblings/icons
          (for-label racket
                     (planet williams/science/science-with-graphics)))

@title[#:tag "fft"]{Fast Fourier Transforms}

@local-table-of-contents[]

This chapter describes the functions for performing Fast Fourier Transforms (FFTs) provided by the Science Collection. This includes radix-2 functions (for lengths that are a power of two) and mixed-radix routines (for any length).

Fast Fourier Transforms are efficient algorithms for calculating the discrete Fourier transform (DFT)

@image["scribblings/images/dft-equation.png"].

The naive evaluation of the discrete Fourier transform is a matrix-vector multiplication @image["scribblings/images/matrix-vector-multiplication-equation.png"]. A general matrix-vector multiplication takes @math{O(n^2)} operations for @math{n} data points. Fast Fourier Transform algorithms use a divide-and-conquer strategy to factor the matrix @math{W} into smaller submatrices corresponding to the integer factors of the length, @math{n}. If @math{n} can be factored into a product of integers, @math{f_1}, @math{f_2}, ..., @math{f_m}, then the DFT can be computed in @math{O(n/Σ(f_i))} operations. For a radix-2 FFT, this gives an operation count of @math{O(n/log_2 n)}.

All of the FFT functions offer three types of transform: forward, inverse, and backwards, based on the same mathematical definitions. The definition of the @italic{forward Fourier transform}, @math{x = FFT(x)}, is

@image["scribblings/images/fft-equation.png"],

and the definition of the @italic{inverse Fourier transform}, @math{x=IFFT(z)}, is

@image["scribblings/images/ifft-equation.png"].

The factor of @math{1/n} makes this a true inverse. For example, a call to @racket[fft-complex-forward] followed by a call to @racket[fft-complex-inverse] should return the original data (within numeric errors).

In general, there are two possible choices for the sign of the exponential in the transform / inverse transform pair. The Science Collection follows the same convential as the GSL (which follows the same convention as FFTPACK) of using a negative exponential for the forward transform. The advantage of this convention is that the inverse transform recreates the original function with simple Fourier synthesis.

The @italic{backwards FFT} is simply an unscaled version of the inverse FFT,

@image["scribblings/images/backwards-fft-equation.png"].

When the overall scale of the result is unimportant, it is often convenient to use the backwards FFT instead of the inverse to save unnecessary divisions.

For the transform versions of the functions, the @racket[sign] argument can be either @racket[fft-forward] (-1.0) or @racket[fft-backward] (1.0).

@defidform[fft-forward]{Specifies a forward transform.}
@defidform[fft-backward]{Specifies a backwards transform.}

The functions described in this chapter are defined in the @filepath{fft.rkt} file in the Science Collection and are made available ising the form:

@defmodule[(planet williams/science/fft)]

@section{Radix-2 FFT Routines}

The radix-2 algorithms described in this section are simple and compact, although not necessarily the most efficient. They use the Cooley-Tukey algorithm to compute in-place complex FFTs for lengths that are a power of 2 --- no additional storage is required. The corresponding mixed-radix routines offer better performance at the expense of requiring additional working space.

@defproc*[(((fft-complex-radix2-forward (data (vectorof complex?))) void?)
           ((fft-complex-radix2-backward (data (vectorof complex?))) void?)
           ((fft-complex-radix2-inverse (data (vectorof complex?))) void?)
           ((fft-complex-radix2-transform (data (vectorof complex?)) (sign (one-of/c -1.0 1.0))) void?)
           ((unchecked-fft-complex-radix2-forward (data (vectorof complex?))) void?)
           ((unchecked-fft-complex-radix2-backward (data (vectorof complex?))) void?)
           ((unchecked-fft-complex-radix2-inverse (data (vectorof complex?))) void?)
           ((unchecked-fft-complex-radix2-transform (data (vectorof complex?)) (sign (one-of/c -1.0 1.0))) void?))]{
These functions compute forward, backward, and inverse FFTs of @racket[data], whose length must be a power of two, using an in-place radix-2 decimation-in-time algorithm.}

@defproc*[(((fft-complex-radix2-dif-forward (data (vectorof complex?))) void?)
           ((fft-complex-radix2-dif-backward (data (vectorof complex?))) void?)
           ((fft-complex-radix2-dif-inverse (data (vectorof complex?))) void?)
           ((fft-complex-radix2-dif-transform (data (vectorof complex?)) (sign (one-of/c -1.0 1.0))) void?)
           ((unchecked-fft-complex-radix2-dif-forward (data (vectorof complex?))) void?)
           ((unchecked-fft-complex-radix2-dif-backward (data (vectorof complex?))) void?)
           ((unchecked-fft-complex-radix2-dif-inverse (data (vectorof complex?))) void?)
           ((unchecked-fft-complex-radix2-dif-transform (data (vectorof complex?)) (sign (one-of/c -1.0 1.0))) void?))]{
These are decimation-in-frequency versions of the radix-2 FFT functions.}

@section{Mixed-Radix FFT Routines}

The mixed-radius algorithm, which work for FFTs of any length, is based on sub-transform modules --- highly optimized small length FFTs that are combined to create larger FFTs. There are efficient modules for factors of 2, 3, 4, 5, 6, and 7. The modules for the composite factors of 4 and 6 are faster than combining the modules for 2*2 and 2*3.

For factors that are not implemented as modules, there is a fall-back to a general length-n module that uses Singleton's method for efficiently computing a DFT. This module is @math{O(N^2)} and is slower than a dedicated module would be, but works for any length @math{n}. Of course, lengths that use the general length-n module will still be factored as much as possible. For example, a length of 143 will be factored into 11*13. Large prime factors are the worst case scenario, e.g. as found in n = 2*3*99991 = 599946, and should be avoided because their @math{O(n^2)} scaling will dominate the run-time.

The mixed radix algorithms require additional working space to hold the intermediate steps of the transform.

@defproc[(fft-complex-workspace? (x any/c)) boolean?]{
Returns true, @racket[#t], if @racket[x] is a complex FFT workspace.}

@defproc[(make-fft-complex-workspace (n exact-positive-integer?)) fft-complex-workspace?]{
Returns a workspace for a complex transform of length @racket[n].}

The following functions compute the transform.

@defproc*[(((fft-complex-forward
             (data (vectorof complex?))
             (#:workspace work fft-complex-workspace? (make-fft-complex-workspace (vector-length data))))
            void?)
           ((fft-complex-backward
             (data (vectorof complex?))
             (#:workspace work fft-complex-workspace? (make-fft-complex-workspace (vector-length data))))
            void?)
           ((fft-complex-inverse
             (data (vectorof complex?))
             (#:workspace work fft-complex-workspace? (make-fft-complex-workspace (vector-length data))))
            void?)
           ((fft-complex-transform
             (data (vectorof complex?))
             (sign (one-of/c -1.0 1.0))
             (#:workspace work fft-complex-workspace? (make-fft-complex-workspace (vector-length data))))
            void?)
           ((unchecked-fft-complex-forward
             (data (vectorof complex?))
             (#:workspace work fft-complex-workspace? (make-fft-complex-workspace (vector-length data))))
            void?)
           ((unchecked-fft-complex-backward
             (data (vectorof complex?))
             (#:workspace work fft-complex-workspace? (make-fft-complex-workspace (vector-length data))))
            void?)
           ((unchecked-fft-complex-inverse
             (data (vectorof complex?))
             (#:workspace work fft-complex-workspace? (make-fft-complex-workspace (vector-length data))))
            void?)
           ((unchecked-fft-complex-transform
             (data (vectorof complex?))
             (sign (one-of/c -1.0 1.0))
             (#:workspace work fft-complex-workspace? (make-fft-complex-workspace (vector-length data))))
            void?))]{
These functions compute forward, backward, and inverse FFTs of @racket[data] using a mixed radix decimation-in-frequency algorithm. Efficient modules are provided for subtransforms of length 2, 3, 4, 5, 6, and 7. Any remaining factors are computed using a slow, @math{O(n^2)} general-n module. The caller may supply a workspace, @racket[work]. If @racket[work] is not provided, an appropriate workspace is created and used.}

@section{FFT Examples}

@subsection{Radix 2 FFT Example}

The following program demonstrates the use of the Radix 2 FFT functions.

The program generates a data vector of length @math{2^12}=4096 data points that is a sampled sine wave with a period of 500 samples. We perform an in-place forward FFT (using @racket[fft-complex-radix2-forward]) and plot the magnitudes of the resulting transform. We then perform an in-place inverse FFT (using @racket[fft-complex-radix2-inverse]) and plot the real-parts of the resulting transform. The final plot is equal to the original data plot (within numeric errors).

Note that most of the code is for building the data vector and plotting results. The calculations of the forward and inverse FFTs are just the two calls noted above.

@racketmod[
racket
(require plot
         (planet williams/science/math)
         (planet williams/science/statistics)
         (planet williams/science/fft))

(printf "Radix 2 Complex FFT - Decimation in Time~n")

(code:comment "Data")
(define data (build-vector
              4096
              (lambda (t)
                (+ (sin (* t (/ 2*pi 500))) 0.0))))
(define data-points (for/list ((j (in-vector data))
                               (i (in-naturals)))
                      (vector i j)))
(plot (points data-points)
      #:title "Data"
      #:x-min 0
      #:x-max (length data-points)
      #:y-min -1.0
      #:y-max 1.0)

(code:comment "Radix 2 Forward FFT")
(fft-complex-radix2-forward data)
(define data-max (for/fold ((max-magnitude 0.0))
                            ((x (in-vector data)))
                    (max (magnitude x) max-magnitude)))
(define data-points-forward (for/list ((j (in-vector data))
                                        (i (in-naturals)))
                               (vector i (magnitude j))))
(plot (points data-points-forward)
      #:title "Radix 2 Complex FFT (Forward)"
      #:x-min 0
      #:x-max (length data-points-forward)
      #:y-max data-max)

(code:comment "Radix 2 Inverse FFT")
(fft-complex-radix2-inverse data)
(define data-points-inverse (for/list ((j (in-vector data))
                                        (i (in-naturals)))
                               (vector i (real-part j))))
(plot (points data-points-inverse)
      #:title "Radix 2 Complex FFT (Inverse)"
      #:x-min 0
      #:x-max (length data-points-inverse)
      #:y-min -1.0
      #:y-max 1.0)
]

Here is the output from the program.

@verbatim{
Radix 2 Complex FFT - Decimation in Time
}

@image["scribblings/images/radix-2-complex-fft-data.png"]

@image["scribblings/images/radix-2-complex-fft-forward.png"]

@image["scribblings/images/radix-2-complex-fft-inverse.png"]

@subsection{Mixed-Radix FFT Example}

The following program demonstrates the use of the Radix 2 FFT functions.

The program generates a data vector of length 5000 data points that is a sampled sine wave with a period of 500 samples. We perform an in-place forward FFT (using @racket[fft-complex-forward]) and plot the magnitudes of the resulting transform. We then perform an in-place inverse FFT (using @racket[fft-complex-inverse]) and plot the real-parts of the resulting transform. The final plot is equal to the original data plot (within numeric errors).

The prime factors of 5000 are 2*2*2*5*5*5*5. This results in subtransforms of lengths 2, 4, 5, 5, 5, and 5 being computed.

Note that most of the code is for building the data vector and plotting results. The calculations of the forward and inverse FFTs are just the two calls noted above.

@racketmod[
racket

(require plot
         (planet williams/science/math)
         (planet williams/science/statistics)
         (planet williams/science/fft))

(printf "Multi-Radix Complex FFT~n")

(code:comment "Data")
(define data (build-vector
              5000
              (lambda (t)
                (+ (sin (* t (/ 2*pi 500))) 0.0))))
(define data-points (for/list ((j (in-vector data))
                               (i (in-naturals)))
                      (vector i j)))
(plot (points data-points)
      #:title "Data"
      #:x-min 0
      #:x-max (length data-points)
      #:y-min -1.0
      #:y-max 1.0)

(code:comment "Multi-Radix Forward FFT")
(fft-complex-forward data)
(define data-max (for/fold ((max-magnitude 0.0))
                            ((x (in-vector data)))
                    (max (magnitude x) max-magnitude)))
(define data-points-forward (for/list ((j (in-vector data))
                                        (i (in-naturals)))
                               (vector i (magnitude j))))
(plot (points data-points-forward)
      #:title "Complex FFT (Forward)"
      #:x-min 0
      #:x-max (length data-points-forward)
      #:y-max data-max)

(code:comment "Multi-Radix Inverse FFT")
(fft-complex-inverse data)
(define data-points-inverse (for/list ((j (in-vector data))
                                        (i (in-naturals)))
                               (vector i (real-part j))))
(plot (points data-points-inverse)
      #:title "Complex FFT (Inverse)"
      #:x-min 0
      #:x-max (length data-points-inverse)
      #:y-min -1.0
      #:y-max 1.0)
]

Here is the output from the program.

@verbatim{
Multi-Radix Complex FFT
}

@image["scribblings/images/multi-radix-fft-data.png"]

@image["scribblings/images/multi-radix-fft-forward.png"]

@image["scribblings/images/multi-radix-fft-inverse.png"]
