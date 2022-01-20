#lang scribble/doc
@(require scribble/manual
          scribblings/icons)

@title[#:tag "introduction"]{Introduction}

@local-table-of-contents[]

The Science Collection is a collection of modules that provide functions for numerical computing.  The structure of the Science Collection and many of the underlying algorithms were inspired by the @link["http://www.gnu.org/software/gsl/"]{GNU Scientific Library (GSL)}.  The functions are written entirely in @link["http://www.racket-lang.org"]{Racket} and present a true Racket look-and-feel throughout.  The source code is distributed with the Science Collection and licensed under the GNU Lesser General Public License, Version 3.0 @cite["LGPL"].

The motivation behind the Science Collection is to provide a numerical framework for knowledge-based simulation in Racket @cite["Williams90"] @cite["Williams96"].  Indeed, many of the routines were originally developed as part of the Simulation Collection.  It was noted that much of the functionality (e.g., random number generation, random distributions, histograms, and statistics) did not depend on, or could be separated from, the underlying simulation engine.  When this was done, it was further noted that this functionality represented a subset of the functionality available in the GSL.  At that point, it was decided to use the structure and, to the extent practical, the algorithms of the GSL as a reference model.  Thus, the Science Collection was born.

This Reference Manual was inspired by the @link["http://www.gnu.org/software/gsl/manual/html_node/"]{GNU Scientific Library Reference Manual - Revised Second Edition (v1.8)} @cite["GSL-RM"].  Because of the differences between C and Racket, and the fundamental differences between the underlying numeric models of each, the presentation of the functions is different here than in the GSL Reference Manual.  We also rely more on the graphical representation of results using the plot collection @link["http://docs.racket-lang.org/plot/index.html"]{(PLoT)} distributed with Racket.

We are indebted to Dr. M. Galassi and Dr. J. Theiler of Los Alamos National Laboratory and the others who have contributed to the development of the @link["http://www.gnu.org/software/gsl/"]{GNU Scientific Library (GSL)}.  Any weaknesses in the Science Collection are our own and must not be construed as having origins in the GSL.

@section{Routines Available in the Science Collection}

@(margin-note finger "This is only a fraction of the areas supported by the GSL.  For a complete reference of the numerical computing areas supported by the GSL, please refer to the " @link["http://www.gnu.org/software/gsl/manual/html_node/"]{GNU Scientific Library Reference Manual - Revised Second Edition (v1.8)} " " @cite["GSL-RM"] ".")

The Science Collection covers a range of topics in numerical computing.  Functions are available for the following areas:

@itemize{
  
  @item{Mathematical Constants and Functions}
  @item{Physical Constants}
  @item{Special Functions}
  @item{Random Numbers}
  @item{Random Distributions}
  @item{Statistics}
  @item{Histograms}
  @item{Ordinary Differential Equations}
  @item{Chebyshev Approximations}
  @item{Fast Fourier Transforms}
  
  }

The use of these functions is described in this manual.  Each chapter provides detailed definitions of the functions, with example code.

@section{The Science Collection is Free Software}

The Science Collection is free software---this means that anyone is free to use it and redistribute it in other free programs.  The Science Collection is not in the public domain---it is copyrighted and there are conditions on its distribution.  Specifically, the Science Collection is distributed under the GNU Lesser General Public License, Version 3.0.  A copy of the LGPL is provided with the software.

@section{Obtaining the Science Collection}

The preferred method for obtaining the Science Collection is via @link["http://planet.racket-lang.org/"]{PLaneT}, Racket's centeralized package distribution system.  The PLaneT identifier for the Science Collection is @racket[(planet williams/science/science)] or, if using any of the graphical features, @racket[(planet williams/science/science-with-graphics)]---either will download the entire Science Collection.  See Chapter 2 for an example.

The source code is maintained on the @link["http://schematics.sourceforge.net"]{Schematics} project website and Subversion repository at @link["http://sourceforge.net"]{SourceForge}.

Note that Version 4.0 and higher of the Science Collection requires Racket Version 5 or higher.

@section{No Warranty}

The Science Collection is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  It is your responsibility to validate the behavior of the software and their accuracy using the source code provided.  See the GNU Lesser General Public License, Version 3.0 @cite["LGPL"] for more details.
