#lang scribble/doc
@(require scribble/manual
          scribble/struct)

@title[#:style '(toc) #:tag "science"]{@bold{Science Collection:} Reference Manual}

Science Collection
@(make-element 'newline '())
Reference Manual
@(make-element 'newline '())
Edition 4.2

October 2011

M. Douglas Williams
@(make-element 'newline '())
@link["mailto:m.douglas.williams@gmail.com"]{doug@tt|{@}|cognidrome.org}

The Science Collection is a collection of modules that provide functions for numerical computing. The structure of the science collection and many of the underlying algorithms were inspired by the @link["http://www.gnu.org/software/gsl/"]{GNU Scientific Library (GSL)}. The functions are written entirely in @link["http://www.racket-lang.org/"]{Racket} and presents a true Racket look-and-feel throughout.  The source code is distributed with the science collection and licensed under the GNU Lesser General Public License, Version 3 @cite["LGPL"].

The structure of this document and certain sections were inspired by the @link["http://www.gnu.org/software/gsl/manual/html_node/"]{GNU Scientific Library -- Reference Manual} @cite["GSL-RM"].

© 2003-2011, M. Douglas Williams

Permission is granted to copy, distribute and/or modify this document under the terms of the GNU Free Documentation License, Version 1.3 or any later version published by the Free Software Foundation; with no Invariant Sections, with no Front-Cover Texts, and with no Back-Cover Texts.  A copy of the license is included in the section entitled "GNU Free Documentation License".

@local-table-of-contents[]

@include-section["introduction.scrbl"]
@include-section["using.scrbl"]
@include-section["error-handling.scrbl"]
@include-section["mathematical-functions.scrbl"]
@include-section["special-functions.scrbl"]
@include-section["random-numbers.scrbl"]
@include-section["random-distributions.scrbl"]
@include-section["statistics.scrbl"]
@include-section["histograms.scrbl"]
@include-section["ode.scrbl"]
@include-section["chebyshev.scrbl"]
@include-section["fft.scrbl"]
@include-section["physical-constants.scrbl"]
@include-section["unsafe-ops-utils.scrbl"]
@include-section["gfdl.scrbl"]

@(bibliography
  
  (bib-entry #:key "LGPL"
             #:title "GNU Lesser General Public License, Version 3"
             #:author "Free Software Foundation, Inc."
             #:date "29 June 2007"
             #:url "http://www.gnu.org/licenses/lgpl-3.0.htmll")

  (bib-entry #:key "FDL"
             #:title "GNU Free Documentation License, Version 1.3"
             #:author "Free Software Foundation, Inc."
             #:date "3 November 2008"
             #:url "http://www.gnu.org/licenses/fdl-1.3.html")
   
  (bib-entry #:key "Abramowitz64"
             #:title "Handbook of Mathematical Functions"
             #:is-book? #t
             #:author "M. Abramowitz and I. Stegun"
             #:date "June 1964"
             #:url "http://www.math.sfu.ca/~cbm/aands/")
  
  (bib-entry #:key "GSL-RM"
             #:title "GNU Scientific Library -- Reference Manual"
             #:is-book? #t
             #:author "M. Gagassi, et al"
             #:url "http://www.gnu.org/software/gsl/manual/html_node/")
  
  (bib-entry #:key "Knuth"
             #:title "Seminumerical Algorithms (3rd edition)"
             #:is-book? #t
             #:author "D.E. Knuth"
             #:location "Section 4.2.2")
  
  (bib-entry #:key "Williams90"
             #:title "Construction of Dynamic Stochastic Simulation Models Using Knowledge-Based Techniques"
             #:is-book? #t
             #:author "M.D. Williams"
             #:date "1990")
  
  (bib-entry #:key "Williams96"
             #:title "Design of a Knowledge-Based Simulation Environment"
             #:author "M.D. Williams, E.C. Gorman, and S.G. Shiva"
             #:location "Simulation 67:2, pp. 121-135"
             #:date "August 1996")

  )

@index-section[]
