#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribblings/icons
          (for-label racket
                     (planet williams/science/science-with-graphics)))

@title[#:tag "physical-constants"]{Physical Constants}

@local-table-of-contents[]

This chapter describes the physical constants, such as the speed of light, @math{c}, and gravitational constant, @math{G}, provided by the Science Collection. The values are available in different unit systems, including the standard MKSA system (meters, kilograms, secomds, amperes) and the CGSM system (centimeters, grams, seconds, gauss), which is commonly used in Astronomy.

The constants described in this chapter are defined in the @racketfont{constants} sub-collection of the Science Collection. All of the modules on the @racketfont{constants} sub-collection can be made available using the form:

@defmodule[(planet williams/science/constants)]

The individual modules in the @racketfont{constants} sub-collection can also be made available using any of the following forms:

@racket[(require (planet williams/science/constants/cgs-constants))]

@racket[(require (planet williams/science/constants/cgms-constants))]

@racket[(require (planet williams/science/constants/mksa-constants))]

@racket[(require (planet williams/science/constants/mks-constants))]

@racket[(require (planet williams/science/constants/num-constants))]

@section{Fundamental Constants}

@deftogether[(@defidform[cgs-speed-of-light]
              @defidform[cgsm-speed-of-light]
              @defidform[mksa-speed-of-light]
              @defidform[mks-speed-of-light])]{
The speed of light in vacuum, @math{c}.}

@deftogether[(@defidform[mksa-vacuum-permeability]
              @defidform[mks-vacuum-permeability])]{
The permeability of free space, @math{μ_0}.}

@deftogether[(@defidform[mksa-vacuum-permittivity]
              @defidform[mks-vacuum-permittivity])]{
The permittivity of free space, @math{ε_0}.}

@deftogether[(@defidform[cgs-plancks-constant-h]
              @defidform[cgsm-plancks-constant-h]
              @defidform[mksa-plancks-constant-h]
              @defidform[mks-plancks-constant-h])]{
Plancks's constant, @math{h}.}

@deftogether[(@defidform[cgs-plancks-constant-hbar]
              @defidform[cgsm-plancks-constant-hbar]
              @defidform[mksa-plancks-constant-hbar]
              @defidform[mks-plancks-constant-hbar])]{
Plancks's constant divided by @math{2π}, @math{ћ}.}
                                                     
@defidform[num-avogadro]{
Avogadro's number, @math{N_a}.}

@deftogether[(@defidform[cgsm-faraday]
              @defidform[mksa-faraday]
              @defidform[mks-faraday])]{
The molar charge of 1 Faraday.}

@deftogether[(@defidform[cgs-boltzmann]
              @defidform[cgsm-boltzmann]
              @defidform[mksa-boltzmann]
              @defidform[mks-boltzmann])]{
Boltzmann's constant, @math{k}.}

@deftogether[(@defidform[cgs-molar-gas]
              @defidform[cgsm-molar-gas]
              @defidform[mksa-molar-gas]
              @defidform[mks-molar-gas])]{
The molar gas constant, @math{R_0}.}

@deftogether[(@defidform[cgs-standard-gas-volume]
              @defidform[cgsm-standard-gas-volume]
              @defidform[mksa-standard-gas-volume]
              @defidform[mks-standard-gas-volume])]{
The standard gas volume, @math{V_0}.}

@deftogether[(@defidform[cgs-stefan-boltzmann-constant]
              @defidform[cgsm-stefan-boltzmann-constant]
              @defidform[mksa-stefan-boltzmann-constant]
              @defidform[mks-stefan-boltzmann-constant])]{
The Stefan-Boltzmann radiation constant, @math{σ}.}

@deftogether[(@defidform[mksa-gauss]
              @defidform[mks-gauss])]{
The magnetic field of 1 Gauss.}

@section{Astronomy and Astrophysics}

@deftogether[(@defidform[cgs-astronomical-unit]
              @defidform[cgsm-astronomical-unit]
              @defidform[mksa-astronomical-unit]
              @defidform[mks-astronomical-unit])]{
The length of 1 astronomical unit (mean earth-sun distance), @math{au}.}

@deftogether[(@defidform[cgs-gravitational-constant]
              @defidform[cgsm-gravitational-constant]
              @defidform[mksa-gravitational-constant]
              @defidform[mks-gravitational-constant])]{
The gravitational constant, @math{G}.}

@deftogether[(@defidform[cgs-light-year]
              @defidform[cgsm-light-year]
              @defidform[mksa-light-year]
              @defidform[mks-light-year])]{
The distance of 1 light-year, @math{ly}.}

@deftogether[(@defidform[cgs-parsec]
              @defidform[cgsm-parsec]
              @defidform[mksa-parsec]
              @defidform[mks-parsec])]{
The distance of 1 parsec, @math{pc}.}

@deftogether[(@defidform[cgs-grav-accel]
              @defidform[cgsm-grav-accel]
              @defidform[mksa-grav-accel]
              @defidform[mks-grav-accel])]{
The standard gravitational acceleration on Earth, @math{g}.}

@deftogether[(@defidform[cgs-solar-mass]
              @defidform[cgsm-solar-mass]
              @defidform[mksa-solar-mass]
              @defidform[mks-solar-mass])]{
The mass of the Sun.}

@section{Atomic and Nuclear Physics}

@deftogether[(@defidform[cgsm-electron-charge]
              @defidform[mksa-electron-charge]
              @defidform[mks-electron-charge])]{
The charge of the electron, @math{e}.}

@deftogether[(@defidform[cgs-electron-volt]
              @defidform[cgsm-electron-volt]
              @defidform[mksa-electron-volt]
              @defidform[mks-electron-volt])]{
The energy of 1 electron volt, @math{eV}.}

@deftogether[(@defidform[cgs-unified-atomic-mass]
              @defidform[cgsm-unified-atomic-mass]
              @defidform[mksa-unified-atomic-mass]
              @defidform[mks-unified-atomic-mass])]{
The unified atomic mass, @math{amu}.}

@deftogether[(@defidform[cgs-mass-electron]
              @defidform[cgsm-mass-electron]
              @defidform[mksa-mass-electron]
              @defidform[mks-mass-electron])]{
The mass of the electron, @math{m_e}.}

@deftogether[(@defidform[cgs-mass-muon]
              @defidform[cgsm-mass-muon]
              @defidform[mksa-mass-muon]
              @defidform[mks-mass-muon])]{
The mass of the muon, @math{m_μ}.}

@deftogether[(@defidform[cgs-mass-proton]
              @defidform[cgsm-mass-proton]
              @defidform[mksa-mass-proton]
              @defidform[mks-mass-proton])]{
The mass of the proton, @math{m_p}.}

@deftogether[(@defidform[cgs-mass-neutron]
              @defidform[cgsm-mass-neutron]
              @defidform[mksa-mass-neutron]
              @defidform[mks-mass-neutron])]{
The mass of the neutron, @math{m_n}.}

@defidform[num-fine-structure]{
The electromagnetic fine structure constant, @math{α}.}

@deftogether[(@defidform[cgs-rydberg]
              @defidform[cgsm-rydberg]
              @defidform[mksa-rydberg]
              @defidform[mks-rydberg])]{
The Rydberg constant, @math{Ry}, in units of energy. This is related to the Rydberg inverse wavelength @image["scribblings/images/R_infty.png"] by @image["scribblings/images/ryberg.png"].}

@deftogether[(@defidform[cgs-bohr-radius]
              @defidform[cgsm-bohr-radius]
              @defidform[mksa-bohr-radius]
              @defidform[mks-bohr-radius])]{
The Bohr radius, @math{a_0}.}

@deftogether[(@defidform[cgs-angstrom]
              @defidform[cgsm-angstrom]
              @defidform[mksa-angstrom]
              @defidform[mks-angstrom])]{
The length of 1 angstrom.}

@deftogether[(@defidform[cgs-barn]
              @defidform[cgsm-barn]
              @defidform[mksa-barn]
              @defidform[mks-barn])]{
The area of 1 barn.}

@deftogether[(@defidform[cgsm-bohr-magneton]
              @defidform[mksa-bohr-magneton]
              @defidform[mks-bohr-magneton])]{
The Bohr magneton, @math{μ_B}.}

@deftogether[(@defidform[cgsm-nuclear-magneton]
              @defidform[mksa-nuclear-magneton]
              @defidform[mks-nuclear-magneton])]{
The nuclear magneton, @math{μ_N}.}

@deftogether[(@defidform[cgsm-electron-magnetic-moment]
              @defidform[mksa-electron-magnetic-moment]
              @defidform[mks-electron-magnetic-moment])]{
The absolute value of the magnetic mement of the electron, @math{μ_e}. The physical magnetic moment of the electron is negative.}

@deftogether[(@defidform[cgsm-proton-magnetic-moment]
              @defidform[mksa-proton-magnetic-moment]
              @defidform[mks-proton-magnetic-moment])]{
The absolute value of the magnetic mement of the proton, @math{μ_p}.}

@deftogether[(@defidform[cgs-thomson-cross-section]
              @defidform[cgsm-thomson-cross-section]
              @defidform[mksa-thomson-cross-section]
              @defidform[mks-thomson-cross-section])]{
The Thomson cross section, @math{σ_T}.}

@deftogether[(@defidform[mksa-debye]
              @defidform[mks-debye])]{
The electric dipole moment of 1 Debye, @math{D}.}

@section{Measurements of Time}

@deftogether[(@defidform[cgs-minute]
              @defidform[cgsm-minute]
              @defidform[mksa-minute]
              @defidform[mks-minute])]{
The number of seconds in 1 minute.}

@deftogether[(@defidform[cgs-hour]
              @defidform[cgsm-hour]
              @defidform[mksa-hour]
              @defidform[mks-hour])]{
The number of seconds in 1 hour.}

@deftogether[(@defidform[cgs-day]
              @defidform[cgsm-day]
              @defidform[mksa-day]
              @defidform[mks-day])]{
The number of seconds in 1 day.}

@deftogether[(@defidform[cgs-week]
              @defidform[cgsm-week]
              @defidform[mksa-week]
              @defidform[mks-week])]{
The number of seconds in 1 week.}

@section{Imperial Units}

@deftogether[(@defidform[cgs-inch]
              @defidform[cgsm-inch]
              @defidform[mksa-inch]
              @defidform[mks-inch])]{
The length of 1 inch.}

@deftogether[(@defidform[cgs-foot]
              @defidform[cgsm-foot]
              @defidform[mksa-foot]
              @defidform[mks-foot])]{
The length of 1 foot.}

@deftogether[(@defidform[cgs-yard]
              @defidform[cgsm-yard]
              @defidform[mksa-yard]
              @defidform[mks-yard])]{
The length of 1 yard.}

@deftogether[(@defidform[cgs-mile]
              @defidform[cgsm-mile]
              @defidform[mksa-mile]
              @defidform[mks-mile])]{
The length of 1 mile.}

@deftogether[(@defidform[cgs-mil]
              @defidform[cgsm-mil]
              @defidform[mksa-mil]
              @defidform[mks-mil])]{
The length of 1 mil (1/1000 of an inch).}

@section{Speed and Nautical Units}

@deftogether[(@defidform[cgs-kilometers-per-hour]
              @defidform[cgsm-kilometers-per-hour]
              @defidform[mksa-kilometers-per-hour]
              @defidform[mks-kilometers-per-hour])]{
The speed of 1 kilometer per hour.}

@deftogether[(@defidform[cgs-miles-per-hour]
              @defidform[cgsm-miles-per-hour]
              @defidform[mksa-miles-per-hour]
              @defidform[mks-miles-per-hour])]{
The speed of 1 mile per hour.}

@deftogether[(@defidform[cgs-nautical-mile]
              @defidform[cgsm-nautical-mile]
              @defidform[mksa-nautical-mile]
              @defidform[mks-nautical-mile])]{
The length of 1 nautical mile.}

@deftogether[(@defidform[cgs-fathom]
              @defidform[cgsm-fathom]
              @defidform[mksa-fathom]
              @defidform[mks-fathom])]{
The length of 1 fathom.}

@deftogether[(@defidform[cgs-knot]
              @defidform[cgsm-knot]
              @defidform[mksa-knot]
              @defidform[mks-knot])]{
The speed of 1 knot.}

@section{Printers Units}

@deftogether[(@defidform[cgs-point]
              @defidform[cgsm-point]
              @defidform[mksa-point]
              @defidform[mks-point])]{
The length of 1 printer's point (1/72 inch).}

@deftogether[(@defidform[cgs-texpoint]
              @defidform[cgsm-texpoint]
              @defidform[mksa-texpoint]
              @defidform[mks-texpoint])]{
The length of 1 TeX point (1/72.27 inch).}

@section{Volume, Area and Length}

@deftogether[(@defidform[cgs-micron]
              @defidform[cgsm-micron]
              @defidform[mksa-micron]
              @defidform[mks-micron])]{
The length of 1 micron.}

@deftogether[(@defidform[cgs-hectare]
              @defidform[cgsm-hectare]
              @defidform[mksa-hectare]
              @defidform[mks-hectare])]{
The area of 1 hectare.}

@deftogether[(@defidform[cgs-acre]
              @defidform[cgsm-acre]
              @defidform[mksa-acre]
              @defidform[mks-acre])]{
The area of 1 acre.}

@deftogether[(@defidform[cgs-liter]
              @defidform[cgsm-liter]
              @defidform[mksa-liter]
              @defidform[mks-liter])]{
The volume of 1 liter.}

@deftogether[(@defidform[cgs-us-gallon]
              @defidform[cgsm-us-gallon]
              @defidform[mksa-us-gallon]
              @defidform[mks-us-gallon])]{
The volume of 1 US gallon.}

@deftogether[(@defidform[cgs-canadian-gallon]
              @defidform[cgsm-canadian-gallon]
              @defidform[mksa-canadian-gallon]
              @defidform[mks-canadian-gallon])]{
The volume of 1 Canadian gallon.}

@deftogether[(@defidform[cgs-uk-gallon]
              @defidform[cgsm-uk-gallon]
              @defidform[mksa-uk-gallon]
              @defidform[mks-uk-gallon])]{
The volume of 1 UK gallon.}

@deftogether[(@defidform[cgs-quart]
              @defidform[cgsm-quart]
              @defidform[mksa-quart]
              @defidform[mks-quart])]{
The volume of 1 quart.}

@deftogether[(@defidform[cgs-pint]
              @defidform[cgsm-pint]
              @defidform[mksa-pint]
              @defidform[mks-pint])]{
The volume of 1 pint.}

@section{Mass and Weight}

@deftogether[(@defidform[cgs-pound-mass]
              @defidform[cgsm-pound-mass]
              @defidform[mksa-pound-mass]
              @defidform[mks-pound-mass])]{
The mass of 1 pound.}

@deftogether[(@defidform[cgs-ounce-mass]
              @defidform[cgsm-ounce-mass]
              @defidform[mksa-ounce-mass]
              @defidform[mks-ounce-mass])]{
The mass of 1 ounce.}

@deftogether[(@defidform[cgs-ton]
              @defidform[cgsm-ton]
              @defidform[mksa-tons]
              @defidform[mks-ton])]{
The mass of 1 ton.}

@deftogether[(@defidform[cgs-metric-ton]
              @defidform[cgsm-metric-ton]
              @defidform[mksa-metric-ton]
              @defidform[mks-metric-ton])]{
The mass of 1 metric ton (1000 kg).}

@deftogether[(@defidform[cgs-uk-ton]
              @defidform[cgsm-uk-ton]
              @defidform[mksa-uk-ton]
              @defidform[mks-uk-ton])]{
The mass of 1 UK ton.}

@deftogether[(@defidform[cgs-troy-ounce]
              @defidform[cgsm-troy-ounce]
              @defidform[mksa-troy-ounce]
              @defidform[mks-troy-ounce])]{
The mass of 1 troy ounce.}

@deftogether[(@defidform[cgs-carat]
              @defidform[cgsm-carat]
              @defidform[mksa-carat]
              @defidform[mks-carat])]{
The mass of 1 carat.}

@deftogether[(@defidform[cgs-gram-force]
              @defidform[cgsm-gram-force]
              @defidform[mksa-gram-force]
              @defidform[mks-gram-force])]{
The force of 1 gram weight.}

@deftogether[(@defidform[cgs-pound-force]
              @defidform[cgsm-pound-force]
              @defidform[mksa-pound-force]
              @defidform[mks-pound-force])]{
The force of 1 pound weight.}

@deftogether[(@defidform[cgs-kilopound-force]
              @defidform[cgsm-kilopound-force]
              @defidform[mksa-kilopound-force]
              @defidform[mks-kilopound-force])]{
The force of 1 kilopound weight.}

@deftogether[(@defidform[cgs-poundal]
              @defidform[cgsm-poundal]
              @defidform[mksa-poundal]
              @defidform[mks-poundal])]{
The force of 1 poundal.}

@section{Thermal Energy and Power}

@deftogether[(@defidform[cgs-calorie]
              @defidform[cgsm-calorie]
              @defidform[mksa-calorie]
              @defidform[mks-calorie])]{
The energy of 1 calorie.}

@deftogether[(@defidform[cgs-btu]
              @defidform[cgsm-btu]
              @defidform[mksa-btu]
              @defidform[mks-btu])]{
The energy of 1 British Thermal Unit, @math{btu}.}

@deftogether[(@defidform[cgs-therm]
              @defidform[cgsm-therm]
              @defidform[mksa-therm]
              @defidform[mks-therm])]{
The energy of 1 therm.}

@deftogether[(@defidform[cgs-horsepower]
              @defidform[cgsm-horsepower]
              @defidform[mksa-horsepower]
              @defidform[mks-horsepower])]{
The energy of 1 horsepower.}

@section{Pressure}

@deftogether[(@defidform[cgs-bar]
              @defidform[cgsm-bar]
              @defidform[mksa-bar]
              @defidform[mks-bar])]{
The pressure of 1 bar.}

@deftogether[(@defidform[cgs-std-atmosphere]
              @defidform[cgsm-std-atmosphere]
              @defidform[mksa-std-atmosphere]
              @defidform[mks-std-atmosphere])]{
The pressure of 1 standard atmosphere.}

@deftogether[(@defidform[cgs-torr]
              @defidform[cgsm-torr]
              @defidform[mksa-torr]
              @defidform[mks-torr])]{
The pressure of 1 torr.}

@deftogether[(@defidform[cgs-meter-of-mercury]
              @defidform[cgsm-meter-of-mercury]
              @defidform[mksa-meter-of-mercury]
              @defidform[mks-meter-of-mercury])]{
The pressure of 1 meter of mercury.}

@deftogether[(@defidform[cgs-inch-of-mercury]
              @defidform[cgsm-inch-of-mercury]
              @defidform[mksa-inch-of-mercury]
              @defidform[mks-inch-of-mercury])]{
The pressure of 1 inch of mercury.}

@deftogether[(@defidform[cgs-inch-of-water]
              @defidform[cgsm-inch-of-water]
              @defidform[mksa-inch-of-water]
              @defidform[mks-inch-of-water])]{
The pressure of 1 inch of water.}

@deftogether[(@defidform[cgs-psi]
              @defidform[cgsm-psi]
              @defidform[mksa-psi]
              @defidform[mks-psi])]{
The pressure of 1 pound per square inch.}

@section{Viscosity}

@deftogether[(@defidform[cgs-poise]
              @defidform[cgsm-poise]
              @defidform[mksa-poise]
              @defidform[mks-poise])]{
The dynamic viscosity of 1 poise.}

@deftogether[(@defidform[cgs-stokes]
              @defidform[cgsm-stokes]
              @defidform[mksa-stokes]
              @defidform[mks-stokes])]{
The kinematic viscosity of 1 stokes.}

@section{Light and Illumination}

@deftogether[(@defidform[cgs-stilb]
              @defidform[cgsm-stilb]
              @defidform[mksa-stilb]
              @defidform[mks-stilb])]{
The luminance of 1 stilb.}

@deftogether[(@defidform[cgs-lumen]
              @defidform[cgsm-lumen]
              @defidform[mksa-lumen]
              @defidform[mks-lumen])]{
The lunimous flux of 1 lumen.}

@deftogether[(@defidform[cgs-lux]
              @defidform[cgsm-lux]
              @defidform[mksa-lux]
              @defidform[mks-lux])]{
The illuminance of 1 lux.}

@deftogether[(@defidform[cgs-phot]
              @defidform[cgsm-phot]
              @defidform[mksa-phot]
              @defidform[mks-phot])]{
The illuminance of 1 phot.}

@deftogether[(@defidform[cgs-footcandle]
              @defidform[cgsm-footcandle]
              @defidform[mksa-footcandle]
              @defidform[mks-footcandle])]{
The illuminance of 1 footcandle.}

@deftogether[(@defidform[cgs-lambert]
              @defidform[cgsm-lambert]
              @defidform[mksa-lambert]
              @defidform[mks-lambert])]{
The luminance of 1 lambert.}

@deftogether[(@defidform[cgs-footlambert]
              @defidform[cgsm-footlambert]
              @defidform[mksa-footlambert]
              @defidform[mks-footlambert])]{
The luminance of 1 footlambert.}

@section{Radioactivity}

@deftogether[(@defidform[cgs-curie]
              @defidform[cgsm-curie]
              @defidform[mksa-curie]
              @defidform[mks-curie])]{
The activity of 1 curie.}

@deftogether[(@defidform[cgs-roentgen]
              @defidform[cgsm-roentgen]
              @defidform[mksa-roentgen]
              @defidform[mks-roentgen])]{
The exposure of 1 roentgen.}

@deftogether[(@defidform[cgs-rad]
              @defidform[cgsm-rad]
              @defidform[mksa-rad]
              @defidform[mks-rad])]{
The absorbed dose of 1 rad.}

@section{Force and Energy}

@deftogether[(@defidform[cgs-newton]
              @defidform[cgsm-newton]
              @defidform[mksa-newton]
              @defidform[mks-newton])]{
The SI unit of force, 1 Newton.}

@deftogether[(@defidform[cgs-dyne]
              @defidform[cgsm-dyne]
              @defidform[mksa-dyne]
              @defidform[mks-dyne])]{
The force of 1 dyne = @math{10^-5} Newton.}

@deftogether[(@defidform[cgs-joule]
              @defidform[cgsm-joule]
              @defidform[mksa-joule]
              @defidform[mks-joule])]{
The SI unit of energy, 1 Joule.}

@deftogether[(@defidform[cgs-erg]
              @defidform[cgsm-erg]
              @defidform[mksa-erg]
              @defidform[mks-erg])]{
The energy of 1 erg = @math{10^-7} Joule.}

@section{Prefixes}

The constants are dimensionless scaling factors.

@defidform[num-yotta]{@math{10^24}}
@defidform[num-zetta]{@math{10^21}}
@defidform[num-exa]{@math{10^18}}
@defidform[num-peta]{@math{10^15}}
@defidform[num-tera]{@math{10^12}}
@defidform[num-giga]{@math{10^9}}
@defidform[num-mega]{@math{10^6}}
@defidform[num-kilo]{@math{10^3}}
@defidform[num-milli]{@math{10^-3}}
@defidform[num-micro]{@math{10^-6}}
@defidform[num-nano]{@math{10^-9}}
@defidform[num-pico]{@math{10^-12}}
@defidform[num-femto]{@math{10^-15}}
@defidform[num-atto]{@math{10^-18}}
@defidform[num-zepto]{@math{10^-21}}
@defidform[num-yacto]{@math{10^-24}}

@section{Physical Constants Example}

The following program demonstrates the use of the physical constants in a calculation. In this case, the goal is to calculate the range of light tracel times from Earth to Mars.

The required data is the average distance of each planet from the Sun in asttonomical units (the eccentricities and inclinations of the orbots will be neglected for the purpose of this calculation). The average radius of the orbit of Mars is 1.52 astronomical units and for the orbit of Earth it is 1 astronomical unit (by definition). These values are combined with the MKSA values for the constants for the speed of light (m/s) and the length of an astronomical unit (m) to produve a result for the shortest and longest light travel time in seconds. The figures are converted into minutes before being displayed.

@racketmod[
racket
(require (planet williams/science/constants/mksa-constants))

(define c mksa-speed-of-light) (code:comment "m/s")
(define au mksa-astronomical-unit) (code:comment "m")
(define minutes mksa-minute) (code:comment "s")

(code:comment "Orbit radii in meters")
(define r-earth (* 1.0 au)) (code:comment "m")
(define r-mars (* 1.52 au)) (code:comment "m")

(code:comment "Light travel times in seconds")
(define t-min (/ (- r-mars r-earth) c)) (code:comment "s")
(define t-max (/ (+ r-mars r-earth) c)) (code:comment "s")

(printf "Light travel time from Earth to Mars:~n")
(printf "min = ~a minutes~n"
        (real->decimal-string (/ t-min minutes) 1))
(printf "max = ~a minutes~n"
        (real->decimal-string (/ t-max minutes) 1))
]

Here is the output from the program.

@verbatim{
Light travel time from Earth to Mars:
min = 4.3 minutes
max = 21.0 minutes
}
