Xpose 4
====================

[![Travis-CI Build Status](https://travis-ci.org/andrewhooker/xpose4.svg?branch=master)](https://travis-ci.org/andrewhooker/xpose4)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/xpose4)](https://CRAN.R-project.org/package=xpose4) 
[![codecov.io](https://codecov.io/github/andrewhooker/xpose4/coverage.svg?branch=master)](https://codecov.io/github/andrewhooker/xpose4?branch=master)


by Andrew C. Hooker, Mats O. Karlsson 
and E. Niclas Jonsson

http://xpose.sourceforge.net/


## Introduction

Xpose 4 is a ground-floor rewrite of Xpose 3.1. Chief amongst the enhancements
in the new version is the migration from S-PLUS to R, a free,
multi-platform statistical environment.  We have also added direct access to Xpose
functions from the command line, and, through R, access to Xpose tools by
third-party applications.


## R installation

To install xpose you will need:

* R version >=2.2.0. Download the latest version of R from www.r-project.org.


## Xpose 4 Installation

Install xpose in R using one of the following methods:

* latest stable release -- From CRAN.  Write at the R command line:
     
```
install.packages("xpose4")
```

* Latest development version -- from Github. Note that the command below installs the "master" 
(development) branch; if you want the release branch from Github add `ref="release"` to the
`install_github()` call. The `install_github()` approach requires that you build from source, 
i.e. `make` and compilers must be installed on your system -- see the R FAQ for your operating system; 
you may also need to install dependencies manually.

```
devtools::install_github("andrewhooker/xpose4")
```


## Running Xpose 4

Start R

To use the classic menu system, type at the R command prompt:
 
```
library(xpose4)
xpose4()
```

Each function is independently available from the command
line, once the Xpose library is loaded. For example, assuming your run is called
'run5.mod', you might do the following:

* Import data

```
  xpdb5 <- xpose.data(5)
```

* Display goodness-of-fit plots

```
  basic.gof(xpdb5)
```

More help is available in the online documentation, which can be found by
typing (for example) `?xpose` at the R command line.  


## The Xpose 4 Bestiary

A more detailed description of Xpose with example plots and explanaitions for
most of the functions in the package is available in our Bestiarium: 
http://xpose.sourceforge.net/bestiarium_v1.0.pdf


## Don't Panic

Andrew Hooker (andrew.hooker at farmbio.uu.se)
should be able to get you an answer if you run into trouble.  The
website http://xpose.sf.net  should also be of help.


## Release Schedule

Bugfix releases will be released regularly, fixing any problems that are
found. 


## License

Xpose 4 is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details 
<http://www.gnu.org/licenses/>.


## Known Bugs

None at present, but there will certainly be a few


