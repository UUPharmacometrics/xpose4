% Xpose 4
% An R-based population pharmacokinetic/
% pharmacodynamic model building aid for NONMEM.
% Copyright (C) 1998-2004 E. Niclas Jonsson and Mats Karlsson.
% Copyright (C) 2005-2008 Andrew C. Hooker, Justin J. Wilkins, 
% Mats O. Karlsson and E. Niclas Jonsson.
% Copyright (C) 2009-2010 Andrew C. Hooker, Mats O. Karlsson and 
% E. Niclas Jonsson.

% This file is a part of Xpose 4.
% Xpose 4 is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public License
% as published by the Free Software Foundation, either version 3
% of the License, or (at your option) any later version.

% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public License
% along with this program.  A copy can be cound in the R installation
% directory under \share\licenses. If not, see http://www.gnu.org/licenses/.

\name{parm.splom}
\alias{parm.splom}
\alias{ranpar.splom}
\alias{cov.splom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot scatterplot matrices of parameters, random parameters or covariates}
\description{
These functions plot scatterplot matrices of parameters, random parameters and 
covariates. 
}
\usage{
parm.splom(object,  
           main = xpose.multiple.plot.title(object=object,
                        plot.text = "Scatterplot matrix of parameters",
                        ...),
           varnames  = NULL,
           onlyfirst=TRUE,
           smooth = TRUE,
           lmline = NULL,
           ...)
           
ranpar.splom(object,  
             main = xpose.multiple.plot.title(object=object,
                        plot.text = "Scatterplot matrix of random parameters",
                        ...),
             varnames  = NULL,
             onlyfirst=TRUE,
             smooth = TRUE,
             lmline = NULL,
             ...)
         
cov.splom(object,  
          main = xpose.multiple.plot.title(object=object,
                        plot.text = "Scatterplot matrix of covariates",
                        ...),
          varnames  = NULL,
          onlyfirst=TRUE,
          smooth = TRUE,
          lmline = NULL,
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An xpose.data object. }
  \item{main}{A string giving the plot title or \code{NULL} if none.}
  \item{varnames}{A vector of strings containing labels for the variables in the
    scatterplot matrix.}
  \item{onlyfirst}{Logical value indicating if only the first row
    per individual is included in the plot. }
  \item{lmline}{logical variable specifying whether a linear regression line should
    be superimposed over an \code{\link[lattice]{xyplot}}. \code{NULL} ~ FALSE. (\code{y~x})}
  \item{smooth}{ A \code{NULL} value indicates that no superposed line
    should be added to the graph. If \code{TRUE} then a smooth of the data will
    be superimposed.}
  \item{\dots}{Other arguments passed to \code{\link{xpose.plot.histogram}}.}
}
\details{
  The parameters or covariates in the Xpose data object, as specified in 
  \code{object@Prefs@Xvardef$parms}, \code{object@Prefs@Xvardef$ranpar} or 
  \code{object@Prefs@Xvardef$covariates}, 
  are plotted together as scatterplot matrices. \code{parm.splom} delivers
  parameters, \code{ranpar.splom} delivers random
  parameters, and \code{cov.splom} delivers covariates.
  
  A wide array of extra options controlling scatterplot matrices are 
  available. See \code{\link{xpose.plot.splom}} for
  details.

  To control the appearance of the labels and names in the scatterplot
  matrix plots you can try \code{varname.cex=0.5} and
  \code{axis.text.cex=0.5} (this changes the tick labels and the
  variable names to be half as large as normal).
}
\value{
  Delivers a scatterplot matrix.
}
\author{Andrew Hooker & Justin Wilkins}

\seealso{
\code{\link{xpose.plot.splom}},
\code{\link{xpose.panel.splom}},
\code{\link[lattice]{splom}},
\code{\link{xpose.data-class}},
\code{\link{xpose.prefs-class}}
}
\examples{
\dontrun{
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)
}

## Here we load the example xpose database 
data(simpraz.xpdb)
xpdb <- simpraz.xpdb

## A scatterplot matrix of parameters, grouped by sex
parm.splom(xpdb, groups="SEX")

## A scatterplot matrix of ETAs, grouped by sex
ranpar.splom(xpdb, groups="SEX")

## Covariate scatterplots, with text customization
cov.splom(xpdb, varname.cex=0.4, axis.text.cex=0.4, smooth=NULL, cex=0.4)
}
\keyword{methods}% at least one, from doc/KEYWORDS

