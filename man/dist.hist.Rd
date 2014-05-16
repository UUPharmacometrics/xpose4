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

\name{dist.hist}
\alias{parm.hist}
\alias{cov.hist}
\alias{ranpar.hist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot the parameter or covariate distributions using a histogram}
\description{
These functions plot the parameter or covariate values stored in an Xpose data object
using histograms. 
}
\usage{
parm.hist(object,
           onlyfirst = TRUE,
           main="Default",
           ...)
         
cov.hist(object,
           onlyfirst=TRUE,
           main="Default",
           ...)
           
ranpar.hist(object,
           onlyfirst=TRUE,
           main="Default",
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An xpose.data object. }
  \item{onlyfirst}{Logical value indicating if only the first row
    per individual is included in the plot. }
  \item{main}{The title of the plot.  If \code{"Default"} then a default
    title is plotted. Otherwise the value should be a string like
    \code{"my title"} or \code{NULL} for no plot title.  For
    \code{"Default"} the function
    \code{\link{xpose.multiple.plot.title}} is used.}
  \item{\dots}{Other arguments passed to \code{\link{xpose.plot.histogram}}.}
}
\details{
  Each of the parameters or covariates in the Xpose data object, as specified in 
  \code{object@Prefs@Xvardef$parms}, \code{object@Prefs@Xvardef$covariates} or 
  \code{object@Prefs@Xvardef$ranpar} is evaluated in turn, creating a stack of histograms.
  
  A wide array of extra options controlling histograms are 
  available. See \code{\link{xpose.plot.histogram}} for details.
}
\value{
  Delivers a stack of histograms.
}
\author{Andrew Hooker & Justin Wilkins}

\seealso{
\code{\link{xpose.plot.histogram}},
\code{\link{xpose.panel.histogram}},
\code{\link[lattice]{histogram}},
\code{\link{xpose.data-class}},
\code{\link{xpose.prefs-class}}
}
\examples{
\dontrun{
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)
}

## Here we load the example xpose database from the xpose4data package
data(simpraz.xpdb)
xpdb <- simpraz.xpdb

\dontrun{
## A stack of parameter histograms, with 3 mirror panes
parm.hist(xpdb, mirror=3)
}

## Covariate distribution, in green
cov.hist(xpdb, hicol=11, hidcol="DarkGreen", hiborder="White")
}
\keyword{methods}% at least one, from doc/KEYWORDS

