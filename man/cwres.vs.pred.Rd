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

\name{cwres.vs.pred}
\alias{cwres.vs.pred}
\title{Population conditional weighted residuals (CWRES) plotted against population 
  predictions (PRED) for Xpose 4}
\description{
  This is a plot of population conditional weighted residuals (cwres) vs population 
  predictions (PRED), a specific function in Xpose 4. It is a wrapper 
  encapsulating arguments to the \code{xpose.plot.default} function. Most 
  of the options take their default values from xpose.data object but may 
  be overridden by supplying them as arguments.
}
\usage{
cwres.vs.pred(object,
           abline=c(0,0),
           smooth = TRUE,
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An xpose.data object. }
  \item{smooth}{Logical value indicating whether an x-y smooth should be superimposed.
    The default is TRUE.}
  \item{abline}{Vector of arguments to the \code{\link[lattice]{panel.abline}}
    function. No abline is drawn if \code{NULL}. }
  \item{\dots}{Other arguments passed to \code{link{xpose.plot.default}}.}
}
\details{

  Conditional weighted residuals (CWRES) require some extra steps to
  calculate. See \code{\link{compute.cwres}} for details.

  A wide array of extra options controlling xyplots are 
  available. See \code{\link{xpose.plot.default}} and 
  \code{\link{xpose.panel.default}} for details.
}
\value{
  Returns an xyplot of CWRES vs PRED.
}
\author{E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins}
\seealso{
  \code{\link{xpose.plot.default}},
  \code{\link[lattice]{xyplot}},
  \code{\link{xpose.prefs-class}},
  \code{\link{compute.cwres}},
  \code{\link{xpose.data-class}}
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

## A vanilla plot
cwres.vs.pred(xpdb)

## A conditioning plot
cwres.vs.pred(xpdb, by="HCTZ")

## Logarithmic Y-axis
cwres.vs.pred(xpdb, logy=TRUE)

## Custom colours and symbols, IDs
cwres.vs.pred(xpdb, cex=0.6, pch=3, col=1, ids=TRUE)

}
\keyword{methods}
