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

\name{basic.gof}
\alias{basic.gof}  
\title{Basic goodness-of-fit plots, for Xpose 4}
\description{
  This is a compound plot consisting of plots of observations (DV) vs 
  population predictions (PRED), observations (DV) vs individual predictions
  (IPRED), absolute individual weighted residuals (|IWRES|) vs IPRED, and 
  weighted population residuals (CWRES) vs independent variable (IDV), a 
  specific function in Xpose 4. WRES are also supported. It is a wrapper 
  encapsulating arguments to the \code{dv.vs.pred}, \code{dv.vs.ipred}, 
  \code{absval.iwres.vs.ipred} and \code{wres.vs.idv} functions. 
}
\usage{
basic.gof(object,
           force.wres=FALSE,
           main="Default",
           use.log = FALSE,
           ...)
           
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An xpose.data object. }
  \item{force.wres}{Should the plots use WRES?  Values can be
    \code{TRUE/FALSE}.  Otherwise the CWRES are used if present.}
  \item{main}{The title of the plot.  If \code{"Default"} then a default
    title is plotted. Otherwise the value should be a string like
    \code{"my title"} or \code{NULL} for no plot title.  For
    \code{"Default"} the function
    \code{\link{xpose.multiple.plot.title}} is used.}
  \item{use.log}{Should we use log transformations in the plots?}
  \item{\dots}{Other arguments passed to \code{\link{xpose.plot.default}}.}
}
\details{
  Four basic goodness-of-fit plots are presented side by side for 
  comparison.
  
  Conditional weighted residuals (CWRES) require some extra steps to
  calculate. See \code{\link{compute.cwres}} for details.

  A wide array of extra options controlling xyplots are 
  available. See \code{\link{xpose.plot.default}} for
  details.

  \code{basic.gof.cwres} is just a wrapper for \code{basic.gof} with
  \code{use.cwres=TRUE}.
}
\value{
  Returns a compound plot comprising plots of observations (DV) vs population 
  predictions (PRED), DV vs individual predictions (IPRED), absolute individual
  weighted residuals (|IWRES|) vs IPRED, and weighted populations residuals 
  (WRES) vs the independent variable (IDV).
}
\author{E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins}
\seealso{
  \code{\link{dv.vs.pred}},
  \code{\link{dv.vs.ipred}},
  \code{\link{absval.iwres.vs.ipred}},
  \code{\link{wres.vs.idv}},
  \code{\link{cwres.vs.idv}},
  \code{\link{xpose.plot.default}},
  \code{\link{xpose.panel.default}},
  \code{\link[lattice]{xyplot}},
  \code{\link{compute.cwres}},
  \code{\link{xpose.prefs-class}},
  \code{\link{xpose.data-class}}
}
\examples{
\dontrun{
## We expect to find the required NONMEM run and table files for run 1
## in the current working directory
xpdb <- xpose.data(1)
}

## Here we load the example xpose database 
data(simpraz.xpdb)
xpdb <- simpraz.xpdb

## A vanilla plot
basic.gof(xpdb)

## Custom colours and symbols, IDs of individuals in study
basic.gof(xpdb, cex=0.6, pch=8, col=1, ids=TRUE)
}
\keyword{methods}
