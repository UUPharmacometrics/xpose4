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

\name{cwres.wres.vs.idv}
\alias{cwres.wres.vs.idv}
\alias{cwres.wres.vs.pred}
\title{Weighted residuals (WRES) and conditional WRES (CWRES) plotted against
 the independent variable (IDV) or the population predictions (PRED) for
 Xpose 4} 
\description{
  This function graphically compares WRES and CWRES as plotted against the
  independent variable or the popultation predictions.
}
\usage{
cwres.wres.vs.idv(object,
           ylb  = "Residuals",
           abline = c(0,0),
           smooth=TRUE,
           scales=list(),
           ...)

cwres.wres.vs.pred(object,
           ylb  = "Residuals",
           abline = c(0,0),
           smooth=TRUE,
           scales=list(),
           ...)
}
\arguments{
  \item{object}{An xpose.data object. }
  \item{ylb}{A string giving the label for the y-axis. \code{NULL} if none.}
  \item{abline}{Vector of arguments to the \code{\link[lattice]{panel.abline}}
    function. No abline is drawn if \code{NULL}.}
  \item{smooth}{ A \code{NULL} value indicates that no superposed line
    should be added to the graph. If \code{TRUE} then a smooth of the data will
    be superimposed.}
  \item{scales}{scales is pased to \code{xpose.plot.default} }
  \item{\dots}{Other arguments passed to \code{link[lattice]{xyplot}}.}           
}
\details{
  This function creates plots of WRES and CWRES, presented side-by-side
  for comparison.  
  
  Conditional weighted residuals (CWRES) require some extra steps to
  calculate. See \code{\link{compute.cwres}} for details.

  A wide array of extra options controlling xyplots are 
  available. See \code{\link{xpose.plot.default}} and 
  \code{\link{xpose.panel.default}} for details.
}
\value{
  A compound xyplot.
}
\author{Niclas Jonsson & Andrew Hooker}

\seealso{
  \code{\link{xpose.plot.default}},
  \code{\link{xpose.panel.default}},
  \code{\link[lattice]{xyplot}},
  \code{\link{xpose.prefs-class}},
  \code{\link{xpose.data-class}},
  \code{\link{compute.cwres}}
}
\examples{
\dontrun{
## We expect to find the required NONMEM run, table and data files for run
## 5 in the current working directory 
xpdb5 <- xpose.data(5)
}

## Here we load the example xpose database 
data(simpraz.xpdb)
xpdb <- simpraz.xpdb

cwres.wres.vs.idv(xpdb)
}
\keyword{methods}
