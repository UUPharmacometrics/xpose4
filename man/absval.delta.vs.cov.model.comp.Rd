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

\name{absval.delta.model.comp}
\alias{absval.dcwres.vs.cov.model.comp}  
\alias{absval.dwres.vs.cov.model.comp}
\alias{absval.diwres.vs.cov.model.comp}    
\alias{absval.dpred.vs.cov.model.comp}  
\alias{absval.dipred.vs.cov.model.comp}  
\title{Model comparison plots, of absolute differences in goodness-of-fit
  predictors against covariates, for Xpose 4}
\description{
  These functions plot absolute differences in PRED, IPRED, WRES, CWRES and IWRES
  against covariates for the two specified model fits. 
}
\usage{
absval.dpred.vs.cov.model.comp(object,
           object.ref = NULL,
           type = NULL,
           ylb=expression(paste("|", Delta, "PRED|")),
           main="Default",
           ...)
           
absval.dipred.vs.cov.model.comp(object,
           object.ref = NULL,
           type = NULL,
           ylb=expression(paste("|", Delta, "IPRED|")),
           main="Default",
           ...)
           
absval.dcwres.vs.cov.model.comp(object,
           object.ref = NULL,
           type = NULL,
           ylb=expression(paste("|", Delta, "CWRES|")),
           main="Default",
           ...)
           
absval.dwres.vs.cov.model.comp(object,
           object.ref = NULL,
           type = NULL,
           ylb=expression(paste("|", Delta, "WRES|")),
           main="Default",
           ...)
           
absval.diwres.vs.cov.model.comp(object,
           object.ref = NULL,
           type = NULL,
           ylb=expression(paste("|", Delta, "IWRES|")),
           main="Default",
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An xpose.data object. }
  \item{object.ref}{An xpose.data object. If not supplied, the user will be 
    prompted.}
  \item{type}{1-character string giving the type of plot desired.  The
    following values are possible, for details, see 'plot': '"p"'
    for points, '"l"' for lines, '"o"' for overplotted points and
    lines, '"b"', '"c"') for (empty if '"c"') points joined by
    lines, '"s"' and '"S"' for stair steps and '"h"' for
    histogram-like vertical lines.  Finally, '"n"' does not
    produce any points or lines.}
  \item{ylb}{A string giving the label for the y-axis. \code{NULL} if
    none.}
  \item{main}{The title of the plot.  If \code{"Default"} then a default
    title is plotted. Otherwise the value should be a string like
    \code{"my title"} or \code{NULL} for no plot title.  For
    \code{"Default"} the function
    \code{\link{xpose.multiple.plot.title}} is used.}
  \item{\dots}{Other arguments passed to \code{link{xpose.plot.default}}.}
}
\details{
  These functions produce plots of the absolute difference in population
  predictions (\code{absval.dpred.vs.cov.model.comp}), individual predictions
  (\code{absval.dipred.vs.cov.model.comp}), weighted population residuals
  (\code{absval.dwres.vs.cov.model.comp}), conditional weighted population residuals
  (\code{absval.dcwres.vs.cov.model.comp}) and individual predictions (\code{absval.diwres.vs.cov.model.comp}).
  
  Conditional weighted residuals (CWRES) require some extra steps to
  calculate. See \code{\link{compute.cwres}} for details.

  A wide array of extra options controlling xyplots are 
  available. See \code{\link{xpose.plot.default}} for details.
}
\value{
  Returns a stack of plots comprising comparisons of PRED, IPRED, WRES (or CWRES)
  and IWRES for the two specified runs.
}
\author{E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins}
\seealso{
  \code{\link{xpose.plot.default}},
  \code{\link{xpose.panel.default}},
  \code{\link[lattice]{xyplot}},
  \code{\link{compute.cwres}},
  \code{\link{xpose.prefs-class}},
  \code{\link{xpose.data-class}}
}
\examples{
\dontrun{
## We expect to find the required NONMEM run and table files for runs
## 5 and 6 in the current working directory
xpdb5 <- xpose.data(5)
xpdb6 <- xpose.data(6)

## A basic dWRES plot, without prompts
absval.dwres.vs.cov.model.comp(xpdb5, xpdb6)

## Custom colours and symbols, no user IDs
absval.dpred.vs.cov.model.comp(xpdb5, xpdb6, cex=0.6, pch=8, col=1, ids=NULL)
}

}
\keyword{methods}
