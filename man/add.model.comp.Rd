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

\name{add.model.comp}
\alias{add.model.comp}  
\title{Additional model comparison plots, for Xpose 4}
\description{
  This creates a stack of four plots, comparing absolute values of PRED, absolute values of IPRED, 
  delta CWRES (or WRES) and delta IWRES
  estimates for the two specified model fits. 
}
\usage{
add.model.comp(object,
           object.ref = NULL,
           onlyfirst = FALSE,
           inclZeroWRES = FALSE,
           subset = xsubset(object),
           main="Default",
           force.wres=FALSE,
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An xpose.data object. }
  \item{object.ref}{An xpose.data object. If not supplied, the user will be 
    prompted.}
  \item{inclZeroWRES}{Logical value indicating whether rows with WRES=0
    is included in the plot. The default is TRUE. }
  \item{onlyfirst}{Logical value indicating whether only the first row
    per individual is included in the plot. }
  \item{subset}{A string giving the subset expression to be applied to
    the data before plotting. See \code{\link{xsubset}}.}
  \item{main}{The title of the plot.  If \code{"Default"} then a default
    title is plotted. Otherwise the value should be a string like
    \code{"my title"} or \code{NULL} for no plot title.  For
    \code{"Default"} the function
    \code{\link{xpose.multiple.plot.title}} is used.}
  \item{force.wres}{Should we use the WRES in the plots instead of CWRES
    (logical \code{TRUE} or \code{FALSE})}
  \item{\dots}{Other arguments passed to \code{link{xpose.plot.default}}.}
}
\details{
  Four model comparison plots are displayed in sequence.
  
  Conditional weighted residuals (CWRES) require some extra steps to
  calculate. See \code{\link{compute.cwres}} for details.

  A wide array of extra options controlling xyplots are 
  available. See \code{\link{xpose.plot.default}} for details.
}
\value{
  Returns a stack of plots comprising comparisons of absolute values of PRED, 
  absolute values of IPRED, absolute differences in CWRES (or WRES)
  and absolute differences in IWRES for the two specified runs.
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

## A vanilla plot, without prompts
add.model.comp(xpdb5, xpdb6, prompt = FALSE)

## Custom colours and symbols, no user IDs
add.model.comp(xpdb5, xpdb6, cex=0.6, pch=8, col=1, ids=NULL)
}

}
\keyword{methods}
