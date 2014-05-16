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

\name{parm.vs.cov}
\alias{parm.vs.cov}
\title{Parameters plotted against covariates, for Xpose 4}
\description{
  This creates a stack of plots of Bayesian parameter estimates plotted 
  against covariates, and is a specific function in Xpose 4. It is a 
  wrapper encapsulating arguments to the \code{xpose.plot.default} 
  function. Most of the options take their default values from xpose.data 
  object but may be overridden by supplying them as arguments.
}
\usage{
parm.vs.cov(object,
           onlyfirst=TRUE,
           smooth=TRUE,
           type="p",
           main="Default",
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An xpose.data object. }
  \item{onlyfirst}{Logical value indicating whether only the first row
    per individual is included in the plot. }
  \item{smooth}{Logical value indicating whether an x-y smooth should be superimposed.
    The default is TRUE.}
  \item{type}{The plot type - defaults to points only.}
  \item{main}{The title of the plot.  If \code{"Default"} then a default
    title is plotted. Otherwise the value should be a string like
    \code{"my title"} or \code{NULL} for no plot title.  For
    \code{"Default"} the function
    \code{\link{xpose.multiple.plot.title}} is used.}
  \item{\dots}{Other arguments passed to \code{link{xpose.plot.default}}.}
}
\details{
  Each of the parameters in the Xpose data object, as specified in 
  \code{object@Prefs@Xvardef$parms}, is plotted against each covariate
  present, as specified in \code{object@Prefs@Xvardef$covariates}, 
  creating a stack of plots.

  A wide array of extra options controlling \code{xyplots} are 
  available. See \code{\link{xpose.plot.default}} and 
  \code{\link{xpose.panel.default}} for details.
}
\value{
  Returns a stack of xyplots and histograms of parameters against covariates.
}
\author{E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins}
\seealso{
  \code{\link{xpose.plot.default}},
  \code{\link{xpose.plot.histogram}},
  \code{\link[lattice]{xyplot}},
  \code{\link[lattice]{histogram}},
  \code{\link{xpose.prefs-class}},
  \code{\link{xpose.data-class}}
}
\examples{
\dontrun{
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb <- xpose.data(5)

## A vanilla plot
parm.vs.cov(xpdb)

## Custom colours and symbols, IDs
parm.vs.cov(xpdb, cex=0.6, pch=3, col=1, ids=TRUE)
}
}
\keyword{methods}
