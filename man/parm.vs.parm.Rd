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

\name{parm.vs.parm}
\alias{parm.vs.parm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot parameters vs other parameters}
\description{
This function plots the parameter values stored in an Xpose data object
versus each other in a series of graphs.  The mirror functionality is
available for this function. 
}
\usage{
parm.vs.parm(object,
           onlyfirst=TRUE,
           abline= FALSE,
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
  \item{abline}{Allows for a line of identity.}
  \item{smooth}{Logical value indicating whether an x-y smooth should be superimposed.
    The default is TRUE.}
  \item{type}{The plot type - defaults to points only.}
  \item{main}{The title of the plot.  If \code{"Default"} then a default
    title is plotted. Otherwise the value should be a string like
    \code{"my title"} or \code{NULL} for no plot title.  For
    \code{"Default"} the function
    \code{\link{xpose.multiple.plot.title}} is used.}
  \item{\dots}{Other arguments passed to \code{xpose.plot.default}.}
}
\details{
  Each of the parameters in the Xpose data object, as specified in 
  \code{object@Prefs@Xvardef$parms}, is plotted against the rest, 
  creating a stack of plots.
  
  A wide array of extra options controlling \code{xyplots} are 
  available. See \code{\link{xpose.plot.default}} and 
  \code{\link{xpose.panel.default}} for details.
}
\value{
Returns a stack of xyplots and histograms of parameters against parameters.
}
\author{Andrew Hooker}
\examples{
\dontrun{
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb <- xpose.data(5)


parm.vs.parm(xpdb)


parm.vs.parm(xpdb,mirror=3)
}
}

\keyword{methods}

