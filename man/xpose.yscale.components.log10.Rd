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

\name{xpose.yscale.components.log10}
\alias{xpose.yscale.components.log10}
\alias{xpose.xscale.components.log10}
\alias{xpose.logTicks}
\title{Functions to create nice looking axes when using Log scales.}
\description{
The funcions are used to create standard tic marks and axis labels when
the axes are on the log scale.
}
\usage{
xpose.yscale.components.log10(lim, ...)
xpose.xscale.components.log10(lim, ...)
xpose.logTicks(lim, loc = c(1, 5))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lim}{Limits}
  \item{loc}{Locations}
  \item{\dots}{Additional arguments pased to the function.}
}
\details{
These functions create log scales that look like they should (not the
default R scales). These functions are used as input to the
\code{\link[lattice:axis.default]{xscale.components}} argument in a
  lattice plot.
}
\author{Andrew Hooker}
\seealso{
  \code{\link{xpose.plot.default}}
  \code{\link[lattice:axis.default]{xscale.components}}
}
\examples{
\dontrun{
xpdb5 <- xpose.data(5)
xpose.plot.default("PRED","DV",xpdb,logy=T,logx=T)
xpose.plot.default("PRED","DV",xpdb,logy=T,logx=T,
                   yscale.components = xpose.yscale.components.log10,
                   xscale.components = xpose.xscale.components.log10)

## both give the same result
}
}
\keyword{methods}

