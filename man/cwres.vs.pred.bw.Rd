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

\name{cwres.vs.pred.bw}
\alias{cwres.vs.pred.bw}
\title{Box-and-whisker plot of conditional weighted residuals vs population predictions
  for Xpose 4}
\description{
  This creates a box and whisker plot of conditional weighted residuals (CWRES) vs 
  population predictions (PRED), and is a specific function in Xpose 4. 
  It is a wrapper encapsulating arguments to the \code{xpose.plot.bw} 
  function. Most of the options take their default values from xpose.data 
  object but may be overridden by supplying them as arguments.
}
\usage{
cwres.vs.pred.bw(object,
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An xpose.data object. }
  \item{\dots}{Other arguments passed to \code{link{xpose.plot.bw}}.}
}
\details{

  This creates a box and whisker plot of conditional weighted residuals (CWRES) vs 
  population predictions (PRED), and is a specific function in Xpose 4. 
  It is a wrapper encapsulating arguments to the \code{xpose.plot.bw} 
  function. Most of the options take their default values from xpose.data 
  object but may be overridden by supplying them as arguments.
  
  Conditional weighted residuals (CWRES) require some extra steps to
  calculate. See \code{\link{compute.cwres}} for details.

  A wide array of extra options controlling bwplots are 
  available. See \code{\link{xpose.plot.bw}} and 
  \code{\link{xpose.panel.bw}} for details.
}
\value{
  Returns a box-and-whisker plot of CWRES vs PRED.
}
\author{E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins}
\seealso{
  \code{\link{xpose.plot.bw}},
  \code{\link{xpose.panel.bw}},
  \code{\link[lattice]{bwplot}},
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
cwres.vs.pred.bw(xpdb)

}
\keyword{methods}
