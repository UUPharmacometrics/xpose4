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

\name{absval.iwres.vs.pred}
\alias{absval.iwres.vs.pred}
\alias{absval.iwres.vs.idv}
\title{Absolute individual weighted residuals vs population predictions
  or independent variable for
  Xpose 4}
\description{
  This is a plot of absolute individual weighted residuals (|IWRES|) vs
  individual predictions (PRED) or independent variable (IDV), specific
  functions in Xpose 4. These functions are 
  wrappers encapsulating arguments to the \code{xpose.plot.default} function.
  Most of the options take their default values from xpose.data object but
  may be overridden by supplying them as arguments.
}
\usage{
absval.iwres.vs.pred(object,
           ylb  = "|IWRES|",
           smooth       = TRUE,
           idsdir       = "up",
           type         = "p",
           ...)

absval.iwres.vs.idv(object,
           ylb  = "|iWRES|",
           smooth       = TRUE,
           idsdir       = "up",
           type         = "p",
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An xpose.data object. }
  \item{ylb}{A string giving the label for the y-axis. \code{NULL} if none.}
  \item{idsdir}{Direction for displaying point labels. The default is "up",
    since we are displaying absolute values. }
  \item{type}{Type of plot. The default is points only ("p"), but lines ("l")
    and both ("b") are also available.}
  \item{smooth}{Logical value indicating whether an x-y smooth should be superimposed.
    The default is TRUE.}
  \item{\dots}{Other arguments passed to \code{link{xpose.plot.default}}.}
}
\details{
  A wide array of extra options controlling xyplots are 
  available. See \code{\link{xpose.plot.default}} for details.
}
\value{
  Returns an xyplot of |IWRES| vs PRED or |IWRES| vs IDV.
}
\author{E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins}
\seealso{
  \code{\link{xpose.plot.default}},
  \code{\link{xpose.panel.default}},
  \code{\link[lattice]{xyplot}},
  \code{\link{xpose.prefs-class}},
  \code{\link{xpose.data-class}}
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

## A vanilla plot
absval.iwres.vs.pred(xpdb)

## A conditioning plot
absval.iwres.vs.pred(xpdb, by="HCTZ")

## Custom heading and axis labels
absval.iwres.vs.pred(xpdb, main="My conditioning plot", ylb="|IWRES|", xlb="PRED")

## Custom colours and symbols, no IDs
absval.iwres.vs.pred(xpdb, cex=0.6, pch=3, col=1, ids=FALSE)


}
\keyword{methods}
