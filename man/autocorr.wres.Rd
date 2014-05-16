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

\name{autocorr.wres}
\alias{autocorr.wres}
\alias{autocorr.iwres}
\title{Autocorrelation of weighted residuals for Xpose 4}
\description{
  This is an autocorrelation plot of weighted residuals.
  Most of the options take their default values from the xpose.data 
  object but may be overridden by supplying them as arguments.
}
\usage{
autocorr.wres(object,
           type="p",
           smooth=TRUE,
           ids=F,
           main = "Default",
           ...)

autocorr.iwres(object,
           type="p",
           smooth=TRUE,
           ids=F,
           main = "Default",
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An xpose.data object. }
  \item{smooth}{Logical value indicating whether a smooth should be superimposed.}
  \item{type}{1-character string giving the type of plot desired. The
    following values are possible, for details, see \code{\link{plot}}: '"p"'
    for points, '"l"' for lines, '"o"' for overplotted points and
    lines, '"b"', '"c"') for (empty if '"c"') points joined by
    lines, '"s"' and '"S"' for stair steps and '"h"' for
    histogram-like vertical lines.  Finally, '"n"' does not
    produce any points or lines. }
  \item{ids}{A logical value indicating whether text labels should be
    used as plotting symbols (the variable used for these symbols
    indicated by the \code{idlab} xpose data variable).}
  \item{main}{The title of the plot.  If \code{"Default"} then a default
    title is plotted. Otherwise the value should be a string like
    \code{"my title"} or \code{NULL} for no plot title.  For
    \code{"Default"} the function
    \code{\link{xpose.multiple.plot.title}} is used.}
  \item{\dots}{Other arguments passed to \code{link{xpose.plot.default}}.}
}
\details{
  A wide array of extra options controlling xyplots are 
  available. See \code{\link{xpose.plot.default}} for
  details.
}
\value{
  Returns an aotocorrelation plot for weighted population residuals
           (WRES) or individual weighted residuals (IWRES).
}
\author{E. Niclas Jonsson, Mats Karlsson, Justin Wilkins & Andrew Hooker}
\seealso{
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
autocorr.wres(xpdb)

## A conditioning plot
autocorr.wres(xpdb, dilution=TRUE)

## Custom heading and axis labels
autocorr.wres(xpdb, main="My conditioning plot", ylb="|CWRES|", xlb="PRED")

## Custom colours and symbols, IDs
autocorr.wres(xpdb, cex=0.6, pch=3, col=1, ids=TRUE)

## A vanilla plot with IWRES
autocorr.iwres(xpdb)
}
\keyword{methods}
