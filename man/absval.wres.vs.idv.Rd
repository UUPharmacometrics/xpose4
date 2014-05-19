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

\name{absval.wres.vs.idv}
\alias{absval.wres.vs.idv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Absolute value of (C)WRES vs. independent variable plot in Xpose4.
}
\description{
  This is a plot of the absolute value of the CWRES (default, other
  residuals as an option) vs
  independent variable, a specific function in Xpose 4. It is a
  wrapper encapsulating arguments to the \code{\link{xpose.plot.default}} function.
  Most of the options take their default values from the xpose.data object but
  may be overridden by supplying them as arguments.
}
\usage{
absval.wres.vs.idv(object,
                   idv = "idv",
                   wres = "Default",
                   ylb = "Default",
                   smooth = TRUE,
                   idsdir = "up",
                   type = "p",
                   ...)
}

\arguments{
  \item{object}{An xpose.data object. }
  \item{idv}{ 
    the independent variable.
  }
  \item{wres}{
    Which weighted residual to use. \code{"Default"} is the CWRES.
  }
  \item{ylb}{
    Y-axis label.
  }
  \item{smooth}{Logical value indicating whether an x-y smooth should be superimposed.
    The default is TRUE.}
  \item{idsdir}{Direction for displaying point labels. The default is "up",
    since we are displaying absolute values. }
  \item{type}{Type of plot. The default is points only ("p"), but lines ("l")
    and both ("b") are also available.}
  \item{\dots}{Other arguments passed to \code{link{xpose.plot.default}}.}
}
\details{
  A wide array of extra options controlling xyplots are 
  available. See \code{\link{xpose.plot.default}} for details.
}
\value{
  Returns an xyplot of |CWRES| vs idv (often TIME, defined by \code{\link{xvardef}}).
}
\author{
Andrew Hooker
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{xpose.plot.default}},
  \code{\link{xpose.panel.default}},
  \code{\link[lattice]{xyplot}},
  \code{\link{xpose.prefs-class}},
  \code{\link{compute.cwres}},
  \code{\link{xpose.data-class}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
absval.wres.vs.idv(xpdb)

## A conditioning plot
absval.wres.vs.idv(xpdb, by="HCTZ")

## Custom heading and axis labels
absval.wres.vs.idv(xpdb, main="Hello World", ylb="|CWRES|", xlb="IDV")

## Custom colours and symbols
absval.wres.vs.idv(xpdb, cex=0.6, pch=3, col=1)

## using the NPDEs instead of CWRES
absval.wres.vs.idv(xpdb,wres="NPDE")

## subsets
absval.wres.vs.idv(xpdb,subset="TIME<10")

}

\keyword{methods}

