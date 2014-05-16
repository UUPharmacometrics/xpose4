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

\name{xpose.panel.qq}
\alias{xpose.panel.qq}
\title{Default QQ panel function for Xpose 4}
\description{
  This is the QQ panel function for Xpose 4. This is not 
  intended to be used outside the \code{xpose.plot.qq} function. 
  Most of the arguments take their default values from xpose.data object but this
  can be overridden by supplying them as argument to \code{xpose.plot.qq}.
}
\usage{
xpose.panel.qq(x, object,
         pch = object@Prefs@Graph.prefs$pch,
         col = object@Prefs@Graph.prefs$col,
         cex = object@Prefs@Graph.prefs$cex,
         abllty = object@Prefs@Graph.prefs$abllty,
         abllwd = object@Prefs@Graph.prefs$abllwd,
         ablcol = object@Prefs@Graph.prefs$ablcol,
         grid = object@Prefs@Graph.prefs$grid,
         ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Name(s) of the x-variable.}
  \item{object}{An xpose.data object. }
  %\item{ylab}{Caption for the y-axis. }
  %\item{xlab}{Caption for the x-axis. }
  \item{col}{Colour of lines and plot symbols.}
  \item{pch}{Plot character to use.}
  \item{cex}{Amount to scale the plotting character by.}
  \item{abllty}{Line type.}
  \item{abllwd}{Line width.}
  \item{ablcol}{Line colour.}
  \item{grid}{logical value indicating whether a visual reference grid
    should be added to the graph. (Could use arguments for line type,
    color etc).}
  \item{\dots}{Other arguments that may be needed in the function. }
}
\author{E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins}
\seealso{
\code{\link{xpose.plot.qq}},
\code{\link[lattice]{qqmath}},
\code{\link[lattice]{panel.qqmathline}}, 
\code{\link{xpose.data-class}}
}
\keyword{methods}
