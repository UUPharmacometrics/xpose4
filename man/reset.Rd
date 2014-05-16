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

\name{reset}
\alias{reset.graph.par}
\alias{reset.variables}
\title{Resets Xpose variable definitions to factory settings}
\description{
  Function to reset Xpose's graphics parameters definitions
  to the default.
}
\usage{
reset.graph.par(object,
                classic = FALSE)
}
\arguments{
  \item{object}{An \code{xpose.data} object. }
  \item{classic}{A logical operator specifying whether the function should
                 assume the classic menu system. This is an internal option
                 and need never be called from the command line.}
}
\details{
  This functions is used to reset Xpose's graphic settings definitions
  to their default values. Graphical settings are read from 
  the file 'xpose.ini' in the root of the 'xpose4' package.
}
\value{
  An \code{\link{xpose.data}} object (classic == FALSE) or null (classic == TRUE).
}
\author{Niclas Jonsson & Justin Wilkins}
\seealso{
  \code{\link{xpose.prefs-class}},
  \code{\link{import.graph.par}},
  \code{\link{change.xvardef}}
}
\examples{
\dontrun{
## xpdb5 is an Xpose data object
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)

## Import graphics preferences you saved earlier using export.graph.par
xpdb5 <- import.graph.par(xpdb5)

## Reset to default values
xpdb5 <- reset.graph.par(xpdb5)

## Change WRES definition
xpdb5 <- change.wres(xpdb5)
}
}
\keyword{methods}
