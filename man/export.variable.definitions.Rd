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

\name{export.variable.definitions}
\alias{export.variable.definitions}
\title{Exports Xpose variable definitions to a file from an Xpose data object.}
\description{
  This function exports variable definitions for a specified Xpose data object to
  a file.
}
\usage{
export.variable.definitions(object)
}
\arguments{
  \item{object}{An \code{xpose.data} object. }
}
\details{
  This function exports variable defintions (contents of object@Prefs@Xvardef)
  for a given \code{xpose.data} object to a file, typically 'xpose.vardefs.ini'. 
  Note that file format is not the same as used for graphics settings. It is a
  wrapper for the R function \code{\link{dput}}.
}
\value{
  Null.
}
\author{Niclas Jonsson & Justin Wilkins}
\seealso{
  \code{\link{import.variable.definitions}},
  \code{\link{xpose.prefs-class}}
  \code{\link{dput}}
}
\examples{
\dontrun{
## xpdb5 is an Xpose data object
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)

export.variable.definitions(xpdb5)
}
}
\keyword{methods}
