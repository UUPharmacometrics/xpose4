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

\name{export.graph.par}
\alias{export.graph.par}
\alias{xpose.write}
\title{Exports Xpose graphics settings to a file.}
\description{
  This function exports graphics settings for a specified Xpose data object to
  a file.
}
\usage{
export.graph.par(object)

xpose.write(object,
          file = "xpose.ini")
}
\arguments{
  \item{object}{An \code{xpose.data} object. }
  \item{file}{The file to contain exported Xpose settings. }
}
\details{
  This function exports the graphics settings (contents of object@Prefs@Graph.prefs)
  for a given \code{xpose.data} object to a file, typically 'xpose.ini'. It is
  a wrapper for \code{xpose.write}. Note that the file format is not the 
  same as is used in \code{\link{import.variable.definitions}}
  and \code{\link{export.variable.definitions}}. 
}
\value{
  Null.
}
\author{Niclas Jonsson & Justin Wilkins}
\seealso{
  \code{\link{import.graph.par}},
  \code{\link{xpose.prefs-class}}
}
\examples{
\dontrun{
## xpdb5 is an Xpose data object
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)

## For a filename prompt
export.graph.par(xpdb5)

## Command-line driven
xpose.write(xpdb5, "c:/XposeSettings/mytheme.ini")
}
}
\keyword{methods}
