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

\name{xpose.ask.for.filename}
\alias{xpose.ask.for.filename}
\alias{xpose.ask.for.mod}
\alias{xpose.ask.for.lst}
\title{Function to ask the user for the name of a file}
\description{
Asks the user for the name of a file.
}
\usage{
xpose.ask.for.filename(object,
                       listfile = paste("run",object@Runno,".lst",sep= ""),
                       modfile = paste("run",object@Runno,".mod",sep =""),
                       ...)

xpose.ask.for.lst(object,
                  listfile = paste("run",object@Runno,".lst",sep = ""),
                  ...)

xpose.ask.for.mod(object,
                  modfile = paste("run", object@Runno,".mod",sep =""), 
                  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An \code{\link{xpose.data}} object. }
  \item{listfile}{A NONMEM output file }
  \item{modfile}{A NONMEM model file}
  \item{\dots}{Additional arguments passed to the function}
}
\details{
  Function checks if the file exists, if it does then the filename is
  returned from the function.
}
\value{
  The name of the file if it exists, otherwise nothing is returned.
}
\author{Niclas Jonsson, Justin Wilkins, Mats Karlsson and Andrew Hooker}
\keyword{methods}

