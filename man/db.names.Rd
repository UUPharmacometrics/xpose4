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

\name{db.names}
\alias{db.names}
\alias{xpose.print}
\title{Prints the contents of an Xpose data object}
\description{
  These functions print a summary of the specified Xpose object to the R
  console.
}
\usage{
db.names(object)

xpose.print(object,
         long = TRUE)         

}
\arguments{
  \item{object}{An \code{xpose.data} object. }
  \item{long}{A logical operator specifying whether the function should 
                 print an abridged (long = FALSE) or full
                 list of its contents.}
}
\details{
  These functions return a detailed summary of the contents of a specified 
  \code{\link{xpose.data}} object. 
}
\value{
  A detailed summary of the contents of a specified 
  \code{\link{xpose.data}} object.
}
\author{Niclas Jonsson & Justin Wilkins}
\seealso{
  \code{\link{xpose.data}}
}
\examples{
\dontrun{
## xpdb5 is an Xpose data object
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)

xpose.print(xpdb5)
}
}
\keyword{methods}
