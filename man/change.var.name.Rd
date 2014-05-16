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

\name{change.var.name}
\alias{change.var.name}
\title{Changes the name of an Xpose data item}
\description{
  This function allows the names of data items in the Xpose database to be
  changed.
}
\usage{
change.var.name(object, 
                classic = FALSE)
}
\arguments{
  \item{object}{An \code{xpose.data} object. }
  \item{classic}{A logical operator specifying whether the function should 
                 assume the classic menu system. This is an internal option 
                 and need never be called from the command line.}
}
\details{
  This function facilitates the changing of data item names in the object@Data
  slot. 
}
\value{
  An \code{\link{xpose.data}} object.
}
\author{Niclas Jonsson & Justin Wilkins}
\seealso{
  \code{\link{Data}},\code{\link{SData}},\code{\link{xpose.data}}
}
\examples{
\dontrun{
## xpdb5 is an Xpose data object
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)

xpdb5 <- change.var.name(xpdb5)
}
}
\keyword{methods}
