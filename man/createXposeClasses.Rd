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

\name{createXposeClasses}
\alias{createXposeClasses}
\title{This function creates the Xpose data classes ("xpose.data" and "xpose.prefs")}
\description{
  This function defines and sets the Xpose data classes.
}
\usage{
createXposeClasses(nm7=F)
}
\arguments{
  \item{nm7}{\code{FALSE} if not using NONMEM 7.}
}
\author{ Niclas Jonsson and Andrew C. Hooker}
\note{All the default settings are defined in this function.  }
\seealso{\code{\link{xpose.data-class}},\code{\link{xpose.prefs-class}} }
\keyword{methods}

