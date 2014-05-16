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
\name{xpose.create.title.text}
\alias{xpose.create.title.text}
\title{
Create Xpose title text for plots.
}
\description{
Create Xpose title text for plots.
}
\usage{
xpose.create.title.text(x, y, text, object, subset, text2 = NULL, ...)
}
\arguments{
  \item{x}{
The x-axis vaiable name.
}
  \item{y}{
The y-axis vaiable name.
}
  \item{text}{
Initial text in title. 
}
  \item{object}{
Xpose data object \code{\link{xpose.data}}.
}
  \item{subset}{
Subset definition.
}
  \item{text2}{
Text at the end of the title.
}
  \item{\dots}{
Additional options passed to function.
}
}
\author{
Andrew C. Hooker
}
\keyword{ methods }
