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
\name{make.sb.data}
\alias{make.sb.data}
\title{
Make stacked bar data set.
}
\description{
Function to make stacked bar data set for categorical data plots.
}
\usage{
make.sb.data(data, idv, dv, nbins = 6, by = NULL, by.nbins = 6, ...)
}
\arguments{
  \item{data}{
Data set to transform.
}
  \item{idv}{
the idenpendent variable.
}
  \item{dv}{
the dependent variable.
}
  \item{nbins}{
the number of bins.
}
  \item{by}{
Conditioning variable.
}
  \item{by.nbins}{
by.nbins.
}
  \item{\dots}{
additional arguments.
}
}
\author{
The Xpose team.
}
\keyword{ methods }

