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

\name{tabulate.parameters}
\alias{tabulate.parameters}
\title{Tabulate the population parameter estimates}
\description{
  This function provides a summary of the model's parameter estimates and
  precision.
}
\usage{
tabulate.parameters(object, prompt=FALSE, outfile = NULL, dir="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An xpose.data object.}
  \item{prompt}{Ask before printing.}
  \item{outfile}{file to output to (NULL means screen).}
  \item{dir}{Wich directory is the file in. \code{""} means the current
    directory. }
}

\value{
  A table summarizing the parameters and their precision.
}
\author{Niclas Jonsson, Andrew Hooker & Justin Wilkins}


\examples{
\dontrun{ 
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)

tabulate.parameters(xpdb)
}}
\keyword{methods}
