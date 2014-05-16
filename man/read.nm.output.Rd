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

\name{Read NONMEM output files}
\alias{fix.wrapped.lines}
\alias{read.lst}
\alias{read.lst7}
\alias{read.lst6}
\alias{read.phi}
\alias{create.parameter.list}
\alias{calc.npar}
\title{Read NONMEM output files into Xpose 4}
\description{
  These are functions that read in a NONMEM output file (a '*.lst' file)
  and then format the input for various \pkg{xpose4specific} fucntions.
}
\usage{
fix.wrapped.lines(par.mat)

read.lst(filename)

read.lst(filename)
read.lst7(filename)
read.lst6(filename)

read.phi(phi.file=NULL,
         phi.prefix="run",
         runno=NULL,
         phi.suffix=".phi",
         quiet=TRUE,
         nm7=TRUE,
	 directory="",
         ...)


create.parameter.list(listfile)

calc.npar(object)

}
\arguments{
  \item{par.mat}{A parameter matrix.}
  \item{filename}{A NONMEM output file.}
  \item{phi.file}{A NONMEM .phi file}
  \item{phi.prefix}{prefix of a NONMEM .phi file}
  \item{runno}{NONMEM run number}
  \item{phi.suffix}{suffix of a NONMEM .phi file}
  \item{quiet}{Quiet or not}
  \item{nm7}{NM7 or not}
\item{directory}{directory in which the NONMEM output files is}
  \item{\dots}{
    Items passed to functions within this function.
  }
  \item{listfile}{A NONMEM output file.}
  \item{object}{The return value of \code{read.lst(filename)}}
}
\details{
  These are internal Xpose functions used to read a NONMEM output file.

  \code{fix.wrapped.lines} unwraps matrix lines that NONMEM wraps in
  table files.
  Assumes no more than 60 ETAs.

  \code{read.lst} parses information out of NONMEM output, specified by
  the \code{filename} argument, for use in run summaries.

  \code{calc.npar} calculates the number and type
  of parameters included in a NONMEM output file
}
\value{
  Internal helper functions for the generic Xpose functions.
}
\author{Niclas Jonsson, Andrew Hooker & Justin Wilkins}
\keyword{methods}
