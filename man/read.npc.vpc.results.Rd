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

\name{read.npc.vpc.results}
\alias{read.npc.vpc.results}
\title{Read the results file from a Numerical or Visual Predictive Check
run in PsN}
\description{ This function reads in a results file from running either the PsN command
\code{vpc} or \code{npc}.  The function then parses the file and passes
the result to plotting functions.
}
\usage{
read.npc.vpc.results(vpc.results = NULL, npc.results = NULL, 
                     verbose = FALSE, ...)
}
\arguments{
  \item{vpc.results}{The name of the results file from running the PsN
    command \code{vcp}.  Often this is named \file{vpc\_results.csv}.
    If the file is in a directory different then the
    working directory then you can define a relative or absolute path
    to the file by, for example, \file{./vpc\_strat\_WT\_4\_mirror\_5/vpc\_results.csv}.}
  \item{npc.results}{The name of the results file from running the PsN
    command \code{npc}.  Often this is named \file{npc\_results.csv}.
    relative or absolute paths to the file are allowed
    as for \code{vpc.results}.}
  \item{verbose}{Text messages passed to screen or not.}
  \item{\dots}{arguments passed to other functions.}
}
\details{
One of \code{vpc.results} or \code{npc.results} are necessary.  If both
or none are defined then the function does nothing and a \code{NULL} is
returned from the function.
}
\value{
  A list of values is returned.
  \item{model.file }{The model file that PsN ran either the \code{npc}
    or \code{vpc} with}
  \item{dv.var }{The dependent variable used in the calculations.}
  \item{idv.var }{The independent variable used in the
    calculations. \code{NULL} if \code{npc.results} is used.}
  \item{num.tables }{The number of separate tables in the results file.}
  \item{by.interval }{The conditioning interval for the stratification
    variable, only returned if \code{vpc.results} is used.}
  \item{result.tables }{The results tables from the results file.  this
  is a list.}
}
\author{Andrew Hooker}
\seealso{
  \code{\link{xpose.VPC}}
  \code{\link{npc.coverage}}
}
\keyword{methods}

