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

\name{read.vpctab}
\alias{read.vpctab}
\title{Read the vpctab file from PsN into Xpose}
\description{
  This function read in the vpctab file created from PsN and gathers the
  information needed to make a vpc plot.
}
\usage{
read.vpctab(vpctab=NULL,
            object=NULL,
            vpc.name="vpctab",
            vpc.suffix="",
            tab.suffix="",
            inclZeroWRES=FALSE,
            verbose = FALSE,
            ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vpctab}{The vpctab file from a '\code{vpc}' run in PsN.}
  \item{object}{An xpose data object. Created from
    \code{\link{xpose.data}}.  One of \code{object} or
    \code{vpctab} is required.  If both are present then the information
    from the \code{vpctab} will over-ride the xpose data
    object \code{object} (i.e. the values from the vpctab will replace any
    matching values in the \code{object@Data} portion of the xpose data
    object). If only \code{object} is present then the function will
    look for a vpctab with the same run number as the one associated
    with the object.}
  \item{vpc.name}{The default name of the vpctab file. Used if only
    \code{object} is supplied.}
  \item{vpc.suffix}{The suffix of the vpctab file. Used if only
    \code{object} is supplied.}
  \item{tab.suffix}{ The table suffix of the vpctab file. Used if only
    \code{object} is supplied. Final order of the file would be then
    \code{paste(vpc.name,object@Runno,vpc.suffix,tab.suffix)}}
  \item{inclZeroWRES}{If there are no zero valued weighted resuiduals in
    the \code{object} then this should be \code{TRUE}.}
  \item{verbose}{Text messages passed to screen or not.}
  \item{\dots}{Other arguments passed to other functions.}
}
\value{
  Returned is an xpose data object with vpctab information included.
}
\author{Andrew Hooker}
\seealso{\code{\link{xpose.VPC}}}
\keyword{methods}

