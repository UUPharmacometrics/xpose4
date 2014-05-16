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

\name{internal.functions}
\alias{get.polygon.regions}
\alias{find.right.table}
\alias{setup.PPI}
\title{Internal functions for the VPC}
\description{
 Internal functions for the VPC
}
\usage{
find.right.table(object, inclZeroWRES, onlyfirst, 
                 samp, PI.subset, subscripts, 
                 PI.bin.table, panel.number, ...)
get.polygon.regions(PPI, PI.mirror, ...)
setup.PPI(PIlimits, PI.mirror, tmp.table, ...)
}
\arguments{
  \item{object}{Xpose object}
  \item{inclZeroWRES}{Include row sof data with WRES=0}
  \item{onlyfirst}{Use only first data for each individual}
  \item{samp}{sample number}
  \item{PI.subset}{Prediction interval subset}
  \item{subscripts}{subscripts}
  \item{PI.bin.table}{prediction interval binning table}
  \item{panel.number}{panel number}
  \item{PPI}{Plot prediction intervals}
  \item{PI.mirror}{Prediction interval mirror}
  \item{PIlimits}{Prediction interval limits}
   \item{tmp.table}{temporary table}
  \item{\dots}{Extra options passed to arguments}
}
\value{
  Returned to \code{\link{xpose.VPC}}
}
\keyword{methods}

