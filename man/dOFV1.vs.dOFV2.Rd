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
\name{dOFV1.vs.dOFV2}
\alias{dOFV1.vs.dOFV2}

\title{
Change in individual objective function value 1 vs. individual objective
function value 2.
}
\description{
Change in individual objective function value 1 vs. individual objective
}
\usage{
dOFV1.vs.dOFV2(xpdb1,
             xpdb2,
             xpdb3,
             ylb=expression(paste(Delta, OFV1[i])),
             xlb=expression(paste(Delta, OFV2[i])),
             main="Default",
             smooth=NULL,
             abline=c(0,1),
             ablcol="grey",
             abllwd=2,
             abllty="dashed",
             lmline=TRUE,
             ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xpdb1}{
Xpose data object for first NONMEM run
}
  \item{xpdb2}{
Xpose data object for second NONMEM run
}
  \item{xpdb3}{
Xpose data object for third NONMEM run
}
  \item{ylb}{
Label for Y axis.
}
  \item{xlb}{
Label for X axis.
}
  \item{main}{
Title of plot.
}
  \item{smooth}{
Should we have a smooth?
}
  \item{abline}{
abline description.
}
  \item{ablcol}{
color of abline
}
  \item{abllwd}{
line width of abline
}
  \item{abllty}{
type of abline
}
  \item{lmline}{
Linear regression line?
}
  \item{\dots}{
Additional arguments to function.
}
}
\author{
Andrew C. Hooker
}
\examples{
\dontrun{
## read in table files
xpdb8 <- xpose.data(8)
xpdb8 <- xpose.data(9)
xpdb11 <- xpose.data(11)

## Make the plot
dOFV.vs.cov(xpdb8,xpdb9,xpdb11)
}
}
\keyword{ methods }
