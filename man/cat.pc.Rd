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
\name{cat.pc}
\alias{cat.pc}

\title{
Categorical (visual) predictive check.
}
\description{
Categorical (visual) predictive check plots. 
}
\usage{
cat.pc(object,
           dv=xvardef("dv",object),
           idv=xvardef("idv",object),
           level.to.plot=NULL,
           subset=NULL,
           histo=T,
           median.line=F,
           PI.lines=F,
           xlb=if(histo){
             paste("Proportion of ",dv)
           } else {
             paste(idv)
           },
           ylb=if(histo){
             paste("Percent of Total")
           } else {
             paste("Proportion of Total")
           },
           main=xpose.create.title.text(NULL,dv,
             "Predictive check of",object,subset=subset,...),
           strip="Default",
           ...)
}

\arguments{
  \item{object}{
Xpose data object.
}
  \item{dv}{
The dependent variable (e.g. \code{"DV"} or \code{"CP"}.)
}
  \item{idv}{
The indenpent variable (e.g. \code{"TIME"}.)
}
\item{level.to.plot}{
  The levels to plot.
}
  \item{subset}{
  Subset of data.
}
\item{histo}{
  If \code{FALSE} then a VPC is created, given that \code{idv} is defined.
}
\item{median.line}{
Make a median line?
}
  \item{PI.lines}{
Make prediction interval lines?
}
  \item{xlb}{
Label for x axis.
}
  \item{ylb}{
label for y axis.
}
  \item{main}{
Main title.
}
  \item{strip}{
Defining how the strips should appear in the conditioning plots.
}
  \item{\dots}{
Extra arguments passed to the function.
}
}

\author{
Andrew C. Hooker
}
\examples{
\dontrun{
## read in table files
runno <- 45
xpdb <- xpose.data(runno)

## create proportion (visual) predictive check
cat.pc(xpdb,idv=NULL)
cat.pc(xpdb,idv="DOSE")
cat.pc(xpdb,idv="DOSE",histo=F)
cat.pc(xpdb,idv="TIME",histo=T,level.to.plot=1)
}
}
\keyword{ methods }
