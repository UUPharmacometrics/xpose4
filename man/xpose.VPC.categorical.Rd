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
\name{xpose.VPC.categorical}
\alias{xpose.VPC.categorical}
\title{
Xpose visual predictive check for categorical data.
}
\description{
Xpose visual predictive check for categorical data (binary, ordered
categorical and count data).
}
\usage{
xpose.VPC.categorical(vpc.info="vpc_results.csv",  
           vpctab = dir(pattern="^vpctab")[1],
           object = NULL,
           subset=NULL,
           main="Default",
           main.sub="Default",  
           main.sub.cex=0.85, 
           real.col=4,
           real.lty="b",
           real.cex=1,
           real.lwd=1,
           median.line=FALSE,
           median.col="darkgrey",
           median.lty=1,
           ci.lines=FALSE,
           ci.col="blue",
           ci.lines.col="darkblue",
           ci.lines.lty=3,
           xlb="Default",
           ylb="Proportion of Total",
           force.x.continuous=FALSE,
           level.to.plot=NULL,
           max.plots.per.page=1,
           rug=TRUE,
           rug.col="orange",
           censored=FALSE,
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vpc.info}{
Name of PSN file to use.  File will come from \code{VPC} command in PsN.
}
  \item{vpctab}{
Name of vpctab file produced from PsN.
}
  \item{object}{
Xpose data object.
}
  \item{subset}{
Subset of data to look at.
}
  \item{main}{
Title for plot.
}
\item{main.sub}{
  Used for names above each plot when using multiple plots. Should be a
vector, e.g. \code{c("title 1","title 2")}.
}
  \item{main.sub.cex}{
Size of \code{main.sub}
}
  \item{real.col}{
Color of real line.
}
  \item{real.lty}{
Real line type.
}
  \item{real.cex}{
Size of real line.
}
  \item{real.lwd}{
Width of real line.
}
  \item{median.line}{
Dray a median line?
}
  \item{median.col}{
Color of median line.
}
  \item{median.lty}{
median line type.
}
  \item{ci.lines}{
Lines marking confidence interval?
}
  \item{ci.col}{
Color of CI area.
}
  \item{ci.lines.col}{
Color of CI lines.
}
  \item{ci.lines.lty}{
Type of CI lines.
}
  \item{xlb}{
X-axis label.  If other than "default"" passed directly to \code{\link{xyplot}}.
}
  \item{ylb}{
Y-axis label. Passed directly to \code{\link{xyplot}}.
}
  \item{force.x.continuous}{
For the x variable to be continuous.
}
  \item{level.to.plot}{
Which levels of the variable to plot. Smallest level is 1, largest is
number_of_levels.  For example, with 4 levels, the largest level would
be 4, the smallest would be 1. 
}
  \item{max.plots.per.page}{
The number of plots per page.
}
  \item{rug}{
Should there be markings on the plot showing where the intervals for the
VPC are?
}
  \item{rug.col}{
Color of the rug.
}
  \item{censored}{
Is this censored data?  Censored data can be both below and above the
limit of quantification.
}
  \item{\dots}{
Additional information passed to function.
}
}
\author{
Andrew C. Hooker
}


\seealso{
  \code{\link{xpose.VPC.both}}.  
}
\examples{
\dontrun{
library(xpose4)

## move to the directory where results from PsN
## are found
cur.dir <- getwd()
setwd(paste(cur.dir,"/binary/vpc_36",sep=""))

xpose.VPC.categorical(level.to.plot=1,max.plots.per.page=4)
xpose.VPC.categorical(level.to.plot=1,max.plots.per.page=4,by="DOSE")

## ordered categorical plots
setwd(paste(cur.dir,"/ordered_cat/vpc_45",sep=""))
xpose.VPC.categorical()


## count
setwd(paste(cur.dir,"/count/vpc65b",sep=""))
xpose.VPC.categorical()

setwd(paste(cur.dir,"/count/vpc65a",sep=""))
xpose.VPC.categorical()

}
}
\keyword{ methods }

