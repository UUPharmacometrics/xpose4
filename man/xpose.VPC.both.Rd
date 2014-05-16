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
\name{xpose.VPC.both}
\alias{xpose.VPC.both}
\title{
Xpose Visual Predictive Check (VPC) for both continuous and Limit of
Quantification data.
}
\description{
Xpose Visual Predictive Check (VPC) for both continuous and Below or Above Limit of
Quantification (BLQ or ALQ) data. 
}
\usage{
xpose.VPC.both(vpc.info="vpc_results.csv",  
           vpctab = dir(pattern="^vpctab")[1],
           object = NULL,
           subset=NULL,
           main="Default",
           main.sub=NULL,
           inclZeroWRES=FALSE,
           cont.logy=F,
           hline="default",
           add.args.cont=list(),
           add.args.cat=list(),
           ...)

}
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
  \item{inclZeroWRES}{
Include WRES=0 rows in the computations for these plots? 
}
  \item{cont.logy}{
Sould the continuous plot y-axis be on the log scale?
}
  \item{hline}{
Howizontal line marking the limits of quantification.  If they are
defined, they must be a vector of values.
}
\item{add.args.cont}{
Additional arguments to the continuous plot. \code{\link{xpose.VPC}}.  
}
  \item{add.args.cat}{
Additional arguments to the categorical plot. \code{\link{xpose.VPC.categorical}}.  
}
  \item{\dots}{
Additional arguments to both plots.
}
}
\author{
Andrew C. Hooker
}


\seealso{
  \code{\link{xpose.VPC}},
  \code{\link{xpose.VPC.categorical}}.  
}
\examples{
\dontrun{
library(xpose4)

## move to the directory where results from PsN
## are found
cur.dir <- getwd()
setwd(paste(cur.dir,"/vpc_cont_LLOQ/",sep=""))

xpose.VPC()
xpose.VPC.categorical(censored=T)

xpose.VPC.both()

xpose.VPC.both(subset="DV>1.75")

xpose.VPC.both(add.args.cont=list(ylim=c(0,80)))

xpose.VPC.both(add.args.cont = list(ylim = c(0.01, 80)), xlim = c(0, 
    40), add.args.cat = list(ylim = c(0, 0.4)), cont.logy = T)

xpose.VPC.both(cont.logy=T)
}
}

\keyword{ methods }

