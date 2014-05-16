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
\name{cat.dv.vs.idv.sb}
\alias{cat.dv.vs.idv.sb}
\title{
Categorical observations vs. idependent variable using stacked bars.
}
\description{
Categorical observations vs. idependent variable using stacked bars.
}
\usage{
cat.dv.vs.idv.sb(object,
           dv=xvardef("dv",object),
           idv=xvardef("idv",object),
           by=NULL,
           groups=dv,
           force.by.factor = FALSE,
           recur=F,
           xlb=idv,
           ylb="Proportion",
           subset=NULL,
           vary.width=T,
           level.to.plot=NULL,
           refactor.levels=TRUE,
           main=xpose.create.title.text(idv,dv,
             "Proportions of",object,subset=subset,...),
           stack=TRUE,
           horizontal=FALSE,

           strip = function(...)
           strip.default(...,strip.names=c(TRUE,TRUE)),
           scales       = list(),
           inclZeroWRES = TRUE,
           onlyfirst    = FALSE,
           samp         = NULL,
           aspect       = object@Prefs@Graph.prefs$aspect,
           auto.key = "Default",
           
           ## mirror stuff
           mirror       = FALSE,
           mirror.aspect="fill",
           pass.plot.list=FALSE,
           x.cex=NULL,
           y.cex=NULL,
           main.cex=NULL,
           mirror.internal=list(strip.missing=missing(strip)),
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
  \item{by}{
Conditioning variable
}
  \item{groups}{
How we should group values in each conditional plot.
}
  \item{force.by.factor}{
Should we force the data to be treated as factors?
}
\item{recur}{
  Not used.
}
\item{xlb}{A string giving the label for the x-axis. \code{NULL} if
  none.}
\item{ylb}{A string giving the label for the y-axis. \code{NULL} if
  none.}
\item{subset}{
  Subset of data.
}
  \item{vary.width}{
Should we vary the width of the bars to match amount of information?
}
  \item{level.to.plot}{
Which levels of the DV to plot.
}
  \item{refactor.levels}{
Should we refactor the levels?
}
  \item{main}{
The title of the plot.
}
  \item{stack}{
Should we stack the bars?
}
  \item{horizontal}{
Should the bars be horizontal?
}
  \item{strip}{
Defining how the strips should appear in the conditioning plots.
}
  \item{scales}{
Scales argument to \code{\link[lattice]{xyplot}}.
}
  \item{inclZeroWRES}{
Include rows with WRES=0?
}
  \item{onlyfirst}{
Only include first data point for each individual?
}
  \item{samp}{
Sample to use in mirror plot (a number).
}
  \item{aspect}{
Aspect argument to \code{\link[lattice]{xyplot}}.
}
  \item{auto.key}{
Make a legend.
}
  \item{mirror}{
Mirror can be \code{FALSE}, \code{TRUE}, 1 or 3.
}
  \item{mirror.aspect}{
Aspect for mirror.
}
  \item{pass.plot.list}{
Should the plot list be passsed back to user?
}
  \item{x.cex}{
Size of x axis label.
}
  \item{y.cex}{
Size of Y axis label.
}
  \item{main.cex}{
Size of Title.
}
  \item{mirror.internal}{
Internal stuff.
}
  \item{\dots}{
Other arguments passed to function.
}
}



\author{
Andrew Hooker
}

\examples{
\dontrun{
## read in table files
runno <- 45
xpdb <- xpose.data(runno)

## make some stacked bar plots
cat.dv.vs.idv.sb(xpdb,idv=NULL,stack=F)
cat.dv.vs.idv.sb(xpdb,idv=NULL,stack=F,by="DOSE")
cat.dv.vs.idv.sb(xpdb,idv="DOSE")
cat.dv.vs.idv.sb(xpdb,idv=NULL,stack=F,by="TIME")
cat.dv.vs.idv.sb(xpdb,idv="TIME")
cat.dv.vs.idv.sb(xpdb,idv="CAVH")
cat.dv.vs.idv.sb(xpdb,idv="TIME",by="DOSE",scales=list(x=list(rot=45)))

## make some mirror plots
cat.dv.vs.idv.sb(xpdb,idv="DOSE",mirror=1)
cat.dv.vs.idv.sb(xpdb,idv="CAVH",mirror=1,auto.key=F)
}
}
\keyword{ methods }
