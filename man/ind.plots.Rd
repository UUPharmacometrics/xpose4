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

\name{ind.plots}
\alias{ind.plots}
\title{Observations (DV), individual predictions (IPRED) and population 
  predictions (PRED) are plotted against the independent variable for every 
  individual in the dataset, for Xpose 4}
\description{
  This is a compound plot consisting of plots of observations (DV),
  individual predictions (IPRED) and population predictions (PRED) against
  the independent variable for every individual in the dataset, a specific 
  function in Xpose 4. It is a wrapper encapsulating arguments to the 
  \code{xpose.plot.default} function. 
}
\usage{
ind.plots(object,
           y.vals = c(
             xvardef("dv",new.obj),
             xvardef("ipred",new.obj),
             xvardef("pred",new.obj)
             ),
           x.vals = xvardef("idv",new.obj),
           id.vals = xvardef("id",new.obj),
           key.text = y.vals,
           main = "Default",
           key="Default",
           xlb = xlabel(xvardef("idv",object),object),
           ylb = NULL,
           layout = c(4,4),
           inclZeroWRES = FALSE,
           subset = xsubset(object),
           type = "o",
           grid=FALSE,
           col = c(1,2,4),
           lty = c(0,1,3),
           lwd = c(1,1,1),
           pch = c(21,32,32),
           cex = c(0.7,0.7,0.7),
           fill=c("lightgrey",0,0),
           prompt = FALSE,
           mirror=NULL,
           main.cex=0.9,
           max.plots.per.page=1,
           pch.ip.sp=c(21,19,18),
           cex.ip.sp=c(0.7,0.4,0.4),
           y.vals.subset=NULL,
           ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An xpose.data object. }
  \item{y.vals}{The Y values to use.}
  \item{x.vals}{The X values to use.}
  \item{id.vals}{The ID values to use.}
  \item{key.text}{The text in the legend to use.}
  \item{key}{Create a legend.}
  \item{grid}{Should the plots have a grid in each plot?}
  \item{fill}{Fill the circles in the points?}
  \item{max.plots.per.page}{Maximum number of plots per page.}
  \item{inclZeroWRES}{Logical value indicating whether rows with WRES=0
    is included in the plot. The default is TRUE. }
  \item{main}{The title of the plot.  If \code{"Default"} then a default
    title is plotted. Otherwise the value should be a string like
    \code{"my title"} or \code{NULL} for no plot title.  For
    \code{"Default"} the function
    \code{\link{xpose.multiple.plot.title}} is used.}
  \item{xlb}{A string giving the label for the x-axis. \code{NULL} if none.}
  \item{ylb}{A string giving the label for the y-axis. \code{NULL} if none.}
  \item{layout}{A list giving the layout of the graphs on the plot, in
    columns and rows. The default is 4x4.}
  \item{subset}{A string giving the subset expression to be applied to
    the data before plotting. See \code{\link{xsubset}}.}
  \item{type}{1-character string giving the type of plot desired. The 
    default is "o", for overplotted points and lines. See 
    \code{\link{xpose.plot.default}}.}
  \item{col}{A list of three elements, giving plotting characters for 
    observations, individual predictions, and population predictions, 
    in that order. The default is black for DV, red for
    individual predictions, and blue for
    population predictions.}
  \item{lty}{A list of three elements, giving line types for observations, 
    individual predictions, and population predictions, in that order.}
  \item{lwd}{A list of three elements, giving line widths for observations, 
    individual predictions, and population predictions, in that order.}  
  \item{pch}{A list of three elements, giving plotting characters for 
    observations, individual predictions, and population predictions, 
    in that order.} 
  \item{cex}{A list of three elements, giving relative point size for 
    observations, individual predictions, and population predictions, 
    in that order. The default is c(0.7,0.7,0.7).} 
  \item{prompt}{Specifies whether or not the user should be prompted to press RETURN
    between plot pages. Default is TRUE.}
  \item{mirror}{Mirror plots are not yet implemented in this function
    and this argument must contain a value of \code{NULL}}
  \item{main.cex}{The size of the title.}
  \item{pch.ip.sp}{If there is a panel with just one observation then this specifies the type of 
    points for the DV, IPRED and PRED respectively.}
  \item{cex.ip.sp}{If there is a panel with just one observation then this specifies the size of the 
    points for the DV, IPRED and PRED respectively.} 
  \item{y.vals.subset}{Used to subset on the DV, IPRED and PRED variables separately.  Either \code{NULL} or a 
        vector of three strings, corresponding to the subset of DV, IPRED and PRED respectively. See examples below.}
  \item{\dots}{Other arguments passed to \code{link{xpose.plot.default}}.}
}
\details{
  Matrices of individual plots are presented for comparison and closer 
  inspection.
}
\value{
  Returns a stack of plots observations (DV) against 
  individual predictions (IPRED) and population predictions (PRED).
  
  A wide array of extra options controlling xyplots are 
  available. See \code{\link{xpose.plot.default}} and 
  \code{\link{xpose.panel.default}} for details.
}
\author{E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins}
\seealso{
  \code{\link{xpose.plot.default}},
  \code{\link{xpose.panel.default}},
  \code{\link[lattice]{xyplot}},
  \code{\link[lattice]{strip.default}},
  \code{\link{xpose.prefs-class}},
  \code{\link{xpose.data-class}}
}
\examples{
\dontrun{
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)
}

## Here we load the example xpose database 
data(simpraz.xpdb)
xpdb <- simpraz.xpdb

## A vanilla plot
ind.plots(xpdb)

## Monochrome, suitable for manuscript or report
ind.plots(xpdb, 
  subset="ID>40 & ID<57", 
  col=c(1,1,1), 
  lty=c(0,2,3), 
  strip=function(..., bg) 
    strip.default(..., bg="grey"))
    
\dontrun{    
## IF we simulate in NONMEM using a dense grid of time points
## and all non-observed DV items are equal to zero.
ind.plots(xpdb,inclZeroWRES=TRUE,y.vals.subset=c("DV!=0","NULL","NULL"))


# to plot individual plots of multiple variables
ind.plots(xpdb,subset="FLAG==1")
ind.plots(xpdb,subset="FLAG==2")
}
}
\keyword{methods}
