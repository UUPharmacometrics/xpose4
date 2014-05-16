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

\name{ind.plots.wres.hist}
\alias{ind.plots.wres.hist}
\alias{ind.plots.cwres.hist}
\title{Histograms of weighted residuals for each individual
  in an Xpose data object, for Xpose 4}
\description{
  This is a compound plot consisting of histograms of the distribution of
  weighted residuals (any weighted residual available from NONMEM) for
  every individual in the dataset. 
  It is a wrapper encapsulating arguments to the 
  \code{\link{xpose.plot.histogram}} function. 
}
\usage{
ind.plots.wres.hist(object,
           main = "Default",
           wres="wres",
           ylb = NULL,
           layout=c(4,4),
           inclZeroWRES=FALSE,
           subset=xsubset(object),
           scales=list(cex=0.7,tck=0.5),
           aspect="fill",
           force.by.factor=TRUE,
           ids=F,
           as.table=TRUE,
           hicol = object@Prefs@Graph.prefs$hicol,
           hilty = object@Prefs@Graph.prefs$hilty,
           hilwd = object@Prefs@Graph.prefs$hilwd,
           hidcol = object@Prefs@Graph.prefs$hidcol,
           hidlty = object@Prefs@Graph.prefs$hidlty,
           hidlwd = object@Prefs@Graph.prefs$hidlwd,
           hiborder = object@Prefs@Graph.prefs$hiborder,
           prompt = FALSE,
           mirror=NULL,
           main.cex=0.9,
           max.plots.per.page=1,
           ...)

ind.plots.cwres.hist(object,
           wres="cwres",
           ...)
}
\arguments{
  \item{object}{An xpose.data object. }
  \item{main}{The title of the plot.  If \code{"Default"} then a default
    title is plotted. Otherwise the value should be a string like
    \code{"my title"} or \code{NULL} for no plot title.  For
    \code{"Default"} the function
    \code{\link{xpose.multiple.plot.title}} is used.}
  \item{wres}{Which weighted residual should we plot? Defaults to the WRES.}
  \item{ylb}{A string giving the label for the y-axis. \code{NULL} if none.}
  \item{layout}{A list giving the layout of the graphs on the plot, in
    columns and rows. The default is 4x4.}
  \item{inclZeroWRES}{Logical value indicating whether rows with WRES=0
    is included in the plot. The default is FALSE. }
  \item{subset}{A string giving the subset expression to be applied to
    the data before plotting. See \code{\link{xsubset}}.}
  \item{scales}{see \code{\link{xpose.plot.histogram}}}
  \item{aspect}{see \code{\link{xpose.plot.histogram}}}
  \item{force.by.factor}{see \code{\link{xpose.plot.histogram}}}
  \item{ids}{see \code{\link{xpose.plot.histogram}}}
  \item{as.table}{see \code{\link{xpose.plot.histogram}}}
  \item{hiborder}{the border colour of the histogram - an integer or string. 
    The default is black (see \code{\link[lattice]{histogram}}).}
  \item{hicol}{the fill colour of the histogram - an integer or string. 
    The default is blue (see \code{\link[lattice]{histogram}}).}
  \item{hilty}{the border line type of the histogram - an integer. 
    The default is 1 (see \code{\link[lattice]{histogram}}).}
  \item{hilwd}{the border line width of the histogram - an integer. 
    The default is 1 (see \code{\link[lattice]{histogram}}).}
  \item{hidcol}{the fill colour of the density line - an integer or string. 
    The default is black (see \code{\link[lattice]{histogram}}).}
  \item{hidlty}{the border line type of the density line - an integer. 
    The default is 1 (see \code{\link[lattice]{histogram}}).}
  \item{hidlwd}{the border line width of the density line - an integer. 
    The default is 1 (see \code{\link[lattice]{histogram}}).}
  \item{prompt}{Specifies whether or not the user should be prompted to press RETURN
    between plot pages. Default is FALSE.}
  \item{mirror}{Mirror plots are not yet implemented in this function
    and this argument must contain a value of \code{NULL}}
  \item{main.cex}{The size of the title.}
  \item{max.plots.per.page}{Maximum number of plots per page}
  \item{\dots}{Other arguments passed to \code{\link{xpose.plot.histogram}}.}
}
\details{
  Matrices of histograms of weighted residuals in each included 
  individual are displayed. \code{ind.plots.cwres.hist} is just a
           wrapper for \code{ind.plots.wres.hist(object,wres="cwres").}
}
\value{
  Returns a compound plot comprising histograms of weighted residual
  conditioned on individual.
}
\author{E. Niclas Jonsson, Mats Karlsson, Justin Wilkins & Andrew Hooker}
\seealso{
  \code{\link{xpose.plot.histogram}},
  \code{\link{xpose.panel.histogram}},
  \code{\link[lattice]{histogram}},
  \code{\link{xpose.prefs-class}},
  \code{\link{xpose.data-class}}
}
\examples{
\dontrun{
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)


## Here we load the example xpose database from the xpose4data package
data(simpraz.xpdb)
xpdb <- simpraz.xpdb

## A vanilla plot
ind.plots.wres.hist(xpdb)

## Different plotting character 
ind.plots.wres.hist(xpdb, pch=3)

## Different plotting character 
ind.plots.wres.hist(xpdb, pch=3)

##with a grid
ind.plots.wres.hist(xpdb, grid=TRUE)

## subsets
ind.plots.wres.hist(xpdb, subset="ID<10")
ind.plots.wres.hist(xpdb, subset="ID<10 | ID>45",grid=TRUE)

## plot the CWRES instead
ind.plots.wres.hist(xpdb, wres="CWRES")
ind.plots.cwres.hist(xpdb)
ind.plots.cwres.hist(xpdb, subset="ID<10 | ID>45",grid=TRUE)
}
}
\keyword{methods}
