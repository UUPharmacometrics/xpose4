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

\name{ind.plots.wres.qq}
\alias{ind.plots.wres.qq}
\alias{ind.plots.cwres.qq}
\title{Quantile-quantile plots of weighted residuals for each 
  individual in an Xpose data object, for Xpose 4}
\description{
  This is a compound plot consisting of QQ plots of the distribution of
  weighted residuals (any weighted residual produced by NONMEM) for
  every individual in the dataset.
  The function is a wrapper encapsulating arguments to the 
  \code{\link{xpose.plot.qq}} function. 
}
\usage{
ind.plots.wres.qq(object,
           main = "Default",
           wres="wres",
           layout=c(4,4),
           inclZeroWRES=FALSE,
           subset=xsubset(object),
           scales=list(cex=0.7,tck=0.5),
           aspect="fill",
           force.by.factor=TRUE,
           ids=F,
           as.table=TRUE,
           type="o",
           pch=object@Prefs@Graph.prefs$pch,
           col=object@Prefs@Graph.prefs$col,
           cex=object@Prefs@Graph.prefs$cex,
           abllty = object@Prefs@Graph.prefs$abllty,
           abllwd = object@Prefs@Graph.prefs$abllwd,
           ablcol = object@Prefs@Graph.prefs$ablcol,
           prompt = FALSE,
           main.cex=0.9,
           mirror=NULL,
           max.plots.per.page=1,
           ...)

ind.plots.cwres.qq(object,
           wres="cwres",
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An xpose.data object. }
  \item{main}{The title of the plot.  If \code{"Default"} then a default
    title is plotted. Otherwise the value should be a string like
    \code{"my title"} or \code{NULL} for no plot title.  For
    \code{"Default"} the function
    \code{\link{xpose.multiple.plot.title}} is used.}
  \item{wres}{Which weighted residual should we plot? Defaults to the WRES.}
  \item{layout}{A list giving the layout of the graphs on the plot, in
    columns and rows. The default is 4x4.}
  \item{inclZeroWRES}{Logical value indicating whether rows with WRES=0
    is included in the plot. The default is FALSE.}
  \item{subset}{A string giving the subset expression to be applied to
    the data before plotting. See \code{\link{xsubset}}.}

  \item{scales}{See \code{\link{xpose.plot.qq}}.}
  \item{aspect}{See \code{\link{xpose.plot.qq}}.}
  \item{force.by.factor}{See \code{\link{xpose.plot.qq}}.}
  \item{ids}{See \code{\link{xpose.plot.qq}}.}
  \item{as.table}{See \code{\link{xpose.plot.qq}}.}
  
  \item{type}{1-character string giving the type of plot desired.  The
    following values are possible, for details, see 'plot': '"p"'
    for points, '"l"' for lines, '"o"' for overplotted points and
    lines, '"b"', '"c"') for (empty if '"c"') points joined by
    lines, '"s"' and '"S"' for stair steps and '"h"' for
    histogram-like vertical lines.  Finally, '"n"' does not
   produce any points or lines.}
  \item{col}{The color for lines and points. Specified as an integer or 
    a text string. A full list is obtained by the R command 
    \code{colours()}. The default is blue (col=4).}
  \item{pch}{The plotting character, or symbol, to use. Specified as an 
   integer. See R help on \code{\link{points}}. The default is an open circle.}
  \item{cex}{The amount by which plotting text and symbols should be scaled 
    relative to the default. 'NULL' and 'NA' are equivalent to '1.0'.}
  \item{abllwd}{Line width of the line of identity.}
  \item{abllty}{Line type of the line of identity.}
  \item{ablcol}{Line colour of the line of identity.}
  \item{prompt}{Specifies whether or not the user should be prompted to press RETURN
    between plot pages. Default is FALSE.}
  \item{mirror}{Mirror plots are not yet implemented in this function
    and this argument must contain a value of \code{NULL}}
  \item{main.cex}{The size of the title.}
  \item{max.plots.per.page}{Maximum number of plots per page}
  \item{\dots}{Other arguments passed to \code{link{xpose.plot.qq}}.}
}
\details{
  Matrices of QQ plots of weighted residuals in each included 
  individual are displayed. 
  
  A wide array of extra options controlling QQ plots are 
  available. See \code{\link{xpose.plot.qq}} for details.
}
\value{
  Returns a compound plot comprising QQ plots of weighted residuals
  conditioned on individual.
}
\author{E. Niclas Jonsson, Mats Karlsson, Justin Wilkins & Andrew Hooker}
\seealso{
  \code{\link{xpose.plot.qq}},
  \code{\link{xpose.panel.qq}},
  \code{\link{qqplot}},
  \code{\link[lattice]{qqmath}},
  \code{\link{xpose.prefs-class}},
  \code{\link{xpose.data-class}}
}
\examples{
\dontrun{
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)

## Here we load the example xpose database 
data(simpraz.xpdb)
xpdb <- simpraz.xpdb

## A vanilla plot
ind.plots.wres.qq(xpdb)

## Custom colours 
ind.plots.wres.qq(xpdb, hicol=5, hidcol=2)

## with a grid
ind.plots.wres.qq(xpdb,grid=TRUE)

## Subset on ID value
ind.plots.wres.qq(xpdb,grid=TRUE,subset="ID<10")

## Use CWRES instead
ind.plots.wres.qq(xpdb,grid=TRUE,subset="ID<10",wres="cwres")
ind.plots.cwres.qq(xpdb,grid=TRUE,subset="ID<10")

## Use NPDEs instead
ind.plots.wres.qq(xpdb,grid=TRUE,subset="ID<10",wres="NPDE")
}
}
\keyword{methods}
