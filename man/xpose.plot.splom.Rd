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

\name{xpose.plot.splom}
\alias{xpose.plot.splom}
\title{The Xpose 4 generic functions for scatterplot matrices. }
\description{
This function is a wrapper for the lattice splom function.
}
\usage{
xpose.plot.splom(plist, object,
           varnames=NULL,
           main = "Scatterplot Matrix",
           xlb = NULL,
           ylb = NULL,
           scales = list(),
           onlyfirst=TRUE,
           inclZeroWRES=FALSE,
           subset = xsubset(object),
           by=object@Prefs@Graph.prefs$condvar,
           force.by.factor=FALSE,
           include.cat.vars = FALSE,
           ordby     = NULL,
           byordfun  = object@Prefs@Graph.prefs$byordfun,
           shingnum  = object@Prefs@Graph.prefs$shingnum,
           shingol   = object@Prefs@Graph.prefs$shingol,
           strip = function(...)
           strip.default(...,strip.names=c(TRUE,TRUE)),
           groups = NULL,
           ids = object@Prefs@Graph.prefs$ids,
           smooth       = TRUE,
           lmline = NULL,
           panel        = xpose.panel.splom,
           aspect = object@Prefs@Graph.prefs$aspect,
           samp=NULL,
           max.plots.per.page=4,
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
  \item{plist}{A vector of strings containing variable names for the scatterplot 
    matrix.}
  \item{object}{An "xpose.data" object.}
  \item{varnames}{A vector of strings containing labels for the variables in the
    scatterplot matrix.}
  \item{inclZeroWRES}{A logical value indicating whether rows with WRES=0
  should be plotted.}
  \item{onlyfirst}{A logical value indicating whether only the first row
    per individual should be included in the plot. }
  \item{panel}{The name of the panel function to use.}
  \item{lmline}{logical variable specifying whether a linear regression line should
    be superimposed over an \code{\link[lattice]{xyplot}}. \code{NULL} ~ FALSE. (\code{y~x})}
  \item{smooth}{ A \code{NULL} value indicates that no superposed line
    should be added to the graph. If \code{TRUE} then a smooth of the data will
    be superimposed.}
  \item{groups}{A string with the name of any grouping variable (used as
    the groups argument to \code{panel.xyplot}.}
  \item{ids}{A logical value indicating whether text labels should be
    used as plotting symbols (the variable used for these symbols
    indicated by the \code{idlab} xpose data variable).}
  \item{aspect}{The aspect ratio of the display (see \code{\link[lattice]{xyplot}}).}
  \item{by}{A string or a vector of strings with the name(s) of the
    conditioning variables.}
  \item{force.by.factor}{Logical value. If TRUE, and \code{by} is not \code{NULL},
    the variable specified by \code{by} is taken as categorical.}
  \item{include.cat.vars}{Logical value.}
  \item{ordby}{A string with the name of a variable to be used to
    reorder any factor conditioning variables (\code{by}). The variable
    is used in a call to the \code{reorder.factor} function. }
  \item{byordfun}{The name of the function to be used when reordering a
    factor conditioning variable (see argument \code{ordby})}
  \item{shingnum}{The number of shingles ("parts") a continuous
    conditioning variable should be divided into.}
  \item{shingol}{The amount of overlap between adjacent shingles (see
    argument \code{shingnum}) }
  \item{strip}{The name of the function to be used as the strip argument
  to the \code{\link[lattice]{xyplot}}.}
  %\item{par.strip.text}{A list of graphical parameters used to control
  %  the apparence of the text in the strips. See \code{\link[lattice]{strip.default}}.}
  \item{main}{A string giving the plot title or \code{NULL} if none.}
  \item{xlb}{A string giving the label for the x-axis. \code{NULL} if none.}
  \item{ylb}{A string giving the label for the y-axis. \code{NULL} if none.}
  \item{subset}{A string giving the subset expression to be applied to
    the data before plotting. See \code{\link{xsubset}}.}
  \item{scales}{A list to be used for the \code{scales} argument in
    \code{xyplot}.}
  \item{mirror}{Should we create mirror plots from simulation data?
    Value can be \code{FALSE}, \code{TRUE} or \code{1} for one mirror
    plot, or \code{3} for three mirror plots.}
  \item{max.plots.per.page}{The maximum number of plots per page that
    can be created with the mirror plots.}
  \item{mirror.aspect}{The aspect ratio of the plots used for mirror
    functionality.}
  \item{samp}{An integer between 1 and object@Nsim
    (see\code{\link{xpose.data-class}}) specifying which of the
    simulated data sets to extract from SData.}
  \item{pass.plot.list}{Should we pass the list of plots created with
    mirror or should we print them directly.  Values can be
    \code{TRUE/FALSE}.}
  \item{x.cex}{The size of the x-axis label.}
  \item{y.cex}{The size of the y-axis label.}
  \item{main.cex}{The size of the title.}
  \item{mirror.internal}{an internal mirror argument used in
    \code{\link{create.mirror}}.  Checks if the
    \code{strip} argument from \code{\link[lattice]{qqmath}} has been used.}
  \item{\dots}{Other arguments passed to \code{\link{xpose.panel.default}}.}
}
\details{
  If \code{ids} is \code{TRUE}, text labels are added to the plotting
  symbols. The labels are taken from the \code{idlab} xpose data
  variable. The way the text labels are plotted is governed by the
  \code{idsmode} argument (passed down to the panel
  function). \code{idsmode=NULL} (the default) means that only extreme
  data points are labelled while a non-\code{NULL} value adds labels to
  all data points (the default in Xpose 3). \code{xpose.panel.default}
  identifies extreme data points by fitting a loess smooth (\code{y~x})
  and looking at the residuals from that fit. Points that are associated
  with the highest/lowest residuals are labelled. "High" and "low" are
  judged by the panel function parameter \code{idsext}, which gives the
  fraction of the total number of data points that are to be judged
  extreme in the "up" and "down" direction. The default value for
  \code{idsext} is 0.05 (see \code{link{xpose.prefs-class}}). There is
  also a possibility to label only the high or low extreme points. This
  is done through the \code{idsdir} argument to
  \code{xpose.panel.default}. A value of "both" (the default) means that
  both high and low extreme points are labelled while "up" and "down"
  labels the high and low extreme points respectively.
  
  More graphical parameters may be passed to
  \code{\link{xpose.panel.splom}}.
  for example, if you want to adjust the size of the \code{varnames} and
  \code{axis tick labels} you can use the parameters
  \code{varname.cex=0.5} and \code{axis.text.cex=0.5}.

}
\value{
  Returns a scatterplot matrix graph object.
}
\author{E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins}
\seealso{
  \code{\link{xpose.panel.splom}},
  \code{\link[lattice]{splom}},
  \code{\link[lattice]{panel.splom}},
  \code{\link{xpose.prefs-class}},
  \code{\link{xpose.data-class}}
}
\examples{
\dontrun{
## xpdb5 is an Xpose data object
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)

## CL, WT, HT, SEX with a regression line
xpose.plot.splom(c("CL", "WT", "HT", "SEX"), xpdb5, lmline = TRUE) 
}

}
\keyword{methods}
