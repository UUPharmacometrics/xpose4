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

\name{xpose.plot.bw}
\alias{xpose.plot.bw}
\title{The generic Xpose functions for box-and-whisker plots}
\description{
  This is a wrapper function for the lattice \code{\link[lattice]{bwplot}} function.
}
\usage{
xpose.plot.bw(x,y,object,
           inclZeroWRES = FALSE,
           onlyfirst    = FALSE,
           samp         = NULL,
           panel        = xpose.panel.bw,
           groups       = NULL,
           ids          = FALSE,
           logy         = FALSE,
           logx         = FALSE,
           aspect       = object@Prefs@Graph.prefs$aspect,
           funy          = NULL,
           funx         = NULL,

           ## Prediction interval settings
           PI           = FALSE,
           
           ## Conditioning settings
           by =  object@Prefs@Graph.prefs$condvar,
           force.by.factor = FALSE,
           ordby     = object@Prefs@Graph.prefs$ordby,
           byordfun  = object@Prefs@Graph.prefs$byordfun,
           shingnum  = object@Prefs@Graph.prefs$shingnum,
           shingol   = object@Prefs@Graph.prefs$shingol,
           strip = function(...)
           strip.default(...,strip.names=c(TRUE,TRUE)),

           ## Subset stuff
           subset       = xsubset(object),

           ## Axes and titles
           main         = xpose.create.title(x,y,object,subset,funx,funy,...),
           xlb          = xpose.create.label(x,object,funx,logx,...),
           ylb          = xpose.create.label(y,object,funy,logy,...),
           scales       = list(),

           ## Superpose smooth
           suline       = object@Prefs@Graph.prefs$suline,

           ## bins
           binvar       = NULL,
           bins         = 10,

           ## mirror stuff
           mirror       = FALSE,
           max.plots.per.page=4,
           mirror.aspect="fill",
           pass.plot.list=FALSE,
           x.cex=NULL,
           y.cex=NULL,
           main.cex=NULL,
           mirror.internal=list(strip.missing=missing(strip)),
           ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Name(s) of the x-variable.}
  \item{y}{Name(s) of the y-variable. }
  \item{object}{An xpose.data object. }
  \item{inclZeroWRES}{A logical value indicating whether rows with WRES=0
  should be plotted.}
  \item{onlyfirst}{A logical value indicating whether only the first row
    per individual should be included in the plot. }
  \item{samp}{An integer between 1 and object@Nsim
    (see\code{\link{xpose.data-class}}) specifying which of the
    simulated data sets to extract from SData.}
  \item{panel}{The name of the panel function to use. This should in most cases
    be left as \code{\link{xpose.panel.bw}}.}
  \item{groups}{A string with the name of any grouping variable (used as
    the groups argument to \code{\link[lattice]{panel.xyplot}}.}
  \item{ids}{A logical value indicating whether text labels should be
    used as plotting symbols (the variable used for these symbols
    indicated by the \code{idlab} Xpose data variable).}
  \item{logy}{Logical value indicating whether the y-axis should be logarithmic.}
  \item{logx}{Logical value indicating whether the x-axis should be logarithmic.}
  \item{aspect}{The aspect ratio of the display (see \code{\link[lattice]{bwplot}}).}
  \item{funy}{String with the name of a function to apply to the
    y-variable before plotting, e.g. "abs". }
  \item{funx}{String with the name of a function to apply to the
    x-variable before plotting, e.g. "abs". }
  \item{PI}{Either "lines", "area" or "both" specifying whether
    prediction intervals (as lines, as a shaded area or both) 
    should be computed from the data in \code{SData} and added to the
    display. \code{NULL} means no prediction interval.}
  \item{by}{A string or a vector of strings with the name(s) of the
    conditioning variables.}
  \item{force.by.factor}{Logical value. If TRUE, and \code{by} is not \code{NULL},
    the variable specified by \code{by} is taken as categorical.}
  \item{ordby}{A string with the name of a variable to be used to
    reorder any factor conditioning variables (\code{by}). The variable
    is used in a call to the \code{reorder} function. }
  \item{byordfun}{The name of the function to be used when reordering a
    factor conditioning variable (see argument \code{ordby}).}
  \item{shingnum}{The number of shingles ("parts") a continuous
    conditioning variable should be divided into.}
  \item{shingol}{The amount of overlap between adjacent shingles (see
    argument \code{shingnum}) }
  \item{strip}{The name of the function to be used as the strip argument
  to the \code{\link[lattice]{bwplot}}.}
  %\item{par.strip.text}{A list of graphical parameters used to control
  %  the apparence of the text in the strips. See \code{\link[lattice]{strip.default}}.}
  \item{main}{A string giving the plot title or \code{NULL} if none.}
  \item{xlb}{A string giving the label for the x-axis. \code{NULL} if none.}
  \item{ylb}{A string giving the label for the y-axis. \code{NULL} if none.}
  \item{subset}{A string giving the subset expression to be applied to
    the data before plotting. See \code{\link{xsubset}}.}
  \item{scales}{A list to be used for the \code{scales} argument in \code{bwplot}.}
  \item{suline}{A string giving the variable to be used to construct a
    smooth to superpose on the display. \code{NULL} if none. This
    argument is used if you want to add a superpose line of a variable
    not present in the \code{y} list of variables.}
  %\item{dilution}{Logical value indicating whether data dilution should
  %  be used.}
  %\item{diltype}{Indicating what type of dilution to apply. \code{NULL}
  %  means random dilution without stratification. A non\code{NULL} value
  %  means stratified dilution.}
  %\item{dilfrac}{Dilution fraction indicating the expected fraction of
  %  individuals to display in the plots. The exact meaning depends on
  %  the type of dilution (see below).}
  %\item{dilci}{A number between 0 and 1 giving the range elegible for
  %  dilution in a stratified dilution (see below). }
  %\item{seed}{Seed number used for random dilution. \code{NULL} means no
  %  seed.}
  \item{binvar}{Variable to be used for binning.}
  \item{bins}{The number of bins to be used. The default is 10.}
  \item{mirror}{Should we create mirror plots from simulation data?
    Value can be \code{FALSE}, \code{TRUE} or \code{1} for one mirror
    plot, or \code{3} for three mirror plots.}
  \item{max.plots.per.page}{The maximum number of plots per page that
    can be created with the mirror plots.}
  \item{mirror.aspect}{The aspect ratio of the plots used for mirror
    functionality.}
  \item{pass.plot.list}{Should we pass the list of plots created with
    mirror or should we print them directly.  Values can be
    \code{TRUE/FALSE}.}
  \item{x.cex}{The size of the x-axis label.}
  \item{y.cex}{The size of the y-axis label.}
  \item{main.cex}{The size of the title.}
  \item{mirror.internal}{an internal mirror argument used in
    \code{\link{create.mirror}}.  Checks if the
    \code{strip} argument from \code{\link[lattice]{bwplot}} has been used.}
  \item{\dots}{Other arguments passed to
    \code{\link{xpose.panel.bw}}.}
}
\author{E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins}
\seealso{\code{\link{xpose.data-class}}, Cross-references above.}
\examples{
\dontrun{
## xpdb5 is an Xpose data object
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)

## Box & whisker plot of WRES vs PRED
xpose.plot.bw("WRES", "PRED", xpdb5, binvar="PRED")
}
}
\keyword{methods}
