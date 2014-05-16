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

\name{npc.coverage}
\alias{npc.coverage}
\title{Function to plot the coverage of the Numerical
  Predictive Check}
\description{
  This function takes the output from the \code{npc} command in Perl
  Speaks NONMEM (PsN) and makes a coverage plot.  A coverage plot for
  the NPC looks at different prediction intervals (PIs) for each data
  point and calculates the total number of data points in the data set
  lying outside of these 
  PIs.  The plot shows the relative amount of data points outside of
  their PI compared to the expected amount at that PI.  In addition a
  confidence interval around these values are computed based on the
  simulated data.
}
\usage{
npc.coverage(npc.info="npc_results.csv",
             main = "Default",
             main.sub=NULL,
             main.sub.cex=0.85,
             ...)
}
\arguments{
  \item{npc.info}{The results file from the \code{npc} command in
    PsN. for example \file{npc\_results.csv}, or if the file is in a
    separate directory \file{./npc\_dir1/npc\_results.csv}.}
  \item{main}{A string giving the plot title or \code{NULL} if
    none. \code{"Default"} creates a default title.}
  \item{main.sub}{Used for names above each plot when using multiple
    plots. Should be a vector \code{c("Group 1","Group 2")}}
  \item{main.sub.cex}{The size of the \code{main.sub} titles.}
  \item{\dots}{Other arguments passed to 
    \code{\link{xpose.multiple.plot.default}}, 
    \code{\link[lattice]{xyplot}} and others. Please see these
    functions for more descriptions of what you can do.
  }
}

\section{Additional arguments for the NPC coverage plots}{
  
  
  \strong{Additional plot features}
  
  \describe{
    \item{\code{CI = "both", "area" or "lines"}}{Specifies whether
      confidence intervals (as lines, a shaded area or both) 
      should be added to the plot. \code{NULL} means no CI.}
    \item{\code{mark.outside.data = TRUE or FALSE }}{Should the points
      outside the CI be marked in a different color to identify them.}
    \item{\code{abline = TRUE} }{Should there be a line to mark the
      value of y=1? Possible values are \code{TRUE}, \code{FALSE}
      and \code{NULL}.}
  }
 
  
  \strong{Line and area control. See
    \code{\link[graphics]{plot}}, \code{\link[grid]{grid.polygon}}
    and  \code{\link[lattice]{xyplot}} for more details.}
    
  \describe{
    \item{\code{CI.area.col = "blue"}}{What color should the area for
      the CI be? Defaults to "blue".}
    \item{\code{CI.area.alpha = 0.3}}{How much transparency should the
      \code{CI.area.col} have? Defaults to 0.3.}
    \item{\code{ab.lwd=1}}{The width of the abline.}
    \item{\code{ab.lty="dashed"}}{How should the abline look?}
    \item{\code{CI.upper.lty="dotted"} }{What should the line at the
      upper edge of the CI look like when using \code{CI = "both" or
	"lines"}?}
    \item{\code{CI.upper.col="brown"}}{What color should the line at the
      upper edge of the CI have when using \code{CI = "both" or
	"lines"}?}
    \item{\code{CI.upper.lwd="2" }}{The line width of the line at the
      upper edge of the CI when using \code{CI = "both" or
	"lines"}?}
    \item{\code{CI.lower.lty="dotted"} }{What should the line at the
      lower edge of the CI look like when using \code{CI = "both" or
	"lines"}?}
    \item{\code{CI.lower.col="brown"}}{What color should the line at the
      lower edge of the CI have when using \code{CI = "both" or
	"lines"}?}
    \item{\code{CI.lower.lwd="2" }}{The line width of the line at the
      lower edge of the CI when using \code{CI = "both" or
	"lines"}?}
    \item{\code{obs.col="black"}}{The color of the observed values.}
    \item{\code{obs.pch=19}}{The type of point to use for the observed
      values.}
    \item{\code{obs.lty="solid"}}{The type of line to use for the observed
      values.}
    \item{\code{obs.type="b"}}{The combination of lines and points
      to use for the observed
      values.}
    \item{\code{obs.cex=1}}{The size of the points to use for the observed
      values.}
    \item{\code{obs.lwd=1}}{The line width to use for the observed
      values.}
    \item{\code{out.col="red"}}{The color of the observed values
      that lie outside of the CI. Only used if \code{mark.outside.data = TRUE}.}
    \item{\code{out.pch=8}}{The type of point to use for the observed values
      that lie outside of the CI. Only used if \code{mark.outside.data = TRUE}.}
    \item{\code{out.cex=1.3}}{The size of the points of the observed values
      that lie outside of the CI. Only used if \code{mark.outside.data = TRUE}.}
    \item{\code{out.lwd=1}}{The line width of the observed values
      that lie outside of the CI. Only used if \code{mark.outside.data = TRUE}.}
  }
}


\value{
  A list of plots
}

\author{Andrew Hooker}

\seealso{
  \code{\link{read.npc.vpc.results}}
  \code{\link{xpose.multiple.plot.default}}
  \code{\link[lattice]{xyplot}}
}
\examples{
\dontrun{
library(xpose4)

npc.coverage()

## to read files in a directory different than the current working directory 
npc.file <- "./another_directory/npc_results.csv"
npc.coverage(npc.info=npc.file)
}
}
\keyword{methods}

