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

\name{xpose.VPC}
\alias{xpose.VPC}
\title{Visual Predictive Check (VPC) using XPOSE}
\description{
  This Function is used to create a VPC in xpose using the output from
  the  \code{vpc} command in Pearl Speaks NONMEM (PsN).
  The function reads in the output files created by PsN and creates a
  plot from the data.  The dependent variable, independent variable and
  conditioning variable are
  automatically determined from the PsN files.
}
\usage{
xpose.VPC(vpc.info = "vpc_results.csv",  
          vpctab = dir(pattern="^vpctab")[1],
          object = NULL,
          ids=FALSE,
          type="p",
          by=NULL,
          PI=NULL,
          PI.ci="area",
          PI.real=T,
          PI.ci.med.arcol="red",
          subset=NULL,
          main="Default",
          main.sub=NULL,
          main.sub.cex=0.85,
          inclZeroWRES=FALSE,
          force.x.continuous=FALSE,
          funy=NULL,
          logy=FALSE,
          ylb = "Default",
          verbose = FALSE,
          ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vpc.info}{The results file from the \code{vpc} command in
    PsN. for example \file{vpc\_results.csv}, or if the file is in a
    separate directory \file{./vpc\_dir1/vpc\_results.csv}.}
  \item{vpctab}{The \file{vpctab} from the \code{vpc} command in
    PsN.  For example \file{vpctab5}, or if the file is in a
    separate directory \file{./vpc\_dir1/vpctab5}.  Can be \code{NULL}.
    The default looks in the current working directory and takes the
    first file that starts with \file{vpctab} that it finds.  Note that
    this default can result in the wrong files being read if there are
    multiple \file{vpctab} files in the directory.
    One of \code{object} or
    \code{vpctab} is required.  If both are present then the information
    from the \code{vpctab} will over-ride the xpose data
    object \code{object} (i.e. the values from the vpctab will replace any
    matching values in the \code{object\@Data} portion of the xpose data
    object).}
  \item{object}{An xpose data object. Created from
    \code{\link{xpose.data}}.  One of \code{object} or
    \code{vpctab} is required.  If both are present then the information
    from the \code{vpctab} will over-ride the xpose data
    object \code{object} (i.e. the values from the vpctab will replace any
    matching values in the \code{object\@Data} portion of the xpose data
    object).}
  \item{ids}{A logical value indicating whether text ID labels should be
    used as plotting symbols (the variable used for these symbols
    indicated by the \code{idlab} xpose data variable). Can be
    \code{FALSE} or \code{TRUE}.}
  \item{type}{Character string describing the way the points in the plot
    will be displayed. For more details, see
    \code{\link[graphics]{plot}}. Use \code{type="n"} if you don't want
    observations in the plot.}
  \item{by}{A string or a vector of strings with the name(s) of the
    conditioning variables. For example \code{by = c("SEX","WT")}.
    Because the function automatically determines the conditioning
    variable from the PsN input file specified in \code{vpc.info}, the
    \code{by} command can control if separate plots are created for each
    condition (\code{by=NULL}), or if a conditioning plot should be
    created (\code{by="WT"} for example).  If the \code{vpc.info} file
    has a conditioning variable then \code{by} must match that
    variable.  If there is no conditioning variable in \code{vpc.info}
    then the \code{PI} for each conditioned plot will be the \code{PI}
    for the entire data set (not only for the conditioning subset).}
  \item{PI}{Either "lines", "area" or "both" specifying whether
    prediction intervals (as lines, a shaded area or both) 
    should be added to the plot. \code{NULL} means no prediction
    interval.}
  \item{PI.ci}{Plot the prediction
    interval of the simulated data's percentiles for each bin. Values
    can be \code{"both", "area" or "lines"} This
    can be thought of as a prediction interval about the
    \code{PI.real} or a confidence interval about the \code{PI}.
    However, note that with increasing number of simulations the CI
    will not go towards zero because the interval is also
    dependent on the size of the data set.}
  \item{PI.real}{Plot the percentiles of the
    real data in
    the various bins. values can be NULL or TRUE.  Note that for a bin with few actual
    observations the percentiles will be approximate.  For example,
    the 95th percentile of 4 data points will always be the largest of
    the 4 data points.}
  \item{PI.ci.med.arcol}{The color of the median
    \code{PI.ci}.}
  \item{force.x.continuous}{Logical value indicating whether x-values should be
    taken as continuous, even if categorical.}
  \item{funy}{String of function to apply to Y data. For example "abs"}
  \item{logy}{Logical value indicating whether the y-axis should be
    logarithmic, base 10.}
  \item{ylb}{Label for the y-axis}
  \item{subset}{A string giving the subset expression to be applied to
    the data before plotting. See \code{\link{xsubset}}.}
  \item{main}{A string giving the plot title or \code{NULL} if
    none. \code{"Default"} creates a default title.}
    \item{main.sub}{Used for names above each plot when using multiple
    plots. Should be a vector \code{c("Group 1","Group 2")}}
  \item{main.sub.cex}{The size of the \code{main.sub} titles.}
  \item{inclZeroWRES}{Logical value indicating whether rows with WRES=0
    is included in the plot.}
  \item{verbose}{Text messages passed to screen or not.}
  \item{\dots}{Other arguments passed to 
    \code{\link{xpose.panel.default}}, 
    \code{\link{xpose.plot.default}} and others. Please see these
    functions for more descriptions of what you can do.}
}

\section{Additional arguments}{
  
  \strong{Additional graphical elements available in the VPC plots\cr}
  
  \describe{
    \item{\code{PI.real = NULL or TRUE}}{Plot the percentiles of the
      real data in
      the various bins.  Note that for a bin with few actual
      observations the percentiles will be approximate.  For example,
      the 95th percentile of 4 data points will always be the largest of
      the 4 data points.}
    \item{\code{PI.mirror = NULL, TRUE or AN.INTEGER.VALUE}}{Plot the
      percentiles of one simulated data set in each bin. \code{TRUE}
      takes the first mirror from \file{vpc\_results.csv} and
      \code{AN.INTEGER.VALUE} can be \code{1, 2, \dots n} where \code{n}
      is the number of mirror's output in the  \file{vpc\_results.csv} file.}
    \item{\code{PI.ci = "both", "area" or "lines"}}{Plot the confidence
      interval  for the
      simulated data's percentiles for each bin (for each simulated data
      set compute the percentiles for each bin, then, from all of the
      percentiles from all of the simulated datasets compute the 95\% CI
      of these percentiles).  These CIs can be used to asses the
      \code{PI.real} values for model misspecification. Again, as with
      the \code{PI.real}, note that
      with  few observations per
      bin the CIs will be approximate because the percentiles in each
      bin will be approximate. For example,
      the 95th percentile of 4 data points will always be the largest of
      the 4 data points.}
    \item{\code{PI.limits = c(0.025, 0.975)}}{A vector of two values
      that describe the limits of the prediction interval that should be
      displayed.  These limits should be found in the
      \file{vpc\_results.csv} file. These limits are also used
      as the percentages for the \code{PI.real, PI.mirror} and
      \code{PI.ci}.  However, the confidence interval in \code{PI.ci} is
      always the one defined in the \file{vpc\_results.csv} file.}
  }
  
  \strong{Additional options to control the look and feel of the
    \code{PI}.  See See \code{\link[grid]{grid.polygon}} and 
    \code{\link[graphics]{plot}} for more details.\cr}
  
  \describe{
    \item{\code{PI.arcol}}{The color of the \code{PI}
      area}
    \item{\code{PI.up.lty}}{The upper line type. can be
      "dotted" or "dashed", etc.}
    \item{\code{PI.up.type}}{The upper type used for
      plotting.  Defaults to a line.}
    \item{\code{PI.up.col}}{The upper line color}
    \item{\code{PI.up.lwd}}{The upper line width}
    \item{\code{PI.down.lty}}{The lower line type. can be
      "dotted" or "dashed", etc.}
    \item{\code{PI.down.type}}{The lower type used for
      plotting.  Defaults to a line.}
    \item{\code{PI.down.col}}{The lower line color}
    \item{\code{PI.down.lwd}}{The lower line width}
    \item{\code{PI.med.lty}}{The median line type. can be
      "dotted" or "dashed", etc.}
    \item{\code{PI.med.type}}{The median type used for
      plotting.  Defaults to a line.}
    \item{\code{PI.med.col}}{The median line color}
    \item{\code{PI.med.lwd}}{The median line width}
  }
    
  \strong{Additional options to control the look and feel of the
    \code{PI.ci}.  See See \code{\link[grid]{grid.polygon}} and 
    \code{\link[graphics]{plot}} for more details.\cr}
  
  \describe{
    \item{\code{PI.ci.up.arcol}}{The color of the upper
      \code{PI.ci}.}
    \item{\code{PI.ci.med.arcol}}{The color of the median
      \code{PI.ci}.}
    \item{\code{PI.ci.down.arcol}}{The color of the lower
      \code{PI.ci}.}
    \item{\code{PI.ci.up.lty}}{The upper line type. can be
      "dotted" or "dashed", etc.}
    \item{\code{PI.ci.up.type}}{The upper type used for
      plotting.  Defaults to a line.}
    \item{\code{PI.ci.up.col}}{The upper line color}
    \item{\code{PI.ci.up.lwd}}{The upper line width}
    \item{\code{PI.ci.down.lty}}{The lower line type. can be
      "dotted" or "dashed", etc.}
    \item{\code{PI.ci.down.type}}{The lower type used for
      plotting.  Defaults to a line.}
    \item{\code{PI.ci.down.col}}{The lower line color}
    \item{\code{PI.ci.down.lwd}}{The lower line width}
    \item{\code{PI.ci.med.lty}}{The median line type. can be
      "dotted" or "dashed", etc.}
    \item{\code{PI.ci.med.type}}{The median type used for
      plotting.  Defaults to a line.}
    \item{\code{PI.ci.med.col}}{The median line color}
    \item{\code{PI.ci.med.lwd}}{The median line width}
    \item{PI.ci.area.smooth}{Should the "area" for \code{PI.ci} be
      smoothed to match the "lines" argument? Allowed values are
      \code{TRUE/FALSE}. The "area" is set by
      default to show
      the bins used in the \code{PI.ci} computation.  By smoothing,
      information is lost and, in general, the confidence intervals will
      be smaller than they are in reality.}
  }
  
  \strong{Additional options to control the look and feel of the
    \code{PI.real}. See See \code{\link[grid]{grid.polygon}} and 
    \code{\link[graphics]{plot}} for more details.\cr}
  
  \describe{
    \item{\code{PI.real.up.lty}}{The upper line type. can be
      "dotted" or "dashed", etc.}
    \item{\code{PI.real.up.type}}{The upper type used for
      plotting.  Defaults to a line.}
    \item{\code{PI.real.up.col}}{The upper line color}
    \item{\code{PI.real.up.lwd}}{The upper line width}
    \item{\code{PI.real.down.lty}}{The lower line type. can be
      "dotted" or "dashed", etc.}
    \item{\code{PI.real.down.type}}{The lower type used for
      plotting.  Defaults to a line.}
    \item{\code{PI.real.down.col}}{The lower line color}
    \item{\code{PI.real.down.lwd}}{The lower line width}
    \item{\code{PI.real.med.lty}}{The median line type. can be
      "dotted" or "dashed", etc.}
    \item{\code{PI.real.med.type}}{The median type used for
      plotting.  Defaults to a line.}
    \item{\code{PI.real.med.col}}{The median line color}
    \item{\code{PI.real.med.lwd}}{The median line width}
  }
    
  \strong{Additional options to control the look and feel of the
    \code{PI.mirror}. See See 
    \code{\link[graphics]{plot}} for more details.\cr}
  
  \describe{
    \item{\code{PI.mirror.up.lty}}{The upper line type. can be
      "dotted" or "dashed", etc.}
    \item{\code{PI.mirror.up.type}}{The upper type used for
      plotting.  Defaults to a line.}
    \item{\code{PI.mirror.up.col}}{The upper line color}
    \item{\code{PI.mirror.up.lwd}}{The upper line width}
    \item{\code{PI.mirror.down.lty}}{The lower line type. can be
      "dotted" or "dashed", etc.}
    \item{\code{PI.mirror.down.type}}{The lower type used for
      plotting.  Defaults to a line.}
    \item{\code{PI.mirror.down.col}}{The lower line color}
    \item{\code{PI.mirror.down.lwd}}{The lower line width}
    \item{\code{PI.mirror.med.lty}}{The median line type. can be
      "dotted" or "dashed", etc.}
    \item{\code{PI.mirror.med.type}}{The median type used for
      plotting.  Defaults to a line.}
    \item{\code{PI.mirror.med.col}}{The median line color}
    \item{\code{PI.mirror.med.lwd}}{The median line width}
  }
}


\value{
  A plot or a list of plots.
}

\author{ Andrew Hooker }

\seealso{
  \code{\link{read.vpctab}}
  \code{\link{read.npc.vpc.results}}
  \code{\link{xpose.panel.default}} 
  \code{\link{xpose.plot.default}}
}
\examples{
\dontrun{
library(xpose4)

xpose.VPC()

## to be more clear about which files should be read in
vpc.file <- "vpc_results.csv"
vpctab <- "vpctab5"
xpose.VPC(vpc.info=vpc.file,vpctab=vpctab)

## with lines and a shaded area for the prediction intervals
xpose.VPC(vpc.file,vpctab=vpctab,PI="both")

## with the percentages of the real data
xpose.VPC(vpc.file,vpctab=vpctab,PI.real=T)

## with mirrors (if supplied in 'vpc.file')
xpose.VPC(vpc.file,vpctab=vpctab,PI.real=T,PI.mirror=5)

## with CIs
xpose.VPC(vpc.file,vpctab=vpctab,PI.real=T,PI.ci="area")
xpose.VPC(vpc.file,vpctab=vpctab,PI.real=T,PI.ci="area",PI=NULL)

## stratification (if 'vpc.file' is stratified)
cond.var <- "WT"
xpose.VPC(vpc.file,vpctab=vpctab)
xpose.VPC(vpc.file,vpctab=vpctab,by=cond.var)
xpose.VPC(vpctab=vpctab,vpc.info=vpc.file,PI="both",by=cond.var,type="n")

## with no data points in the plot
xpose.VPC(vpc.file,vpctab=vpctab,by=cond.var,PI.real=T,PI.ci="area",PI=NULL,type="n")

## with different DV and IDV, just read in new files and plot
vpc.file <- "vpc_results.csv"
vpctab <- "vpctab5"
cond.var <- "WT"
xpose.VPC(vpctab=vpctab,vpc.info=vpc.file,PI="both",by=cond.var)
xpose.VPC(vpctab=vpctab,vpc.info=vpc.file,PI="both")

## to use an xpose data object instead of vpctab
##
## In this example
## we expect to find the required NONMEM run and table files for run
## 5 in the current working directory
runnumber <- 5
xpdb <- xpose.data(runnumber)
xpose.VPC(vpc.file,object=xpdb)

## to read files in a directory different than the current working directory 
vpc.file <- "./vpc_strat_WT_4_mirror_5/vpc_results.csv"
vpctab <- "./vpc_strat_WT_4_mirror_5/vpctab5"
xpose.VPC(vpc.info=vpc.file,vpctab=vpctab)

## to rearrange order of factors in VPC plot
xpdb@Data$SEX <- factor(xpdb@Data$SEX,levels=c("2","1"))
xpose.VPC(by="SEX",object=xpdb)

}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
