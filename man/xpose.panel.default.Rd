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

\name{xpose.panel.default}
\alias{xpose.panel.default}
\title{Default panel function for Xpose 4}
\description{
  This is the panel function for Xpose 4. This is not intended to be
  ised outside the \code{xpose.plot.default} function. Most of the
  arguments take their default values from xpose.data object but this
  can be overridden by supplying them as argument to \code{xpose.plot.default}.
}
\usage{

xpose.panel.default(x, y,object,
           subscripts,
           groups = object@Prefs@Xvardef$id,
           grp.col = NULL,
           iplot = NULL,
           inclZeroWRES = FALSE,
           onlyfirst = FALSE,
           samp = NULL,
                      
           xvarnam = NULL,
           yvarnam = NULL,
           
           ###############################
           ## Prediction interval settings
           ###############################
           PI      = NULL,
           PI.subset=NULL,
           PI.bin.table=NULL,
           PI.real=NULL,  # can be NULL/TRUE
           PI.mirror=NULL,
           PI.ci = NULL,
           PPI = NULL,
           PI.mean = FALSE, # Should the mean be plotted in the VPCs 
           PI.delta.mean = FALSE, # Should the delta.mean be plotted in the VPCs 
           
           PI.limits= c(0.025, 0.975),#object@Prefs@Graph.prefs$PI.limits,

           PI.arcol = "lightgreen",#object@Prefs@Graph.prefs$PI.arcol,

           PI.up.lty = 2,#object@Prefs@Graph.prefs$PI.up.lty,
           PI.up.type = "l",#object@Prefs@Graph.prefs$PI.up.type,
           PI.up.col = "black",#object@Prefs@Graph.prefs$PI.up.col,
           PI.up.lwd = 2,#object@Prefs@Graph.prefs$PI.up.lwd,
           
           PI.down.lty = 2,#object@Prefs@Graph.prefs$PI.down.lty,
           PI.down.type = "l",#object@Prefs@Graph.prefs$PI.down.type,
           PI.down.col = "black",#object@Prefs@Graph.prefs$PI.down.col,
           PI.down.lwd = 2,#object@Prefs@Graph.prefs$PI.down.lwd,
           
           PI.med.lty = 1,#object@Prefs@Graph.prefs$PI.med.lty,
           PI.med.type = "l",#object@Prefs@Graph.prefs$PI.med.type,
           PI.med.col = "black",#object@Prefs@Graph.prefs$PI.med.col,
           PI.med.lwd = 2,#object@Prefs@Graph.prefs$PI.med.lwd,
                  
           PI.mean.lty = 3,#object@Prefs@Graph.prefs$PI.med.lty,
           PI.mean.type = "l",#object@Prefs@Graph.prefs$PI.med.type,
           PI.mean.col = "black",#object@Prefs@Graph.prefs$PI.med.col,
           PI.mean.lwd = 2,#object@Prefs@Graph.prefs$PI.med.lwd,

           PI.delta.mean.lty = 3,#object@Prefs@Graph.prefs$PI.med.lty,
           PI.delta.mean.type = "l",#object@Prefs@Graph.prefs$PI.med.type,
           PI.delta.mean.col = "black",#object@Prefs@Graph.prefs$PI.med.col,
           PI.delta.mean.lwd = 2,#object@Prefs@Graph.prefs$PI.med.lwd,
                          
           PI.real.up.lty = 2,#object@Prefs@Graph.prefs$PI.real.up.lty,
           PI.real.up.type = "l",#object@Prefs@Graph.prefs$PI.real.up.type,
           PI.real.up.col = "red",#object@Prefs@Graph.prefs$PI.real.up.col,
           PI.real.up.lwd = 2,#object@Prefs@Graph.prefs$PI.real.up.lwd,
           
           PI.real.down.lty = 2,#object@Prefs@Graph.prefs$PI.real.down.lty,
           PI.real.down.type = "l",#object@Prefs@Graph.prefs$PI.real.down.type,
           PI.real.down.col = "red",#object@Prefs@Graph.prefs$PI.real.down.col,
           PI.real.down.lwd = 2,#object@Prefs@Graph.prefs$PI.real.down.lwd,
           
           PI.real.med.lty = 1,#object@Prefs@Graph.prefs$PI.real.med.lty,
           PI.real.med.type = "l",#object@Prefs@Graph.prefs$PI.real.med.type,
           PI.real.med.col = "red",#object@Prefs@Graph.prefs$PI.real.med.col,
           PI.real.med.lwd = 2,#object@Prefs@Graph.prefs$PI.real.med.lwd,

           PI.real.mean.lty = 3,#object@Prefs@Graph.prefs$PI.real.med.lty,
           PI.real.mean.type = "l",#object@Prefs@Graph.prefs$PI.real.med.type,
           PI.real.mean.col = "red",#object@Prefs@Graph.prefs$PI.real.med.col,
           PI.real.mean.lwd = 2,#object@Prefs@Graph.prefs$PI.real.med.lwd,

           PI.real.delta.mean.lty = 3,#object@Prefs@Graph.prefs$PI.real.med.lty,
           PI.real.delta.mean.type = "l",#object@Prefs@Graph.prefs$PI.real.med.type,
           PI.real.delta.mean.col = "red",#object@Prefs@Graph.prefs$PI.real.med.col,
           PI.real.delta.mean.lwd = 2,#object@Prefs@Graph.prefs$PI.real.med.lwd,

           PI.mirror.up.lty = 2,#object@Prefs@Graph.prefs$PI.mirror.up.lty,
           PI.mirror.up.type = "l",#object@Prefs@Graph.prefs$PI.mirror.up.type,
           PI.mirror.up.col = "darkgreen",#object@Prefs@Graph.prefs$PI.mirror.up.col,
           PI.mirror.up.lwd = 1,#object@Prefs@Graph.prefs$PI.mirror.up.lwd,
           
           PI.mirror.down.lty = 2,#object@Prefs@Graph.prefs$PI.mirror.down.lty,
           PI.mirror.down.type = "l",#object@Prefs@Graph.prefs$PI.mirror.down.type,
           PI.mirror.down.col = "darkgreen",#object@Prefs@Graph.prefs$PI.mirror.down.col,
           PI.mirror.down.lwd = 1,#object@Prefs@Graph.prefs$PI.mirror.down.lwd,
           
           PI.mirror.med.lty = 1,#object@Prefs@Graph.prefs$PI.mirror.med.lty,
           PI.mirror.med.type = "l",#object@Prefs@Graph.prefs$PI.mirror.med.type,
           PI.mirror.med.col = "darkgreen",#object@Prefs@Graph.prefs$PI.mirror.med.col,
           PI.mirror.med.lwd = 1,#object@Prefs@Graph.prefs$PI.mirror.med.lwd,

           PI.mirror.mean.lty = 3,#object@Prefs@Graph.prefs$PI.mirror.med.lty,
           PI.mirror.mean.type = "l",#object@Prefs@Graph.prefs$PI.mirror.med.type,
           PI.mirror.mean.col = "darkgreen",#object@Prefs@Graph.prefs$PI.mirror.med.col,
           PI.mirror.mean.lwd = 1,#object@Prefs@Graph.prefs$PI.mirror.med.lwd,

           PI.mirror.delta.mean.lty = 3,#object@Prefs@Graph.prefs$PI.mirror.med.lty,
           PI.mirror.delta.mean.type = "l",#object@Prefs@Graph.prefs$PI.mirror.med.type,
           PI.mirror.delta.mean.col = "darkgreen",
           PI.mirror.delta.mean.lwd = 1,#object@Prefs@Graph.prefs$PI.mirror.med.lwd,

           PI.ci.up.arcol = "blue",
           PI.ci.up.lty = 3,#object@Prefs@Graph.prefs$PIuplty,
           PI.ci.up.type = "l",#object@Prefs@Graph.prefs$PIuptyp,
           PI.ci.up.col = "darkorange",#object@Prefs@Graph.prefs$PI.up.col,
           PI.ci.up.lwd = 2,#object@Prefs@Graph.prefs$PI.up.lwd,
           
           PI.ci.down.arcol = "blue",
           PI.ci.down.lty = 3,#object@Prefs@Graph.prefs$PIdolty,
           PI.ci.down.type = "l",#object@Prefs@Graph.prefs$PIdotyp,
           PI.ci.down.col = "darkorange",#object@Prefs@Graph.prefs$PI.down.col,
           PI.ci.down.lwd = 2,#object@Prefs@Graph.prefs$PI.down.lwd,
           
           PI.ci.med.arcol = "blue",
           PI.ci.med.lty = 4,#object@Prefs@Graph.prefs$PImelty,
           PI.ci.med.type = "l",#object@Prefs@Graph.prefs$PImetyp,
           PI.ci.med.col = "darkorange",#object@Prefs@Graph.prefs$PI.med.col,
           PI.ci.med.lwd = 2,#object@Prefs@Graph.prefs$PI.med.lwd,
           
           PI.ci.mean.arcol = "purple",
           PI.ci.mean.lty = 4,#object@Prefs@Graph.prefs$PImelty,
           PI.ci.mean.type = "l",#object@Prefs@Graph.prefs$PImetyp,
           PI.ci.mean.col = "darkorange",#object@Prefs@Graph.prefs$PI.med.col,
           PI.ci.mean.lwd = 2,#object@Prefs@Graph.prefs$PI.med.lwd,

           PI.ci.delta.mean.arcol = "purple",
           PI.ci.delta.mean.lty = 4,#object@Prefs@Graph.prefs$PImelty,
           PI.ci.delta.mean.type = "l",#object@Prefs@Graph.prefs$PImetyp,
           PI.ci.delta.mean.col = "darkorange",#object@Prefs@Graph.prefs$PI.med.col,
           PI.ci.delta.mean.lwd = 2,#object@Prefs@Graph.prefs$PI.med.lwd,

           PI.ci.area.smooth=FALSE,
           ###############################
           ## end of PI settings
           ###############################


           ## Basic plot characteristics
           type = object@Prefs@Graph.prefs$type,
           col  = object@Prefs@Graph.prefs$col,
           pch  = object@Prefs@Graph.prefs$pch,
           cex  = object@Prefs@Graph.prefs$cex,
           lty  = object@Prefs@Graph.prefs$lty,
           lwd  = object@Prefs@Graph.prefs$lwd,
           fill = object@Prefs@Graph.prefs$fill,
           
           ## Text label setting
           ids  = NULL,
           idsmode=object@Prefs@Graph.prefs$idsmode,
           idsext =object@Prefs@Graph.prefs$idsext,
           idscex= object@Prefs@Graph.prefs$idscex,
           idsdir= object@Prefs@Graph.prefs$idsdir,

           ## abline settings
           abline= object@Prefs@Graph.prefs$abline,
           abllwd= object@Prefs@Graph.prefs$abllwd,
           abllty= object@Prefs@Graph.prefs$abllty,
           ablcol= object@Prefs@Graph.prefs$ablcol,

           smooth= object@Prefs@Graph.prefs$smooth, 
           smlwd = object@Prefs@Graph.prefs$smlwd, 
           smlty = object@Prefs@Graph.prefs$smlty, 
           smcol = object@Prefs@Graph.prefs$smcol, 
           smspan= object@Prefs@Graph.prefs$smspan,
           smdegr= object@Prefs@Graph.prefs$smdegr,
           smooth.for.groups=NULL,
           
           lmline= object@Prefs@Graph.prefs$lmline,
           lmlwd = object@Prefs@Graph.prefs$lmlwd ,
           lmlty = object@Prefs@Graph.prefs$lmlty ,
           lmcol = object@Prefs@Graph.prefs$lmcol ,

           suline = object@Prefs@Graph.prefs$suline,
           sulwd  = object@Prefs@Graph.prefs$sulwd ,
           sulty  = object@Prefs@Graph.prefs$sulty ,
           sucol  = object@Prefs@Graph.prefs$sucol ,
           suspan = object@Prefs@Graph.prefs$suspan,
           sudegr = object@Prefs@Graph.prefs$sudegr,

           ## Layout parameters
           grid = object@Prefs@Graph.prefs$grid,
           logy = FALSE,
           logx = FALSE,

           ## Force x variables to be continuous
           force.x.continuous = FALSE,
           
           ## Categorcal x-variable
           bwhoriz  = object@Prefs@Graph.prefs$bwhoriz,
           bwratio  = object@Prefs@Graph.prefs$bwratio,
           bwvarwid = object@Prefs@Graph.prefs$bwvarwid,
           bwdotpch = object@Prefs@Graph.prefs$bwdotpch,
           bwdotcol = object@Prefs@Graph.prefs$bwdotcol,
           bwdotcex = object@Prefs@Graph.prefs$bwdotcex,
           bwreccol = object@Prefs@Graph.prefs$bwreccol,
           bwrecfill= object@Prefs@Graph.prefs$bwrecfill,
           bwreclty = object@Prefs@Graph.prefs$bwreclty,
           bwreclwd = object@Prefs@Graph.prefs$bwreclwd,
           bwumbcol = object@Prefs@Graph.prefs$bwumbcol,
           bwumblty = object@Prefs@Graph.prefs$bwumblty,
           bwumblwd = object@Prefs@Graph.prefs$bwumblwd,
           bwoutcol = object@Prefs@Graph.prefs$bwoutcol,
           bwoutcex = object@Prefs@Graph.prefs$bwoutcex,
           bwoutpch = object@Prefs@Graph.prefs$bwoutpch,
           autocorr=FALSE,
           
           ## vline settings
           vline= NULL,#object@Prefs@Graph.prefs$abline,
           vllwd= 3,#object@Prefs@Graph.prefs$abllwd,
           vllty= 2,#object@Prefs@Graph.prefs$abllty,
           vlcol= "grey",#object@Prefs@Graph.prefs$ablcol,
           
           ## hline settings
           hline= NULL,#object@Prefs@Graph.prefs$abline,
           hllwd= 3,#object@Prefs@Graph.prefs$abllwd,
           hllty= 1,#object@Prefs@Graph.prefs$abllty,
           hlcol= "grey",#object@Prefs@Graph.prefs$ablcol,
           
           # ind.plots/group panels with single point per panel/group
           pch.ip.sp=pch, 
           cex.ip.sp=cex, 
           
           ...)

}

\arguments{
  \item{x}{Name(s) of the x-variable.}
  \item{y}{Name(s) of the y-variable. }
  \item{object}{An xpose.data object. }
  \item{subscripts}{The standard Trellis subscripts argument (see
    \code{\link[lattice]{xyplot}})} 
  \item{groups}{Name of the variable used for superpose plots.}
  \item{grp.col}{Logical value indicating whether or not to use colour
    highlighting when groups are specified. NULL means no highlighting, while
    TRUE will identify group members by colour.}
  \item{iplot}{Is this an indvidual plots matrix? Internal use only.}
  \item{inclZeroWRES}{Logical value indicating whether rows with WRES=0
    is included in the plot.}
  \item{onlyfirst}{Logical value indicating whether only the first row
    per individual is included in teh plot. }
  \item{samp}{An integer between 1 and object@Nsim
    (see\code{\link{xpose.data-class}}) specifying which of the
    simulated data sets to extract from SData.}
  \item{xvarnam}{Character string with the name of the x-variable. }
  \item{yvarnam}{Character string with the name of the y-variable.}
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
  \item{lty}{The line type. Line types can either be specified as an
    integer (0=blank, 1=solid, 2=dashed, 3=dotted, 4=dotdash,
    5=longdash, 6=twodash) or as one of the character strings
    '"blank"', '"solid"', '"dashed"', '"dotted"', '"dotdash"',
    '"longdash"', or '"twodash"', where '"blank"' uses 'invisible
    lines' (i.e., doesn't draw them).}
  \item{lwd}{the width for lines. Specified as an integer. The default is
    1.}
  \item{fill}{fill for areas in plot}  
  \item{ids}{Logical value specifying whether to label data points.}
  \item{idsmode}{Determines the way text labels are added to
    plots. \code{NULL} means that only extreme points are
    labelled. Non-\code{NULL} means all data points are labelled. (See
    \code{link{xpose.plot.default}})}
  \item{idsext}{specifies the extent of the extremes to be used in labelling 
    points. The default is 0.05 (only the most extreme 5\% of points are
    labelled).}
  \item{idscex}{the amount by which labels should be scaled relative to the 
    default. 'NULL' and 'NA' are equivalent to '1.0'.}
  \item{idsdir}{a string indicating the directions of the extremes to include in 
    labelling. Possible values are "up", "down" and "both".}
  \item{abline}{Vector of arguments to the \code{\link[lattice]{panel.abline}}
    function. No abline is drawn if \code{NULL}. }
  \item{abllwd}{Line width of any abline.}
  \item{abllty}{Line type of any abline.}
  \item{ablcol}{Line colour of any abline.}
  \item{lmline}{logical variable specifying whether a linear regression line should
    be superimposed over an \code{\link[lattice]{xyplot}}. \code{NULL} ~ FALSE. (\code{y~x})}
  \item{lmlwd}{ Line width of the lmline. }
  \item{lmlty}{ Line type of the lmline. }
  \item{lmcol}{ Line colour of the lmline. }
  \item{smooth}{ A \code{NULL} value indicates that no superposed line
    should be added to the graph. If \code{TRUE} then a smooth of the data will
    be superimposed.}
  \item{smlwd}{Line width of the x-y smooth.}
  \item{smlty}{Line type of the x-y smooth.}
  \item{smcol}{Line color of the x-y smooth.}
  \item{smspan}{The smoothness parameter for the x-y smooth. The default is 0.667. An argument 
    to \code{\link[lattice]{panel.loess}}.}
  \item{smdegr}{The degree of the polynomials to be used for the x-y smooth, up to 2. The default
    is 1. An argument to \code{\link[lattice]{panel.loess}}.}
  \item{smooth.for.groups}{Should a smooth for each group be drawn?}
  \item{suline}{ A \code{NULL} value indicates that no superposed line
    should be added to the graph. If non-\code{NULL} then this should be
    the vector (the same length as y) of data points to be used for the smoothed
    superposed line.}
  \item{sulwd}{Line width of the superposed smooth.}
  \item{sulty}{Line type of the superposed smooth.}
  \item{sucol}{Line color of the superposed smooth.}
  \item{suspan}{The smoothness parameter. The default is 0.667. An argument 
    to \code{\link[lattice]{panel.loess}}.}
  \item{sudegr}{The degree of the polynomials to be used, up to 2. The default
    is 1. An argument to \code{\link[lattice]{panel.loess}}.}
  \item{grid}{logical value indicating whether a visual reference grid
    should be added to the graph. (Could use arguments for line type,
    color etc).}
  \item{logy}{Logical value indicating whether the y-axis should be logarithmic.}
  \item{logx}{Logical value indicating whether the y-axis should be logarithmic.}
  \item{force.x.continuous}{Logical value indicating whether x-values should be
    taken as continuous, even if categorical.}
  \item{bwhoriz}{logical value indicating whether box and whiskers should be 
    horizontal or not. The default is FALSE.}
  \item{bwratio}{Ratio of box height to inter-box space. The default is 1.5.
    An argument for \code{\link[lattice]{panel.bwplot}}.}
  \item{bwvarwid}{Logical. If TRUE, widths of boxplots are proportional to the
    number of points used in creating it. The default is FALSE. An argument 
    for \code{\link[lattice]{panel.bwplot}}.}
  \item{bwdotpch}{Graphical parameter controlling the dot plotting character
    in boxplots. 'bwdotpch="|"' is treated specially, by replacing 
    the dot with a line. The default is 16. An argument for \code{\link[lattice]{panel.bwplot}}.}
  \item{bwdotcol}{Graphical parameter controlling the dot colour in boxplots - an integer or 
    string. See 'col'. The default is black. An argument for \code{\link[lattice]{panel.bwplot}}.}
  \item{bwdotcex}{The amount by which plotting text and symbols should be scaled 
    relative to the default in boxplots. 'NULL' and 'NA' are equivalent to '1.0'. An argument 
    for \code{\link[lattice]{panel.bwplot}}.}
  \item{bwreccol}{The colour to use for the box rectangle in boxplots - an integer or string. 
    The default is blue. See \code{\link[lattice]{trellis.par.get}} and "box.rectangle".}
  \item{bwrecfill}{The colour to use for filling the box rectangle in boxplots - an integer 
    or string. The default is transparent (none). See \code{\link[lattice]{trellis.par.get}} 
    and "box.rectangle".}
  \item{bwreclty}{The line type for the box rectangle in boxplots - an integer or string. 
    The default is solid. See \code{\link[lattice]{trellis.par.get}} and "box.rectangle".}
  \item{bwreclwd}{The width of the lines for the box rectangle in boxplots - an integer. The 
    default is 1. See \code{\link[lattice]{trellis.par.get}} and "box.rectangle".}
  \item{bwumbcol}{The colour to use for the umbrellas in boxplots - an integer or string. 
    The default is blue. See \code{\link[lattice]{trellis.par.get}} and "box.umbrella".}
  \item{bwumblty}{The line type for the umbrellas in boxplots - an integer or string. The 
    default is solid.See \code{\link[lattice]{trellis.par.get}} and "box.umbrella".}
  \item{bwumblwd}{the width of the lines for the umbrellas in boxplots - an integer. The 
    default is 1. See \code{\link[lattice]{trellis.par.get}} and "box.umbrella".}
  \item{bwoutcol}{The colour to use for the outliers in boxplots - an integer or string. 
    The default is blue. See \code{\link[lattice]{trellis.par.get}} and "box.symbol".}
  \item{bwoutcex}{The amount by which outlier points should be scaled relative 
    to the default in boxplots. 'NULL' and 'NA' are equivalent to '1.0'. The default
    is 0.8. See \code{\link[lattice]{trellis.par.get}} and "box.symbol".}
  \item{bwoutpch}{The plotting character, or symbol, to use for outlier points in boxplots. 
    Specified as an integer. See R help on 'points'. The default is an open 
    circle. See \code{\link[lattice]{trellis.par.get}} and "box.symbol".}
  \item{PI}{Either "lines", "area" or "both" specifying whether
    prediction intervals (as lines, as a shaded area or both) 
    should be computed from the data in \code{SData} and added to the
    display. \code{NULL} means no prediction interval.}
  \item{PI.subset }{The subset to be used for the PI. }
  \item{PI.bin.table}{The table used to create VPC plots.  Has a
    specific format created by \code{\link{read.npc.vpc.results}}}
  \item{PI.real}{Plot the percentiles of the
    real data in
    the various bins. values can be NULL or TRUE.  Note that for a bin with few actual
    observations the percentiles will be approximate.  For example,
    the 95th percentile of 4 data points will always be the largest of
    the 4 data points.}
  \item{PI.mirror}{Plot the
    percentiles of one simulated data set in each bin. values allowed
    are \code{NULL}, \code{TRUE} or
    \code{AN.INTEGER.VALUE}.
    \code{TRUE}
    takes the first mirror from \code{PI.bin.table} and
    \code{AN.INTEGER.VALUE} can be \code{1, 2, \dots n} where \code{n}
    is the number of mirror's output in the  \code{PI.bin.table}.  Used
    mainly by \code{\link{xpose.VPC}}.}
  \item{PI.ci}{Plot the prediction
    interval of the simulated data's percentiles for each bin. Values
    can be \code{"both", "area" or "lines"} This
    can be thought of as a prediction interval about the
    \code{PI.real} or a confidence interval about the \code{PI}.
    However, note that with increasing number of simulations the CI
    will not go towards zero because the interval is also
    dependent on the size of the data set.}
  \item{PPI}{The plot prediction interval. Has a specific format that
  must be followed.  See \code{\link{setup.PPI}}.}
  \item{PI.mean}{Should the mean be plotted in the VPCs? TRUE or FALSE.} 
  \item{PI.delta.mean}{Should the delta mean be plotted in the VPCs? TRUE or FALSE.} 
  \item{PI.limits}{A vector of two values
    that describe the limits of the prediction interval that should be
    displayed.  For example \code{c(0.025, 0.975)}.  These limits should
    be found in the
    \file{PI.bin.table} table. These limits are also used
    as the percentages for the \code{PI.real, PI.mirror} and
    \code{PI.ci}.  However, the confidence interval in \code{PI.ci} is
    always the one defined in the \code{PI.bin.table}.}
  \item{PI.arcol}{The color of the \code{PI}
    area}

  \item{PI.up.lty}{The upper line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.up.type}{The upper type used for
    plotting.  Defaults to a line.}
  \item{PI.up.col}{The upper line color}
  \item{PI.up.lwd}{The upper line width}
  \item{PI.down.lty}{The lower line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.down.type}{The lower type used for
    plotting.  Defaults to a line.}
  \item{PI.down.col}{The lower line color}
  \item{PI.down.lwd}{The lower line width}
  \item{PI.med.lty}{The median line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.med.type}{The median type used for
    plotting.  Defaults to a line.}
  \item{PI.med.col}{The median line color}
  \item{PI.med.lwd}{The median line width}
  
  \item{PI.mean.lty}{The mean line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.mean.type}{The mean type used for
    plotting.  Defaults to a line.}
  \item{PI.mean.col}{The mean line color}
  \item{PI.mean.lwd}{The mean line width}

  \item{PI.delta.mean.lty}{The delta.mean line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.delta.mean.type}{The delta.mean type used for
    plotting.  Defaults to a line.}
  \item{PI.delta.mean.col}{The delta.mean line color}
  \item{PI.delta.mean.lwd}{The delta.mean line width}

  \item{PI.ci.up.arcol}{The color of the upper
    \code{PI.ci}.}
  \item{PI.ci.med.arcol}{The color of the median
    \code{PI.ci}.}
  \item{PI.ci.down.arcol}{The color of the lower
    \code{PI.ci}.}

  \item{PI.ci.up.lty}{The upper line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.ci.up.type}{The upper type used for
    plotting.  Defaults to a line.}
  \item{PI.ci.up.col}{The upper line color}
  \item{PI.ci.up.lwd}{The upper line width}
  \item{PI.ci.down.lty}{The lower line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.ci.down.type}{The lower type used for
    plotting.  Defaults to a line.}
  \item{PI.ci.down.col}{The lower line color}
  \item{PI.ci.down.lwd}{The lower line width}
  \item{PI.ci.med.lty}{The median line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.ci.med.type}{The median type used for
    plotting.  Defaults to a line.}
  \item{PI.ci.med.col}{The median line color}
  \item{PI.ci.med.lwd}{The median line width}
  
  \item{PI.ci.mean.arcol}{The color of the mean
    \code{PI.ci}.}
  \item{PI.ci.mean.lty}{The mean line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.ci.mean.type}{The mean type used for
    plotting.  Defaults to a line.}
  \item{PI.ci.mean.col}{The mean line color}
  \item{PI.ci.mean.lwd}{The mean line width}

  \item{PI.ci.delta.mean.arcol}{The color of the delta.mean
    \code{PI.ci}.}
  \item{PI.ci.delta.mean.lty}{The delta.mean line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.ci.delta.mean.type}{The delta.mean type used for
    plotting.  Defaults to a line.}
  \item{PI.ci.delta.mean.col}{The delta.mean line color}
  \item{PI.ci.delta.mean.lwd}{The delta.mean line width}
  
  \item{PI.real.up.lty}{The upper line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.real.up.type}{The upper type used for
    plotting.  Defaults to a line.}
  \item{PI.real.up.col}{The upper line color}
  \item{PI.real.up.lwd}{The upper line width}
  \item{PI.real.down.lty}{The lower line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.real.down.type}{The lower type used for
    plotting.  Defaults to a line.}
  \item{PI.real.down.col}{The lower line color}
  \item{PI.real.down.lwd}{The lower line width}
  \item{PI.real.med.lty}{The median line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.real.med.type}{The median type used for
    plotting.  Defaults to a line.}
  \item{PI.real.med.col}{The median line color}
  \item{PI.real.med.lwd}{The median line width}

  \item{PI.real.mean.lty}{The mean line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.real.mean.type}{The mean type used for
    plotting.  Defaults to a line.}
  \item{PI.real.mean.col}{The mean line color}
  \item{PI.real.mean.lwd}{The mean line width}

  \item{PI.real.delta.mean.lty}{The delta.mean line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.real.delta.mean.type}{The delta.mean type used for
    plotting.  Defaults to a line.}
  \item{PI.real.delta.mean.col}{The delta.mean line color}
  \item{PI.real.delta.mean.lwd}{The delta.mean line width}

  \item{PI.mirror.up.lty}{The upper line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.mirror.up.type}{The upper type used for
    plotting.  Defaults to a line.}
  \item{PI.mirror.up.col}{The upper line color}
  \item{PI.mirror.up.lwd}{The upper line width}
  \item{PI.mirror.down.lty}{The lower line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.mirror.down.type}{The lower type used for
    plotting.  Defaults to a line.}
  \item{PI.mirror.down.col}{The lower line color}
  \item{PI.mirror.down.lwd}{The lower line width}
  \item{PI.mirror.med.lty}{The median line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.mirror.med.type}{The median type used for
    plotting.  Defaults to a line.}
  \item{PI.mirror.med.col}{The median line color}
  \item{PI.mirror.med.lwd}{The median line width}

  \item{PI.mirror.mean.lty}{The mean line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.mirror.mean.type}{The mean type used for
    plotting.  Defaults to a line.}
  \item{PI.mirror.mean.col}{The mean line color}
  \item{PI.mirror.mean.lwd}{The mean line width}

  \item{PI.mirror.delta.mean.lty}{The delta.mean line type. can be
    "dotted" or "dashed", etc.}
  \item{PI.mirror.delta.mean.type}{The delta.mean type used for
    plotting.  Defaults to a line.}
  \item{PI.mirror.delta.mean.col}{The delta.mean line color}
  \item{PI.mirror.delta.mean.lwd}{The delta.mean line width}

  \item{PI.ci.area.smooth}{Should the "area" for \code{PI.ci} be
    smoothed to match the "lines" argument? Allowed values are
    \code{TRUE/FALSE}. The "area" is set by
    default to show
    the bins used in the \code{PI.ci} computation.  By smoothing,
    information is lost and, in general, the confidence intervals will
    be smaller than they are in reality.}

  \item{autocorr}{Is this an autocorrelation plot?  Values can be
    \code{TRUE/FALSE}.}

  \item{vline}{Add a vertical line to the plot at the values specified.}
  \item{vllwd}{Width (lwd) of vertical line}
  \item{vllty}{Line type (lty) for vertical line}
  \item{vlcol}{Color (col) of vertical line}

  \item{hline}{Add a horizontal line to the plot at the values specified.}
  \item{hllwd}{Width (lwd) of horizontal line}
  \item{hllty}{Line type (lty) for horizontal line}
  \item{hlcol}{Color (col) of horizontal line}
  
  \item{pch.ip.sp}{If there is a panel with just one observation then this specifies the type of 
    points for the DV, IPRED and PRED respectively.}
  \item{cex.ip.sp}{If there is a panel with just one observation then this specifies the size of the 
    points for the DV, IPRED and PRED respectively.} 

  \item{\dots}{Other arguments that may be needed in the function. }
}
\author{E. Niclas Jonsson, Mats Karlsson, Justin Wilkins and Andrew Hooker}
\seealso{\code{xpose.data-class}, Cross-references above.}
\keyword{methods}
