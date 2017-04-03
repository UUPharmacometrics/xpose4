# Xpose 4
# An R-based population pharmacokinetic/
# pharmacodynamic model building aid for NONMEM.
# Copyright (C) 1998-2004 E. Niclas Jonsson and Mats Karlsson.
# Copyright (C) 2005-2008 Andrew C. Hooker, Justin J. Wilkins, 
# Mats O. Karlsson and E. Niclas Jonsson.
# Copyright (C) 2009-2010 Andrew C. Hooker, Mats O. Karlsson and 
# E. Niclas Jonsson.

# This file is a part of Xpose 4.
# Xpose 4 is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation, either version 3
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.

# You should have received a copy of the GNU Lesser General Public License
# along with this program.  A copy can be cound in the R installation
# directory under \share\licenses. If not, see http://www.gnu.org/licenses/.



#' Default box-and-whisker panel function for Xpose 4
#' 
#' This is the box-and-whisker panel function for Xpose 4. This is not intended
#' to be used outside the \code{xpose.plot.bw} function.  Most of the arguments
#' take their default values from xpose.data object but this can be overridden
#' by supplying them as arguments to \code{xpose.plot.bw}.
#' 
#' 
#' @param x Name(s) of the x-variable.
#' @param y Name(s) of the y-variable.
#' @param object An xpose.data object.
#' @param subscripts The standard Trellis subscripts argument (see
#' \code{\link[lattice]{xyplot}}).
#' @param groups Name of the variable used for superpose plots.
#' @param inclZeroWRES Logical value indicating whether rows with WRES=0 is
#' included in the plot.
#' @param onlyfirst Logical value indicating whether only the first row per
#' individual is included in the plot.
#' @param samp An integer between 1 and object@Nsim
#' (see\code{\link{xpose.data-class}}) specifying which of the simulated data
#' sets to extract from SData.
#' @param xvarnam Character string with the name of the x-variable.
#' @param yvarnam Character string with the name of the y-variable.
#' @param type Character value indicating the type of display to use:
#' "l"=lines, "p"=points, "b"=both points and lines.
#' @param col Colour of lines and plot symbols.
#' @param pch Plot character to use.
#' @param cex Size of the plot characters.
#' @param lty Line type.
#' @param fill Fill colour.
#' @param ids Character value with the name of the variable to label data
#' points with.
#' @param idsmode Determines the way text labels are added to plots.
#' \code{NULL} means that only extreme points are labelled. Non-\code{NULL}
#' means all data points are labelled. (See \code{link{xpose.plot.default}})
#' @param idsext See \code{link{xpose.plot.bw}}
#' @param idscex Size of text labels.
#' @param idsdir A value of "both" (the default) means that both high and low
#' extreme points are labelled while "up" and "down" labels the high and low
#' extreme points respectively. See \code{\link{xpose.plot.bw}}
#' @param bwhoriz logical value indicating whether box and whiskers should be
#' horizontal or not. The default is FALSE.
#' @param bwratio Ratio of box height to inter-box space. The default is 1.5.
#' An argument for \code{\link[lattice]{panel.bwplot}}.
#' @param bwvarwid Logical. If TRUE, widths of boxplots are proportional to the
#' number of points used in creating it. The default is FALSE. An argument for
#' \code{\link[lattice]{panel.bwplot}}.
#' @param bwdotpch Graphical parameter controlling the dot plotting character
#' 'bwdotpch="|"' is treated specially, by replacing the dot with a line. The
#' default is 16. An argument for \code{\link[lattice]{panel.bwplot}}.
#' @param bwdotcol Graphical parameter controlling the dot colour - an integer
#' or string. See 'col'. The default is black. An argument for
#' \code{\link[lattice]{panel.bwplot}}.
#' @param bwdotcex The amount by which plotting text and symbols should be
#' scaled relative to the default. 'NULL' and 'NA' are equivalent to '1.0'. An
#' argument for \code{\link[lattice]{panel.bwplot}}.
#' @param bwreccol The colour to use for the box rectangle - an integer or
#' string.  The default is blue. See \code{\link[lattice]{trellis.par.get}} and
#' "box.rectangle".
#' @param bwrecfill The colour to use for filling the box rectangle - an
#' integer or string. The default is transparent (none). See
#' \code{\link[lattice]{trellis.par.get}} and "box.rectangle".
#' @param bwreclty The line type for the box rectangle - an integer or string.
#' The default is solid. See \code{\link[lattice]{trellis.par.get}} and
#' "box.rectangle".
#' @param bwreclwd The width of the lines for the box rectangle - an integer.
#' The default is 1. See \code{\link[lattice]{trellis.par.get}} and
#' "box.rectangle".
#' @param bwumbcol The colour to use for the umbrellas - an integer or string.
#' The default is blue. See \code{\link[lattice]{trellis.par.get}} and
#' "box.umbrella".
#' @param bwumblty The line type for the umbrellas - an integer or string. The
#' default is solid.See \code{\link[lattice]{trellis.par.get}} and
#' "box.umbrella".
#' @param bwumblwd the width of the lines for the umbrellas - an integer. The
#' default is 1. See \code{\link[lattice]{trellis.par.get}} and "box.umbrella".
#' @param bwoutcol The colour to use for the outliers - an integer or string.
#' The default is blue. See \code{\link[lattice]{trellis.par.get}} and
#' "box.symbol".
#' @param bwoutcex The amount by which outlier points should be scaled relative
#' to the default. 'NULL' and 'NA' are equivalent to '1.0'. The default is 0.8.
#' See \code{\link[lattice]{trellis.par.get}} and "box.symbol".
#' @param bwoutpch The plotting character, or symbol, to use for outlier
#' points.  Specified as an integer. See R help on 'points'. The default is an
#' open circle. See \code{\link[lattice]{trellis.par.get}} and "box.symbol".
#' @param grid logical value indicating whether a visual reference grid should
#' be added to the graph. (Could use arguments for line type, color etc).
#' @param logy Logical value indicating whether the y-axis should be
#' logarithmic.
#' @param logx Logical value indicating whether the x-axis should be
#' logarithmic.
#' @param force.x.continuous Logical value indicating whether x-values should
#' be taken as continuous, even if categorical.
#' @param binvar Variable to be used for binning.
#' @param bins The number of bins to be used. The default is 10.
#' @param \dots Other arguments that may be needed in the function.
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.data-class}}, Cross-references above.
#' @keywords methods
#' @export xpose.panel.bw
"xpose.panel.bw" <-
function(x, y, object,
           subscripts,
           groups = NULL,
           inclZeroWRES = FALSE,
           onlyfirst = FALSE,
           samp = NULL,
           
           xvarnam = NULL,
           yvarnam = NULL,
           
           ## Basic plot characteristics
           type = object@Prefs@Graph.prefs$type,
           col  = object@Prefs@Graph.prefs$col,
           pch  = object@Prefs@Graph.prefs$pch,
           cex  = object@Prefs@Graph.prefs$cex,
           lty  = object@Prefs@Graph.prefs$lty,
           fill = object@Prefs@Graph.prefs$col,
           
           ## Text label setting
           ids  = NULL,
           idsmode=object@Prefs@Graph.prefs$idsmode,
           idsext =object@Prefs@Graph.prefs$idsext,
           idscex= object@Prefs@Graph.prefs$idscex,
           idsdir= object@Prefs@Graph.prefs$idsdir,
           
           ## BW settings
           bwhoriz=object@Prefs@Graph.prefs$bwhoriz,
           bwratio=object@Prefs@Graph.prefs$bwratio,
           bwvarwid=object@Prefs@Graph.prefs$bwvarwid,
           bwdotpch= object@Prefs@Graph.prefs$bwdotpch,
           bwdotcol= object@Prefs@Graph.prefs$bwdotcol,
           bwdotcex=object@Prefs@Graph.prefs$bwdotcex,
           bwreccol =object@Prefs@Graph.prefs$bwreccol,
           bwrecfill= object@Prefs@Graph.prefs$bwrecfill,
           bwreclty= object@Prefs@Graph.prefs$bwreclty,
           bwreclwd=object@Prefs@Graph.prefs$bwreclwd,
           bwumbcol =object@Prefs@Graph.prefs$bwumbcol,
           bwumblty= object@Prefs@Graph.prefs$bwumblty,
           bwumblwd= object@Prefs@Graph.prefs$bwumblwd,
           bwoutcol =object@Prefs@Graph.prefs$bwoutcol,
           bwoutcex= object@Prefs@Graph.prefs$bwoutcex,
           bwoutpch= object@Prefs@Graph.prefs$bwoutpch,

           ## Layout parameters
           grid = object@Prefs@Graph.prefs$grid,
           logy = FALSE,
           logx = FALSE,

           ## Force x variables to be continuous
           force.x.continuous = TRUE,
           
           ## bins
           binvar = NULL,
           bins   = 10,
           #xvar   = NULL,
           ...

           ) {
    #cat(x,"\n")
    #cat(str(x))
    
    
    if(!is.null(samp)) {
      data <- SData(object,inclZeroWRES,onlyfirst=onlyfirst,samp=samp)
    } else {
      data <- Data(object,inclZeroWRES,onlyfirst=onlyfirst)
    }
    
    ## if lengths disagree, re-read x
    if (length(x) != nrow(data)) {
    #cat(length(x))
    #cat(nrow(data))
      for (i in 1:length(names(data))) {
         if (names(data)[i] == xvarnam) {
         #if (names(data)[i] == binvar) {
          x <- as.vector(as.matrix(data[i]))
        } 
      }
    #cat(length(x))
    #cat(nrow(data))
    } 
     
    #cat(str(x)) 
    
    if(force.x.continuous != FALSE) {
      if(length(unique(data[subscripts,xvarnam])) <= object@Prefs@Cat.levels) x <- as.factor(x)
    }

    ## Stuff common to both xy and bw
    if(grid != "F") {
      panel.grid(h = -1, v = -1)
    }

    y.bw <- xpose.bin(data, binvar, bins)
    bwhoriz <- bwhoriz
    ## Plot the data
    if(!is.factor(x) && !bwhoriz) {

    trellis.par.set(list(box.rectangle = list(col = bwreccol, fill = bwrecfill, lty = bwreclty, lwd = bwreclwd)))
    trellis.par.set(list(box.umbrella = list(col = bwumbcol, lty = bwumblty, lwd = bwumblwd)))
    trellis.par.set(list(box.dot = list(col = bwdotcol, cex = bwdotcex, pch = bwdotpch)))
    trellis.par.set(list(plot.symbol = list(col = bwoutcol, cex = bwoutcex, pch = bwoutpch)))
    
    try(
      if(any(is.null(groups))) {

     #cat(length(x))
     #cat(length(xpdb5@Data$TIME[xpdb5@Data$WRES!=0]))
     #cat(length(y.bw))
        panel.bwplot(x, y.bw,
                     col   =bwdotcol,
                     pch   =bwdotpch,
                     lty   =bwreclty,
                     type  =type,
                     cex   = bwdotcex,
                     varwidth = bwvarwid,
                     box.ratio = bwratio,
                     fill = bwrecfill
                     )
      } else {
        ord <- order(x)
        panel.superpose(x[ord],
                        y.bw[ord],
                        subscripts[ord],
                        col   =bwdotcol,
                        pch   =bwdotpch,
                        cex   =bwdotcex,
                        lty   =bwreclty,
                        type  =type,
                        groups=groups,
                        varwidth = bwvarwid,
                        box.ratio = bwratio,
                        fill = bwrecfill
                        )
      }     ) 
    
      if (ids) {
      ## Add id-numbers as plot symbols
      if(!any(is.null(ids))) {
        ids <- ids[subscripts]
        addid(x,y,ids=ids,
              idsmode=idsmode,
              idsext =idsext,
              idscex = idscex,
              idsdir = idsdir)
      }
      }
      
    } 

  }

