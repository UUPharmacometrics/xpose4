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



#' Scatterplot matrix panel function for Xpose 4
#' 
#' This is the scatterplot matrix panel function for Xpose 4. This is not
#' intended to be ised outside the \code{xpose.plot.splom} function. Most of
#' the arguments take their default values from xpose.data object but this can
#' be overridden by supplying them as argument to \code{xpose.plot.splom}.
#' 
#' 
#' @param x Name(s) of the x-variable.
#' @param y Name(s) of the y-variable.
#' @param object An xpose.data object.
#' @param subscripts The standard Trellis subscripts argument (see
#' \code{\link[lattice]{xyplot}})
#' @param groups Name of the variable used for superpose plots.
#' @param inclZeroWRES Logical value indicating whether rows with WRES=0 is
#' included in the plot.
#' @param onlyfirst Logical value indicating whether only the first row per
#' individual is included in teh plot.
#' @param type 1-character string giving the type of plot desired.  The
#' following values are possible, for details, see 'plot': '"p"' for points,
#' '"l"' for lines, '"o"' for overplotted points and lines, '"b"', '"c"') for
#' (empty if '"c"') points joined by lines, '"s"' and '"S"' for stair steps and
#' '"h"' for histogram-like vertical lines.  Finally, '"n"' does not produce
#' any points or lines.
#' @param col The color for lines and points. Specified as an integer or a text
#' string. A full list is obtained by the R command \code{colours()}. The
#' default is blue (col=4).
#' @param pch The plotting character, or symbol, to use. Specified as an
#' integer. See R help on \code{\link{points}}. The default is an open circle.
#' @param cex The amount by which plotting text and symbols should be scaled
#' relative to the default. 'NULL' and 'NA' are equivalent to '1.0'.
#' @param lty The line type. Line types can either be specified as an integer
#' (0=blank, 1=solid, 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash) or
#' as one of the character strings '"blank"', '"solid"', '"dashed"',
#' '"dotted"', '"dotdash"', '"longdash"', or '"twodash"', where '"blank"' uses
#' 'invisible lines' (i.e., doesn't draw them).
#' @param lwd the width for lines. Specified as an integer. The default is 1.
#' @param lmline logical variable specifying whether a linear regression line
#' should be superimposed over an \code{\link[lattice]{xyplot}}. \code{NULL} ~
#' FALSE. (\code{y~x})
#' @param lmlwd Line width of the lmline.
#' @param lmlty Line type of the lmline.
#' @param lmcol Line colour of the lmline.
#' @param smooth A \code{NULL} value indicates that no superposed line should
#' be added to the graph. If \code{TRUE} then a smooth of the data will be
#' superimposed.
#' @param smlwd Line width of the x-y smooth.
#' @param smlty Line type of the x-y smooth.
#' @param smcol Line color of the x-y smooth.
#' @param smspan The smoothness parameter for the x-y smooth. The default is
#' 0.667. An argument to \code{\link[lattice]{panel.loess}}.
#' @param smdegr The degree of the polynomials to be used for the x-y smooth,
#' up to 2. The default is 1. An argument to
#' \code{\link[lattice]{panel.loess}}.
#' @param grid logical value indicating whether a visual reference grid should
#' be added to the graph. (Could use arguments for line type, color etc).
#' @param \dots Other arguments that may be needed in the function.
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.plot.splom}}, \code{\link{xpose.data-class}},
#' \code{\link[lattice]{xyplot}} \code{\link[lattice]{splom}},
#' \code{\link[lattice]{panel.splom}}, \code{\link[lattice]{panel.pairs}}
#' @keywords methods
#' @export xpose.panel.splom
"xpose.panel.splom" <-
  function(x, y, object, subscripts,
           onlyfirst=TRUE,
           inclZeroWRES=FALSE,
           type = "p",
           col  = object@Prefs@Graph.prefs$col,
           pch  = object@Prefs@Graph.prefs$pch,
           cex  = object@Prefs@Graph.prefs$cex,
           lty  = object@Prefs@Graph.prefs$lty,
           lwd  = object@Prefs@Graph.prefs$lwd,
           
           smooth=  TRUE, 
           smlwd = object@Prefs@Graph.prefs$smlwd, 
           smlty = object@Prefs@Graph.prefs$smlty, 
           smcol = object@Prefs@Graph.prefs$smcol, 
           smspan= object@Prefs@Graph.prefs$smspan,
           smdegr= object@Prefs@Graph.prefs$smdegr,
           
           lmline = NULL,
           lmlwd = object@Prefs@Graph.prefs$lmlwd ,
           lmlty = object@Prefs@Graph.prefs$lmlty ,
           lmcol = object@Prefs@Graph.prefs$lmcol ,
           
           grid = object@Prefs@Graph.prefs$grid,
           
           ##scales = list(),
           groups = NULL,
           ... ) {
    
    if(grid != FALSE) {
      panel.grid(h = -1, v = -1)
    }
    
    if(any(is.null(groups))) {
      panel.splom(x, y,
                  col   =col,
                  pch   =pch,
                  lty   =lty,
                  type  =type,
                  cex   = cex,
                  lwd   = lwd,
                  ...
                  )
    } else {
      ord <- order(x)
      panel.superpose(x[ord],
                      y[ord],
                      subscripts[ord],
                      pch   =pch,
                      cex   = cex,
                      lty   =lty,
                      type  =type,
                      lwd   = lwd,
                      groups=groups
                      )
    }
    
    if(!any(is.null(smooth))) {
      if(!is.factor(x) & !is.factor(y)){
        panel.loess(x,y,
                    span  = smspan,
                    degree= smdegr,
                    col   = smcol,
                    lwd   = smlwd,
                    lty   = smlty )
      } else {
        if(is.factor(x) & !is.factor(y)){
          panel.average(x, y,
                        fun = median,
                        horizontal = FALSE,
                        lwd=smlwd, lty=smlty, col=smcol,
                        col.line=smcol,
                        #type="l",
                        ...)
        }
        if(!is.factor(x) & is.factor(y)){
          panel.linejoin(x, y, fun = median, horizontal = TRUE,
                         lwd=smlwd, lty=smlty, col=smcol,
                         col.line=smcol, #type=smlty,
                         ...)
        }
        
      }
       

    }
    if(!any(is.null(lmline))) {
      if(!is.factor(x) & !is.factor(y)){
        panel.abline(lm(y~x),
                     col   = lmcol,
                     lwd   = lmlwd,
                     lty   = lmlty
                     )
      }
    }
  }
		                
