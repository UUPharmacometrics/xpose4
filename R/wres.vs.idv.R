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



#' Population weighted residuals (WRES) plotted against the independent
#' variable (IDV) for Xpose 4
#' 
#' This is a plot of population weighted residuals (WRES) vs the independent
#' variable (IDV), a specific function in Xpose 4. It is a wrapper
#' encapsulating arguments to the \code{xpose.plot.default} function. Most of
#' the options take their default values from xpose.data object but may be
#' overridden by supplying them as arguments.
#' 
#' Weighted residuals (WRES) are plotted against the independent variable, as
#' specified in \code{object@Prefs@Xvardef$idv}.
#' 
#' A wide array of extra options controlling xyplots are available. See
#' \code{\link{xpose.plot.default}} and \code{\link{xpose.panel.default}} for
#' details.
#' 
#' @param object An xpose.data object.
#' @param abline Vector of arguments to the \code{\link[lattice]{panel.abline}}
#' function. No abline is drawn if \code{NULL}.
#' @param smooth A \code{NULL} value indicates that no superposed line should
#' be added to the graph. If \code{TRUE} then a smooth of the data will be
#' superimposed.
#' @param \dots Other arguments passed to \code{link{xpose.plot.default}}.
#' @return Returns an xyplot of WRES vs IDV.
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.plot.default}},
#' \code{\link{xpose.panel.default}}, \code{\link[lattice]{xyplot}},
#' \code{\link{xpose.prefs-class}}, \code{\link{xpose.data-class}}
#' @examples
#' 
#' ## Here we load the example xpose database 
#' xpdb <- simpraz.xpdb
#' 
#' wres.vs.idv(xpdb)
#' 
#' ## A conditioning plot
#' wres.vs.idv(xpdb, by="HCTZ")
#' 
#' @export wres.vs.idv
#' @family specific functions 
"wres.vs.idv" <-
  function(object,
           abline=c(0,0),
           smooth=TRUE,
           ...) {

    if(is.null(check.vars(c("idv","wres"),
                          object,silent=FALSE))) {
      return()
    }

    xplot <- xpose.plot.default(xvardef("idv",object),
                                xvardef("wres",object),
                                smooth = smooth,
                                abline=abline,
                                object,
                                ...)
    
    return(xplot)
    
  }



