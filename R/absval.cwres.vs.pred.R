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



#' Absolute population conditional weighted residuals vs population predictions
#' for Xpose 4
#' 
#' This is a plot of absolute population conditional weighted residuals
#' (|CWRES|) vs population predictions (PRED), a specific function in Xpose 4.
#' It is a wrapper encapsulating arguments to the \code{xpose.plot.default}
#' function.  Most of the options take their default values from xpose.data
#' object but may be overridden by supplying them as arguments.
#' 
#' Conditional weighted residuals (CWRES) require some extra steps to
#' calculate. See \code{\link{compute.cwres}} for details.
#' 
#' A wide array of extra options controlling xyplots are available. See
#' \code{\link{xpose.plot.default}} for details.
#' 
#' @param object An xpose.data object.
#' @param idsdir Direction for displaying point labels. The default is "up",
#' since we are displaying absolute values.
#' @param type Type of plot. The default is points only ("p"), but lines ("l")
#' and both ("b") are also available.
#' @param smooth Logical value indicating whether an x-y smooth should be
#' superimposed.  The default is TRUE.
#' @param \dots Other arguments passed to \code{link{xpose.plot.default}}.
#' @return Returns an xyplot of |CWRES| vs PRED.
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.plot.default}},
#' \code{\link{xpose.panel.default}}, \code{\link[lattice]{xyplot}},
#' \code{\link{xpose.prefs-class}}, \code{\link{compute.cwres}},
#' \code{\link{xpose.data-class}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' }
#' 
#' ## Here we load the example xpose database 
#' data(simpraz.xpdb)
#' xpdb <- simpraz.xpdb
#' 
#' ## A vanilla plot
#' absval.cwres.vs.pred(xpdb)
#' 
#' ## A conditioning plot
#' absval.cwres.vs.pred(xpdb, by="HCTZ")
#' 
#' ## Custom heading and axis labels
#' absval.cwres.vs.pred(xpdb, main="My conditioning plot", ylb="|CWRES|", xlb="PRED")
#' 
#' ## Custom colours and symbols, no IDs
#' absval.cwres.vs.pred(xpdb, cex=0.6, pch=3, col=1, ids=FALSE)
#' 
#' 
#' @export absval.cwres.vs.pred
#' @family specific functions 
"absval.cwres.vs.pred" <-
  function(object,
           idsdir="up",
           type="p",
           smooth=TRUE,
           ...) {

    if(is.null(xvardef("cwres",object)) ||
       is.null(xvardef("pred",object))) {
      cat("The required (CWRES and PRED) variables are not set in the database!\n")
      return()
    }
    
    xplot <- xpose.plot.default(xvardef("pred",object),
                                xvardef("cwres",object),
                                object,
                                funy="abs",
                                idsdir=idsdir,
                                type=type,
                                smooth=smooth,
                                ...)
    return(xplot)
  }

