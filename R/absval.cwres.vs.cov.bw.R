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



#' Absolute conditional weighted residuals vs covariates for Xpose 4
#' 
#' This creates a stack of box and whisker plot of absolute population
#' conditional weighted residuals (|CWRES|) vs covariates, and is a specific
#' function in Xpose 4.  It is a wrapper encapsulating arguments to the
#' code\link{xpose.plot.bw} function. Most of the options take their default
#' values from xpose.data object but may be overridden by supplying them as
#' arguments.
#' 
#' Each of the covariates in the Xpose data object, as specified in
#' \code{object@Prefs@Xvardef$Covariates}, is evaluated in turn, creating a
#' stack of plots.
#' 
#' Conditional weighted residuals (CWRES) require some extra steps to
#' calculate. See \code{\link{compute.cwres}} for details.
#' 
#' A wide array of extra options controlling box-and-whisker plots are
#' available. See \code{\link{xpose.plot.bw}} for details.
#' 
#' @param object An xpose.data object.
#' @param xlb A string giving the label for the x-axis. \code{NULL} if none.
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  For \code{"Default"} the function
#' \code{\link{xpose.multiple.plot.title}} is used.
#' @param \dots Other arguments passed to \code{\link{xpose.plot.bw}}.
#' @return Returns a stack of box-and-whisker plots of |CWRES| vs covariates.
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.plot.bw}}, \code{\link{xpose.panel.bw}},
#' \code{\link{compute.cwres}}, \code{\link[lattice]{bwplot}},
#' \code{\link{xpose.prefs-class}}, \code{\link{xpose.data-class}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' 
#' ## Here we load the example xpose database 
#' data(simpraz.xpdb)
#' xpdb <- simpraz.xpdb
#' 
#' ## A vanilla plot
#' absval.cwres.vs.cov.bw(xpdb)
#' 
#' 
#' ## A custom plot
#' absval.cwres.vs.cov.bw(xpdb, bwdotcol="white", 
#'   bwdotpch=15,
#'   bwreccol="red",
#'   bwrecfill="red",
#'   bwumbcol="red",
#'   bwoutpch=5,
#'   bwoutcol="black")
#' }
#' @export absval.cwres.vs.cov.bw
"absval.cwres.vs.cov.bw" <-
  function(object,
           
           xlb = "|CWRES|",
           main="Default",
           ...) {

    if (is.null(xvardef("covariates", object))) {
        return(cat("Covariates are not properly set in the database!\n"))
    }
    
    if(is.null(xvardef("cwres",object))) {
        return(cat("There are no CWRES defined in the database!\n"))    
    }


    ## create list for plots
    number.of.plots <- 0
    for (i in xvardef("covariates", object)) {
      number.of.plots <- number.of.plots + 1
    }
    plotList <- vector("list",number.of.plots)
    
    
    ## loop through covariates
    plot.num <- 0 # initialize plot number
    for (i in xvardef("covariates", object)) {
    
      xplot <- xpose.plot.bw(xvardef("cwres",object),
                             i,
                             object,
                             main=NULL,
                             xlb=xlb,
                             binvar = i,
                             funx="abs",
                             pass.plot.list = TRUE,
                             ...)
      

      plot.num <- plot.num+1
      plotList[[plot.num]] <- xplot
    }
    
    default.plot.title <- paste("|",xlabel(xvardef("cwres",object),object),"| vs ",
                                "Covariates", sep="")
    
    plotTitle <- xpose.multiple.plot.title(object=object,
                                           plot.text = default.plot.title,
                                           main=main,
                                           ...)
    obj <- xpose.multiple.plot(plotList,plotTitle,...)
    return(obj)

  }
