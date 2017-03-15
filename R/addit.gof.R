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



#' Additional goodness-of-fit plots, for Xpose 4
#' 
#' This is a compound plot consisting of plots of weighted population residuals
#' (WRES) vs population predictions (PRED), absolute individual weighted
#' residuals (|IWRES|) vs independent variable (IDV), WRES vs IDV, and weighted
#' population residuals vs log(IDV), a specific function in Xpose 4. It is a
#' wrapper encapsulating arguments to the \code{wres.vs.pred},
#' \code{iwres.vs.idv} and \code{wres.vs.idv} functions.
#' 
#' Four additional goodness-of-fit plots are presented side by side for
#' comparison.
#' 
#' A wide array of extra options controlling xyplots are available. See
#' \code{\link{xpose.plot.default}} and
#' \code{\link{xpose.multiple.plot.default}} for details.
#' 
#' @param object An xpose.data object.
#' @param type 1-character string giving the type of plot desired.  The
#' following values are possible, for details, see 'plot': '"p"' for points,
#' '"l"' for lines, '"o"' for overplotted points and lines, '"b"', '"c"') for
#' (empty if '"c"') points joined by lines, '"s"' and '"S"' for stair steps and
#' '"h"' for histogram-like vertical lines.  Finally, '"n"' does not produce
#' any points or lines.
#' @param title.size Amount, in a range of 0-1, of how much space the title
#' should take up in the plot)
#' @param title.just how the title should be justified
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  For \code{"Default"} the function
#' \code{\link{xpose.multiple.plot.title}} is used.
#' @param force.wres Plot the WRES even if other residuals are available.
#' @param \dots Other arguments passed to \code{link{xpose.plot.default}}.
#' @return Returns a compound plot comprising plots of weighted population
#' residuals (WRES) vs population predictions (PRED), absolute individual
#' weighted residuals (|IWRES|) vs independent variable (IDV), WRES vs IDV, and
#' weighted population residuals vs log(IDV).
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{wres.vs.pred}}, \code{\link{iwres.vs.idv}},
#' \code{\link{wres.vs.idv}}, \code{\link{xpose.plot.default}},
#' \code{\link{xpose.panel.default}}, \code{\link[lattice]{xyplot}},
#' \code{\link{xpose.prefs-class}}, \code{\link{xpose.data-class}}
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
#' addit.gof(xpdb)
#' 
#' ## Custom colours and symbols
#' addit.gof(xpdb, cex=0.3, pch=0, col=8)
#' 
#' 
#' 
#' @export addit.gof
"addit.gof" <-
  function(object,
           type="p",
           title.size=0.02,
           title.just=c("center","top"),
           
           main="Default",
           force.wres=FALSE,
           ...) {

    if(is.null(check.vars(c("dv","pred","ipred","iwres","idv"),
                          object,silent=FALSE))) {
      return()
    }

    use.cwres=TRUE
    if(force.wres){
      use.cwres=FALSE
      if(is.null(check.vars(c("wres"),object,silent=FALSE))) return()
    } else {
      if(is.null(check.vars(c("cwres"),object,silent=TRUE))) {
        use.cwres=FALSE
        if(is.null(check.vars(c("wres"),object,silent=FALSE))) return()
      }
    }
    
    ## create empty list for plots
    num.of.plots <- 4
    plotList <- vector("list",length(num.of.plots))

    if(use.cwres){
      xplot1 <- cwres.vs.pred(object,
                              main=NULL,
                              type=type,
                              pass.plot.list=TRUE,
                              ...)
      
      xplot3 <- cwres.vs.idv(object,
                            main=NULL,
                            type="n",
                            ids=F,
                            pass.plot.list=TRUE,
                            ...)

      xplot4 <- cwres.vs.idv(object,
                            main=NULL,
                            logx=T,
                            type=type,
                            pass.plot.list=TRUE,
                            ...)
      

    } else {
      xplot1 <- wres.vs.pred(object,
                             main=NULL,
                             type=type,
                             pass.plot.list=TRUE,
                             ...)

      xplot3 <- wres.vs.idv(object,
                            main=NULL,
                            type="n",
                            ids=F,
                            pass.plot.list=TRUE,
                            ...)

      xplot4 <- wres.vs.idv(object,
                            main=NULL,
                            logx=T,
                            type=type,
                            pass.plot.list=TRUE,
                            ...)
      
    }
    
    xplot2 <- absval.iwres.vs.idv(object,
                                  main=NULL,
                                  ylb=paste("Absolute value of iWRES"),
                                  pass.plot.list=TRUE,
                                  ...)

    
    plotList[[1]] <- xplot1
    plotList[[2]] <- xplot2
    plotList[[3]] <- xplot3
    plotList[[4]] <- xplot4      


    default.plot.title <- "Additional goodness of fit plots"
    plotTitle <- xpose.multiple.plot.title(object=object,
                                           plot.text = default.plot.title,main=main,
                                           ...)

    
    obj <- xpose.multiple.plot(plotList,plotTitle,...)
    return(obj)

  }

