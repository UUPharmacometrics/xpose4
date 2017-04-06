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



#' Dependent variable vs individual predictions, conditioned on covariates, for
#' Xpose 4
#' 
#' This is a plot of dependent variable (DV) vs individual predictions (IPRED)
#' conditioned by covariates, a specific function in Xpose 4.  It is a wrapper
#' encapsulating arguments to the \code{xpose.plot.default} function. Most of
#' the options take their default values from xpose.data object but may be
#' overridden by supplying them as arguments.
#' 
#' Each of the covariates in the Xpose data object, as specified in
#' \code{object@Prefs@Xvardef$Covariates}, is evaluated in turn, creating a
#' stack of plots.
#' 
#' A wide array of extra options controlling \code{\link[lattice]{xyplot}} are
#' available. See \code{\link{xpose.plot.default}} and
#' \code{\link{xpose.panel.default}} for details.
#' 
#' @param object An xpose.data object.
#' @param abline Vector of arguments to the \code{\link[lattice]{panel.abline}}
#' function. No abline is drawn if \code{NULL}.
#' @param smooth Logical value indicating whether an x-y smooth should be
#' superimposed.  The default is TRUE.
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  
#' @param \dots Other arguments passed to \code{link{xpose.plot.default}}.
#' @return Returns a stack of \code{xyplot}s of DV vs IPRED, conditioned on
#' covariates.
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{dv.vs.ipred}}, \code{\link{xpose.plot.default}},
#' \code{\link{xpose.panel.default}}, \code{\link[lattice]{xyplot}},
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
#' dv.vs.ipred.by.cov(xpdb)
#' 
#' ## Custom axis labels
#' dv.vs.ipred.by.cov(xpdb, ylb="DV", xlb="PRED")
#' 
#' ## Custom colours and symbols, IDs
#' dv.vs.ipred.by.cov(xpdb, cex=0.6, pch=3, col=1, ids=TRUE)
#' }
#' 
#' @export dv.vs.ipred.by.cov
"dv.vs.ipred.by.cov" <-
  function(object,
           
           #xlb  = NULL,
           #ylb  = NULL,
           #onlyfirst=FALSE,
           #inclZeroWRES=FALSE,
           #subset=xsubset(object),
           #mirror=FALSE,
           #seed  = NULL,
           abline = c(0,1),
           #abllwd = object@Prefs@Graph.prefs$abllwd,
           #abllty = object@Prefs@Graph.prefs$abllty,
           #ablcol = object@Prefs@Graph.prefs$ablcol,
           #prompt = FALSE,
           smooth=TRUE,
           #main=NULL,
           #samp  = NULL,
           #max.plots.per.page=1,
           #scales=list(),
           #aspect = object@Prefs@Graph.prefs$aspect,#"fill"
           #mirror.aspect="fill",
           #pass.plot.list=FALSE,
           #x.cex=NULL,
           #y.cex=NULL,
           #main.cex=NULL,
           main="Default",
           ...) {

    
    ## Make sure we have the necessary variables defined in the 
    ## object.                                                  
    if(is.null(check.vars(c("dv","ipred","covariates"),object))) {
      return(NULL)
    }

    
      ## create enpty list for plots
      number.of.plots <- 0
      for (i in xvardef("covariates", object)) {
          number.of.plots <- number.of.plots + 1
      }    
      plotList <- vector("list",number.of.plots)
      plot.num <- 0 # initialize plot number      
        
      ## loop
      for (i in xvardef("covariates", object)) {      


    
        xplot <- xpose.plot.default(xvardef("ipred",object),
                                    xvardef("dv",object),
                                    #xlb = xlb,
                                    #ylb = ylb,
                                    abline=abline,
                                    #abllwd=abllwd,
                                    #scales=scales,
                                    #aspect=aspect,
                                    object,
                                    main=NULL,
                                    by=i,
                                    #subset=subset,
                                    smooth=smooth,
                                    #samp=samp,
                                    pass.plot.list=TRUE,
                                    ...)
                                  

        plot.num <- plot.num+1
        plotList[[plot.num]] <- xplot
      }

    default.plot.title <- paste(xlabel(xvardef("dv",object),object)," vs ",
                                xlabel(xvardef("ipred",object),object),
                                sep="")
    plotTitle <- xpose.multiple.plot.title(object=object,
                                           plot.text = default.plot.title,
                                           main=main,
                                           ...)
    obj <- xpose.multiple.plot(plotList,plotTitle,...)
    return(obj)

  }
