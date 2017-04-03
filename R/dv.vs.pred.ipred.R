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



#' Observations (DV) are plotted against individual predictions (IPRED) and
#' population predictions (PRED), for Xpose 4
#' 
#' This is a compound plot consisting of plots of observations (DV) against
#' individual predictions (IPRED) and population predictions (PRED), a specific
#' function in Xpose 4. It is a wrapper encapsulating arguments to the
#' \code{xpose.plot.default} function.
#' 
#' Plots of DV vs PRED and IPRED are presented side by side for comparison.
#' 
#' A wide array of extra options controlling \code{xyplot}s are available. See
#' \code{\link{xpose.plot.default}} and \code{\link{xpose.panel.default}} for
#' details.
#' 
#' @param object An xpose.data object.
#' @param xlb A string giving the label for the x-axis. \code{NULL} if none.
#' @param layout A list giving the layout of the graphs on the plot, in columns
#' and rows.
#' @param abline Vector of arguments to the \code{\link[lattice]{panel.abline}}
#' function. No abline is drawn if \code{NULL}.
#' @param lmline logical variable specifying whether a linear regression line
#' should be superimposed over an \code{\link[lattice]{xyplot}}. \code{NULL} ~
#' FALSE. (\code{y~x})
#' @param scales A list to be used for the \code{scales} argument in
#' \code{xyplot}.
#' @param smooth \code{NULL} or \code{TRUE} value indicating whether an x-y
#' smooth should be superimposed.
#' @param \dots Other arguments passed to \code{link{xpose.plot.default}}.
#' @return Returns a compound plot comprising plots of observations (DV)
#' against individual predictions (IPRED) and population predictions (PRED).
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{dv.vs.pred}}, \code{\link{dv.vs.ipred}},
#' \code{\link{xpose.plot.default}}, \code{\link{xpose.panel.default}},
#' \code{\link[lattice]{xyplot}}, \code{\link{xpose.prefs-class}},
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
#' dv.vs.pred.ipred(xpdb)
#' 
#' ## Custom colours and symbols, IDs
#' dv.vs.pred.ipred(xpdb, cex=0.6, pch=8, col=1, ids=TRUE)
#' 
#' 
#' @export dv.vs.pred.ipred
"dv.vs.pred.ipred" <-
  function(object,
           #main = NULL,
           xlb  = "Predictions",
           #ylb  = xlabel(xvardef("dv",object),object),
           layout=c(2,1),
           abline=c(0,1),
           lmline=TRUE,
           #subset = NULL, 
           #onlyfirst = FALSE,
           #inclZeroWRES = FALSE,
           smooth=NULL,
           scales=list(),
           ...) {

    ## Make sure we have the necessary variables defined in the 
    ## object.                                                  
    if(is.null(check.vars(c("id","dv","pred","ipred"),object))) {
      return(NULL)
    }

    ## set scales
    if(is.null(scales$x$relation)) scales$x$relation="same"

    
    xplot <- xpose.plot.default(c(xvardef("pred",object),
                                  xvardef("ipred",object)),
                                xvardef("dv",object),
                                object,
                                scales=scales,
                                xlb=xlb,
                                #ylb=ylb,
                                #main=main,
                                layout=layout,
                                abline=abline,
                                lmline=lmline,
                                #subset=subset,
                                smooth=smooth,
                                ...)
        
    return(xplot)
  }

