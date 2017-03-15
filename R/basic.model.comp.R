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



#' Basic model comparison plots, for Xpose 4
#' 
#' This creates a stack of four plots, comparing PRED, IPRED, WRES (or CWRES),
#' and IWRES estimates for the two specified model fits.
#' 
#' Four basic model comparison plots are displayed in sequence.
#' 
#' Conditional weighted residuals (CWRES) require some extra steps to
#' calculate. See \code{\link{compute.cwres}} for details.
#' 
#' A wide array of extra options controlling xyplots are available. See
#' \code{\link{xpose.plot.default}} for details.
#' 
#' @param object An xpose.data object.
#' @param object.ref An xpose.data object. If not supplied, the user will be
#' prompted.
#' @param inclZeroWRES Logical value indicating whether rows with WRES=0 is
#' included in the plot. The default is TRUE.
#' @param onlyfirst Logical value indicating whether only the first row per
#' individual is included in the plot.
#' @param subset A string giving the subset expression to be applied to the
#' data before plotting. See \code{\link{xsubset}}.
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  For \code{"Default"} the function
#' \code{\link{xpose.multiple.plot.title}} is used.
#' @param force.wres Force function to use WRES?
#' @param \dots Other arguments passed to \code{link{xpose.plot.default}}.
#' @return Returns a stack of plots comprising comparisons of PRED, IPRED, WRES
#' (or CWRES) and IWRES for the two specified runs.
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.plot.default}},
#' \code{\link{xpose.panel.default}}, \code{\link[lattice]{xyplot}},
#' \code{\link{compute.cwres}}, \code{\link{xpose.prefs-class}},
#' \code{\link{xpose.data-class}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## We expect to find the required NONMEM run and table files for runs
#' ## 5 and 6 in the current working directory
#' xpdb5 <- xpose.data(5)
#' xpdb6 <- xpose.data(6)
#' 
#' ## A vanilla plot, without prompts
#' basic.model.comp(xpdb5, xpdb6, prompt = FALSE)
#' 
#' ## Custom colours and symbols, no user IDs
#' basic.model.comp.cwres(xpdb5, xpdb6, cex=0.6, pch=8, col=1, ids=NULL)
#' }
#' 
#' 
#' @export basic.model.comp
"basic.model.comp" <- function(object, 
                               object.ref = NULL,
                               onlyfirst = FALSE,
                               inclZeroWRES = FALSE,
                               subset = xsubset(object),
                               #prompt = TRUE,
                               main="Default",
                               force.wres=FALSE,
                               #ref.default = ".ref.db",
                               ...) {

    if (is.null(object.ref)) {
      ref.list <- get.refrunno()
      if(exists(".ref.db")){
        object.ref <- eval(parse(text=".ref.db"))
      } else {
        return()
      }
      if(any(is.null(ref.list)))
        return()
    } 
  
  if(dim(object@Data)[1] != dim(object.ref@Data)[1]) {
    cat("The current database and the reference database do not have\n")
    cat("the same number of lines!\n")
    invisible()
    return()
  }


  if(is.null(check.vars(c("idlab","pred","ipred","iwres"),
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

  
  object@Data$PRED.REF <- object.ref@Data[,xvardef("pred", object.ref)]
  object@Data$IPRED.REF <- object.ref@Data[,xvardef("ipred", object.ref)]
  if(use.cwres){
    object@Data$CWRES.REF <- object.ref@Data[,xvardef("cwres", object.ref)]
  } else {
    object@Data$WRES.REF <- object.ref@Data[,xvardef("wres", object.ref)]
  }
  
  object@Data$IWRES.REF <- object.ref@Data[,xvardef("iwres", object.ref)]
  
  
  ## PRED vs PRED
  if(!any(is.null(xvardef("pred", object))) && !any(is.null(xvardef("pred", object.ref)))) {
    xlb <- paste(xlabel(xvardef("pred",object),object), " (Run ", object@Runno, ")",sep="")
    ylb <- paste(xlabel(xvardef("pred",object.ref),object.ref), " (Run ", object.ref@Runno, ")",sep="")
    main <- paste(ylb, " vs ", xlb, sep="")
   
    xplot1 <- xpose.plot.default(xvardef("pred", object),
                        "PRED.REF",
                        object,
                        xlb = xlb,
                        ylb = ylb,
                        main = NULL,
                        abline=c(0,1),
                        onlyfirst = onlyfirst,
                        inclZeroWRES = inclZeroWRES,
                        subset = subset,
                        ...)
    
  }

                                        # IPRED vs IPRED
  if(!any(is.null(xvardef("ipred", object))) && !any(is.null(xvardef("ipred", object.ref)))) {
    xlb <- paste(xlabel(xvardef("ipred",object),object), " (Run ", object@Runno, ")",sep="")
    ylb <- paste(xlabel(xvardef("ipred",object.ref),object.ref), " (Run ", object.ref@Runno, ")",sep="")
    main <- paste(ylb, " vs ", xlb, sep="")
   
    xplot2 <- xpose.plot.default(xvardef("ipred", object),
                        "IPRED.REF",
                        object,
                        xlb = xlb,
                        ylb = ylb,
                        main = NULL,
                        abline=c(0,1),
                        onlyfirst = onlyfirst,
                        inclZeroWRES = inclZeroWRES,
                        subset = subset,
                        ...)
    
  }

  if(use.cwres){
    ## CWRES vs CWRES
    if(!any(is.null(xvardef("cwres", object))) && !any(is.null(xvardef("cwres", object.ref)))) {
      xlb <- paste(xlabel(xvardef("cwres",object),object), " (Run ", object@Runno, ")",sep="")
      ylb <- paste(xlabel(xvardef("cwres",object.ref),object.ref), " (Run ", object.ref@Runno, ")",sep="")
      main <- paste(ylb, " vs ", xlb, sep="")
      
      xplot3 <- xpose.plot.default(xvardef("cwres", object),
                                   "CWRES.REF",
                                   object,
                                   xlb = xlb,
                                   ylb = ylb,
                                   main = NULL,
                                   abline=c(0,1),
                                   onlyfirst = onlyfirst,
                                   inclZeroWRES = inclZeroWRES,
                                   subset = subset,
                                   ...) 
      
    }
  } else {
    ## WRES vs WRES
    if(!any(is.null(xvardef("wres", object))) && !any(is.null(xvardef("wres", object.ref)))) {
      xlb <- paste(xlabel(xvardef("wres",object),object), " (Run ", object@Runno, ")",sep="")
      ylb <- paste(xlabel(xvardef("wres",object.ref),object.ref), " (Run ", object.ref@Runno, ")",sep="")
      main <- paste(ylb, " vs ", xlb, sep="")
      
      xplot3 <- xpose.plot.default(xvardef("wres", object),
                                   "WRES.REF",
                                   object,
                                   xlb = xlb,
                                   ylb = ylb,
                                   main = NULL,
                                   abline=c(0,1),
                                   onlyfirst = onlyfirst,
                                   inclZeroWRES = inclZeroWRES,
                                   subset = subset,
                                   ...) 
      
    }
  }
          
  # IWRES vs IWRES
  if(!any(is.null(xvardef("iwres", object))) && !any(is.null(xvardef("iwres", object.ref)))) {
    xlb <- paste(xlabel(xvardef("iwres",object),object), " (Run ", object@Runno, ")",sep="")
    ylb <- paste(xlabel(xvardef("iwres",object.ref),object.ref), " (Run ", object.ref@Runno, ")",sep="")
    main <- paste(ylb, " vs ", xlb, sep="")
   
    xplot4 <- xpose.plot.default(xvardef("iwres", object),
                        "IWRES.REF",
                        object,
                        xlb = xlb,
                        ylb = ylb,
                        main = NULL,
                        abline=c(0,1),
                        onlyfirst = onlyfirst,
                        inclZeroWRES = inclZeroWRES,
                        subset = subset,
                        ...) 
  }
    ## create enpty list for plots
    num.of.plots <- 4
    plotList <- vector("list",num.of.plots)

    plotList[[1]] <- xplot1
    plotList[[2]] <- xplot2
    plotList[[3]] <- xplot3
    plotList[[4]] <- xplot4      

        
    
    default.plot.title <- "Basic model comparison plots"
    plotTitle <- xpose.multiple.plot.title(object=object,
                                           plot.text = default.plot.title,
                                           subset=subset,
                                           main=main,
                                           ...)
  
    
    obj <- xpose.multiple.plot(plotList,plotTitle,...)
    return(obj)

  }
