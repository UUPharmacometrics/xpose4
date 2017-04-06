
#' Absolute population weighted residuals vs population predictions, and
#' absolute individual weighted residuals vs individual predictions, for Xpose
#' 4
#' 
#' This is a matrix plot of absolute population weighted residuals (|CWRES|) vs
#' population predictions (PRED) and absolute individual weighted residuals
#' (|IWRES|) vs individual predictions (IPRED), a specific function in Xpose 4.
#' It is a wrapper encapsulating arguments to the \code{absval.cwres.vs.pred}
#' and \code{absval.iwres.vs.ipred} functions.
#' 
#' The plots created by the \code{absval.wres.vs.pred} and
#' \code{absval.iwres.vs.ipred} functions are presented side by side for
#' comparison.
#' 
#' A wide array of extra options controlling xyplots are available. See
#' \code{\link{xpose.plot.default}} for details.
#' 
#' @aliases absval.iwres.wres.vs.ipred.pred absval.iwres.cwres.vs.ipred.pred
#' @param object An xpose.data object.
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  
#' @param \dots Other arguments passed to \code{link{xpose.plot.default}}.
#' @return Returns a compound plot.
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{absval.wres.vs.pred}},
#' \code{\link{absval.iwres.vs.ipred}}, \code{\link{xpose.plot.default}},
#' \code{\link{xpose.panel.default}}, \code{\link[lattice]{xyplot}},
#' \code{\link{xpose.prefs-class}}, \code{\link{xpose.data-class}}
#' @examples
#' 
#' ## Here we load the example xpose database 
#' xpdb <- simpraz.xpdb
#' 
#' ## A vanilla plot
#' absval.iwres.wres.vs.ipred.pred(xpdb)
#' absval.iwres.cwres.vs.ipred.pred(xpdb)
#' 
#' ## Custom colours and symbols
#' absval.iwres.cwres.vs.ipred.pred(xpdb, cex=0.6, pch=8, col=1)
#' 
#' @export 
#' @family specific functions 
absval.iwres.cwres.vs.ipred.pred <-
  function(object,
           
           ##aspect="fill",
           main="Default",
           ...) {

  
    if(is.null(check.vars(c("pred","cwres","iwres","ipred"),
                          object,silent=FALSE))) {
      return()
    }

    num.of.plots <- 2
    plotList <- vector("list",num.of.plots)

    plot1 <- absval.cwres.vs.pred(object,main=NULL,
                              ##aspect=aspect,
                              pass.plot.list=TRUE,
                              ...)
    plot2 <- absval.iwres.vs.ipred(object,main=NULL,
                                ##aspect=aspect,
                                pass.plot.list=TRUE,
                                ...)

    plotList[[1]] <- plot1
    plotList[[2]] <- plot2

    default.plot.title <- "(Condotional) Weighted residuals vs. Predictions"
    
    plotTitle <- xpose.multiple.plot.title(object=object,
                                           plot.text = default.plot.title,
                                           main=main,
                                           ...)
    obj <- xpose.multiple.plot(plotList,plotTitle,...)
    return(obj)

}
