#' Weighted residuals (WRES) and conditional WRES (CWRES) plotted against the
#' independent variable (IDV) or the population predictions (PRED) for Xpose 4
#' 
#' These functions graphically compare WRES and CWRES as plotted against the
#' independent variable or the population predictions.
#' 
#' This function creates plots of WRES and CWRES, presented side-by-side for
#' comparison.
#' 
#' Conditional weighted residuals (CWRES) require some extra steps to
#' calculate. See \code{\link{compute.cwres}} for details.
#' 
#' A wide array of extra options controlling xyplots are available. See
#' \code{\link{xpose.plot.default}} and \code{\link{xpose.panel.default}} for
#' details.
#' 
#' @param object An xpose.data object.
#' @param ylb A string giving the label for the y-axis. \code{NULL} if none.
#' @param abline Vector of arguments to the \code{\link[lattice]{panel.abline}}
#' function. No abline is drawn if \code{NULL}.
#' @param smooth A \code{NULL} value indicates that no superposed line should
#' be added to the graph. If \code{TRUE} then a smooth of the data will be
#' superimposed.
#' @param scales scales is passed to \code{xpose.plot.default}
#' @param \dots Other arguments passed to \code{link[lattice]{xyplot}}.
#' @return A compound xyplot.
#' @author Niclas Jonsson & Andrew Hooker
#' @seealso \code{\link{xpose.plot.default}},
#' \code{\link{xpose.panel.default}}, \code{\link[lattice]{xyplot}},
#' \code{\link{xpose.prefs-class}}, \code{\link{xpose.data-class}},
#' \code{\link{compute.cwres}}
#' @examples
#' ## Here we load the example xpose database 
#' xpdb <- simpraz.xpdb
#' 
#' cwres.wres.vs.idv(xpdb)
#' @name cwres_wres_vs_x
#' @family specific functions 
NULL



#' @export  
#' @describeIn cwres_wres_vs_x Weighted residuals (WRES) and conditional WRES (CWRES) plotted against the
#' independent variable (IDV)
cwres.wres.vs.idv <-
  function(object,
           ylb  = "Residuals",
           abline = c(0,0),
           smooth=TRUE,
           scales=list(),
           ...) {

    ## Make sure we have the necessary variables defined in the 
    ## object.                                                  
    if(is.null(check.vars(c("idv","cwres","wres"),object))) {
      return(NULL)
    }

    ## set scales
    if(is.null(scales$x$relation)) scales$x$relation="same"
    
    xplot <- xpose.plot.default(xvardef("idv",object),
                                c(xvardef("cwres",object),
                                  xvardef("wres",object)),
                                object,
                                scales=scales,
                                ylb=ylb,
                                abline=abline,
                                smooth=smooth,
                                ...)
        
    return(xplot)

  }

