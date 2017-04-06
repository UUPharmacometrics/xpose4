#' Observations (DV), individual predictions (IPRED) and population predictions
#' (IPRED) plotted against the independent variable (IDV), for Xpose 4
#' 
#' This is a compound plot consisting of plots of observations (DV), individual
#' predictions (IPRED), and population predictions (PRED) against the
#' independent variable (IDV), a specific function in Xpose 4.  It is a wrapper
#' encapsulating arguments to the \code{xpose.plot.default} function.
#' 
#' A wide array of extra options controlling \code{xyplots} are available. See
#' \code{\link{xpose.plot.default}} and \code{\link{xpose.panel.default}} for
#' details.
#' 
#' @param object An xpose.data object.
#' @param ylb A string giving the label for the y-axis. \code{NULL} if none.
#' @param layout A list controlling the number of columns and rows in a
#' compound plot. The default is 2 columns and 1 row.
#' @param smooth Logical value indicating whether an x-y smooth should be
#' superimposed.  The default is TRUE.
#' @param scales A list to be used for the \code{scales} argument in
#' \code{xyplot}.
#' @param \dots Other arguments passed to \code{link{xpose.plot.default}}.
#' @return Returns a compound plot comprising plots of observations (DV),
#' individual predictions (IPRED), and population predictions (PRED) against
#' the independent variable (IDV).
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{dv.vs.idv}}, \code{\link{ipred.vs.idv}},
#' \code{\link{pred.vs.idv}}, \code{\link{xpose.plot.default}},
#' \code{\link{xpose.panel.default}}, \code{\link[lattice]{xyplot}},
#' \code{\link{xpose.prefs-class}}, \code{\link{xpose.data-class}}
#' @keywords methods
#' @examples
#' 
#' ## Here we load the example xpose database 
#' xpdb <- simpraz.xpdb
#' 
#' dv.preds.vs.idv(xpdb)
#' 
#' @export dv.preds.vs.idv
#' @family specific functions 
"dv.preds.vs.idv" <-
  function(object,
           #main = NULL,
           ylb  = "Observations/Predictions",
           #xlb  = xlabel(xvardef("idv",object),object),
           layout=c(3,1),
           #subset = xsubset(object), 
           #onlyfirst = FALSE,
           #inclZeroWRES = FALSE,
           smooth=TRUE,
           scales=list(),
           ...) 
  
{

    ## Make sure we have the necessary variables defined in the 
    ## object.                                                  
    if(is.null(check.vars(c("id","idv","dv","pred","ipred"),object))) {
      return(NULL)
    }


    if(is.null(scales$x$relation)) scales$x$relation="same"
    
    xplot <- xpose.plot.default(xvardef("idv",object),
                                c(xvardef("dv",object),
                                  xvardef("ipred",object),
                                  xvardef("pred",object)),
                                object,
                                scales=scales,
                                #xlb=xlb,
                                ylb=ylb,
                                #main=main,
                                layout=layout,
                                #abline=abline,
                                #lmline=lmline,
                                #subset=subset,
                                smooth=smooth,
                                ...)
        
    return(xplot)

  }

