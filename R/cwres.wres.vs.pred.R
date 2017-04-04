#' @export  
#' @describeIn cwres_wres_vs_x Weighted residuals (WRES) and conditional WRES (CWRES) plotted against the
#' population predictions (PRED)
cwres.wres.vs.pred <-
  function(object,
           ylb  = "Residuals",
           abline = c(0,0),
           smooth=TRUE,
           scales=list(),
           ...) {

    ## Make sure we have the necessary variables defined in the 
    ## object.                                                  
    if(is.null(check.vars(c("pred","cwres","wres"),object))) {
      return(NULL)
    }

    ## set scales
    if(is.null(scales$x$relation)) scales$x$relation="same"
    
    xplot <- xpose.plot.default(xvardef("pred",object),
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

