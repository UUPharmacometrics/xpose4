#' autocorrelation of the individual weighted residuals
#' 
#'
#' @inheritParams xpose.plot.default
#' @inheritParams xpose.panel.default
#' @return A Lattice object
#' @export
#'
#' 
#' @family specific functions 

autocorr.iwres <-
  function(object,
           #ylb  = "|WRES|",
           #idsdir="up",
           type="p",
           smooth=TRUE,
           ids=F,
           main = "Default",
           ...) {

    if(is.null(check.vars(c("iwres"),
                          object,silent=FALSE))) {
      return()
    }
    

    default.plot.title <- paste("Autocorrelation of ",xlabel(xvardef("iwres",object),object),
                                sep="")
    plotTitle <- xpose.multiple.plot.title(object=object,
                                           plot.text = default.plot.title,
                                           main=main,
                                           ...)

    xplot <- xpose.plot.default(xvardef("iwres",object),
                                xvardef("iwres",object),
                                object,
                                #ylb=ylb,
                                #idsdir=idsdir,
                                type=type,
                                smooth=smooth,
                                ids=ids,
                                autocorr=TRUE,
                                main=plotTitle,
                                ...)

    return(xplot)
  }
