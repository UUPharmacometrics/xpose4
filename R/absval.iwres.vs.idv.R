#' abslute value of the 
#' individual weighted residuals vs. the indenpendent variable
#' 
#'
#' @inheritParams xpose.plot.default
#'
#' @return A lattice object
#' @export
#'
#' 
#' @family specific functions 
absval.iwres.vs.idv <-
  function(object,
           ylb  = "|iWRES|",
           smooth       = TRUE,
           idsdir       = "up",
           type         = "p",
           ...) {

    if(is.null(check.vars(c("idv","iwres"),
                          object,silent=FALSE))) {
      return()
    }



    xplot <- xpose.plot.default(xvardef("idv",object),
                                xvardef("iwres",object),
                                object,
                                ylb=ylb,
                                funy="abs",
                                idsdir=idsdir,
                                smooth=smooth,
                                type = type,
                                ...)
    
    return(xplot)
  }

