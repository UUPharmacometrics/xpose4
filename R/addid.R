#' Generic internal functions for Xpose 4
#' 
#' These are internal functions relating to the Xpose generic functions.
#' 
#' These are internal Xpose functions, for adding ID numbers, computing
#' prediction intervals, randomization, stacking, and binning. They are not
#' intended for direct use.
#' 
#'      
#' 
#' @return Internal helper functions for the generic Xpose functions.
#' @author Justin Wilkins and Andrew Hooker
#' @keywords internal
#' @importFrom stats residuals
#' @importFrom stats loess

addid <- function(x,y,
                  ids   = ids,
                  idsmode=NULL,
                  idsext =0.05,
                  idscex= 0.7,
                  idsdir= "both",
                  gridmode=TRUE
                  )  {

  textfun <- "text"
  if(gridmode) textfun <- "ltext"
  
  if(!is.null(idsmode)) {
    do.call(textfun,list(x,y,ids,cex=idscex))
  } else {
    idd  <- ids
    yres <- residuals(loess(y~x))
    xcut <- cut(x,breaks=4,include.lowest=TRUE)

    ## Determine the number of points to plot
    if(idsext < 1) { # Fraction of total number of points
      idsext    <- ceiling(length(x)*idsext/4)
    }

    for(pp in levels(xcut)) {
      sel   <- xcut == pp
      yyres <- yres[sel]
      xx    <- x[sel]
      yy    <- y[sel]
      iidd  <- idd[sel]
      
      ord     <- order(yyres)
      ordy    <- yy[ord]
      ordx    <- xx[ord]
      ordidd  <- iidd[ord]

      if(!is.null(idsdir)) {
      ## Lower extreme
      if(idsdir=="both" || idsdir=="down") {
        do.call(textfun,list(ordx[1:idsext],
                             ordy[1:idsext],
                             ordidd[1:idsext],
                             cex=idscex))
      }
      ## Upper extreme
      if(idsdir=="both" || idsdir=="up") {
        ll <- length(ordx)
        
        ## Sanity check on ll added by Justin 10/10/05
        ## to prevent subscript errors in conditioned plots
        if (ll!=0) {
        try(
        do.call(textfun,list(ordx[(ll-idsext+1):ll],
                             ordy[(ll-idsext+1):ll],
                             ordidd[(ll-idsext+1):ll],
                             cex=idscex)), silent=T)
        }
        
      }
      }
    }
  }
}

  
