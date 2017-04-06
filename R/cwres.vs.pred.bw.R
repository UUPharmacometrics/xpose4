#' Box-and-whisker plot of conditional weighted residuals vs population
#' predictions for Xpose 4
#' 
#' This creates a box and whisker plot of conditional weighted residuals
#' (CWRES) vs population predictions (PRED), and is a specific function in
#' Xpose 4.  It is a wrapper encapsulating arguments to the
#' \code{xpose.plot.bw} function. Most of the options take their default values
#' from xpose.data object but may be overridden by supplying them as arguments.
#' 
#' 
#' This creates a box and whisker plot of conditional weighted residuals
#' (CWRES) vs population predictions (PRED), and is a specific function in
#' Xpose 4.  It is a wrapper encapsulating arguments to the
#' \code{xpose.plot.bw} function. Most of the options take their default values
#' from xpose.data object but may be overridden by supplying them as arguments.
#' 
#' Conditional weighted residuals (CWRES) require some extra steps to
#' calculate. See \code{\link{compute.cwres}} for details.
#' 
#' A wide array of extra options controlling bwplots are available. See
#' \code{\link{xpose.plot.bw}} and \code{\link{xpose.panel.bw}} for details.
#' 
#' @param object An xpose.data object.
#' @param \dots Other arguments passed to \code{link{xpose.plot.bw}}.
#' @return Returns a box-and-whisker plot of CWRES vs PRED.
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.plot.bw}}, \code{\link{xpose.panel.bw}},
#' \code{\link[lattice]{bwplot}}, \code{\link{xpose.prefs-class}},
#' \code{\link{compute.cwres}}, \code{\link{xpose.data-class}}
#' @keywords methods
#' @examples
#' ## Here we load the example xpose database 
#' xpdb <- simpraz.xpdb
#' 
#' cwres.vs.pred.bw(xpdb)
#' 
#' @export cwres.vs.pred.bw
#' @family specific functions 
cwres.vs.pred.bw <-
  function(object,
           #main = NULL,
           #xlb  = NULL,
           #ylb  = NULL,
           #onlyfirst=FALSE,
           #inclZeroWRES=FALSE,
           #subset=xsubset(object),
           #mirror=FALSE,
           #seed  = NULL,
           #bins  = 10,
           #samp  = NULL,
           ...) {

    ## check for arguments in function
    if(is.null(check.vars(c("cwres","pred"),
                          object,silent=FALSE))) {      
      return()
    }
    xplot <- xpose.plot.bw(xvardef("cwres",object),
                           xvardef("pred",object),
                           #xlb = xlb,
                           #ylb = ylb,
                                        #scales=list(cex=0.5,tck=0.5),
                           #aspect="fill",
                                  object,#main=list(main,cex=0.7),
                           #main = main,
                           #bins=bins,
                           #ids=FALSE,
                           binvar = xvardef("pred",object),
                                        #xvar = xvardef("cwres",object),
                           #subset=subset,
                           ...)


      return(xplot)
    }
