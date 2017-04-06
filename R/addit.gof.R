

#' Additional goodness-of-fit plots, for Xpose 4
#' 
#' This is a compound plot consisting of plots of weighted population residuals
#' (WRES) vs population predictions (PRED), absolute individual weighted
#' residuals (|IWRES|) vs independent variable (IDV), WRES vs IDV, and weighted
#' population residuals vs log(IDV), a specific function in Xpose 4. It is a
#' wrapper encapsulating arguments to the \code{wres.vs.pred},
#' \code{iwres.vs.idv} and \code{wres.vs.idv} functions.
#' 
#' Four additional goodness-of-fit plots are presented side by side for
#' comparison.
#' 
#' A wide array of extra options controlling xyplots are available. See
#' \code{\link{xpose.plot.default}} and
#' \code{\link{xpose.multiple.plot.default}} for details.
#' 
#' @param object An xpose.data object.
#' @param type 1-character string giving the type of plot desired.  The
#' following values are possible, for details, see 'plot': '"p"' for points,
#' '"l"' for lines, '"o"' for overplotted points and lines, '"b"', '"c"') for
#' (empty if '"c"') points joined by lines, '"s"' and '"S"' for stair steps and
#' '"h"' for histogram-like vertical lines.  Finally, '"n"' does not produce
#' any points or lines.
#' @param title.size Amount, in a range of 0-1, of how much space the title
#' should take up in the plot)
#' @param title.just how the title should be justified
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  
#' @param force.wres Plot the WRES even if other residuals are available.
#' @param \dots Other arguments passed to \code{link{xpose.plot.default}}.
#' @return Returns a compound plot comprising plots of weighted population
#' residuals (WRES) vs population predictions (PRED), absolute individual
#' weighted residuals (|IWRES|) vs independent variable (IDV), WRES vs IDV, and
#' weighted population residuals vs log(IDV).
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{wres.vs.pred}}, \code{\link{iwres.vs.idv}},
#' \code{\link{wres.vs.idv}}, \code{\link{xpose.plot.default}},
#' \code{\link{xpose.panel.default}}, \code{\link[lattice]{xyplot}},
#' \code{\link{xpose.prefs-class}}, \code{\link{xpose.data-class}}
#' @keywords methods
#' @examples

#' ## Here we load the example xpose database 
#' xpdb <- simpraz.xpdb
#' 
#' ## A vanilla plot
#' addit.gof(xpdb)
#' 
#' @export addit.gof
#' @family specific functions 
"addit.gof" <-
  function(object,
           type="p",
           title.size=0.02,
           title.just=c("center","top"),
           
           main="Default",
           force.wres=FALSE,
           ...) {

    if(is.null(check.vars(c("dv","pred","ipred","iwres","idv"),
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
    
    ## create empty list for plots
    num.of.plots <- 4
    plotList <- vector("list",length(num.of.plots))

    if(use.cwres){
      xplot1 <- cwres.vs.pred(object,
                              main=NULL,
                              type=type,
                              pass.plot.list=TRUE,
                              ...)
      
      xplot3 <- cwres.vs.idv(object,
                            main=NULL,
                            type="n",
                            ids=F,
                            pass.plot.list=TRUE,
                            ...)

      xplot4 <- cwres.vs.idv(object,
                            main=NULL,
                            logx=T,
                            type=type,
                            pass.plot.list=TRUE,
                            ...)
      

    } else {
      xplot1 <- wres.vs.pred(object,
                             main=NULL,
                             type=type,
                             pass.plot.list=TRUE,
                             ...)

      xplot3 <- wres.vs.idv(object,
                            main=NULL,
                            type="n",
                            ids=F,
                            pass.plot.list=TRUE,
                            ...)

      xplot4 <- wres.vs.idv(object,
                            main=NULL,
                            logx=T,
                            type=type,
                            pass.plot.list=TRUE,
                            ...)
      
    }
    
    xplot2 <- absval.iwres.vs.idv(object,
                                  main=NULL,
                                  ylb=paste("Absolute value of iWRES"),
                                  pass.plot.list=TRUE,
                                  ...)

    
    plotList[[1]] <- xplot1
    plotList[[2]] <- xplot2
    plotList[[3]] <- xplot3
    plotList[[4]] <- xplot4      


    default.plot.title <- "Additional goodness of fit plots"
    plotTitle <- xpose.multiple.plot.title(object=object,
                                           plot.text = default.plot.title,main=main,
                                           ...)

    
    obj <- xpose.multiple.plot(plotList,plotTitle,...)
    return(obj)

  }

