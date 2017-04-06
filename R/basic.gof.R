
#' Basic goodness-of-fit plots, for Xpose 4
#' 
#' This is a compound plot consisting of plots of observations (DV) vs
#' population predictions (PRED), observations (DV) vs individual predictions
#' (IPRED), absolute individual weighted residuals (|IWRES|) vs IPRED, and
#' weighted population residuals (CWRES) vs independent variable (IDV), a
#' specific function in Xpose 4. WRES are also supported. It is a wrapper
#' encapsulating arguments to the \code{dv.vs.pred}, \code{dv.vs.ipred},
#' \code{absval.iwres.vs.ipred} and \code{wres.vs.idv} functions.
#' 
#' Four basic goodness-of-fit plots are presented side by side for comparison.
#' 
#' Conditional weighted residuals (CWRES) require some extra steps to
#' calculate. See \code{\link{compute.cwres}} for details.
#' 
#' A wide array of extra options controlling xyplots are available. See
#' \code{\link{xpose.plot.default}} for details.
#' 
#' \code{basic.gof.cwres} is just a wrapper for \code{basic.gof} with
#' \code{use.cwres=TRUE}.
#' 
#' @param object An xpose.data object.
#' @param force.wres Should the plots use WRES?  Values can be
#' \code{TRUE/FALSE}.  Otherwise the CWRES are used if present.
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  
#' @param use.log Should we use log transformations in the plots?
#' @param \dots Other arguments passed to \code{\link{xpose.plot.default}}.
#' @return Returns a compound plot comprising plots of observations (DV) vs
#' population predictions (PRED), DV vs individual predictions (IPRED),
#' absolute individual weighted residuals (|IWRES|) vs IPRED, and weighted
#' populations residuals (WRES) vs the independent variable (IDV).
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{dv.vs.pred}}, \code{\link{dv.vs.ipred}},
#' \code{\link{absval.iwres.vs.ipred}}, \code{\link{wres.vs.idv}},
#' \code{\link{cwres.vs.idv}}, \code{\link{xpose.plot.default}},
#' \code{\link{xpose.panel.default}}, \code{\link[lattice]{xyplot}},
#' \code{\link{compute.cwres}}, \code{\link{xpose.prefs-class}},
#' \code{\link{xpose.data-class}}
#' @examples
#' ## Here we load the example xpose database 
#' xpdb <- simpraz.xpdb
#' 
#' basic.gof(xpdb)
#' 
#' ## Custom colours and symbols, IDs of individuals in study
#' basic.gof(xpdb, cex=0.6, pch=8, col=1, ids=TRUE)
#' 
#' @export basic.gof
#' @family specific functions 
"basic.gof" <-
  function(object,
           force.wres=FALSE,
           main="Default",
           use.log = FALSE,
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
    
    ## create enpty list for plots
    num.of.plots <- 4
    plotList <- vector("list",num.of.plots)

    loglist <- c(dv=F, pred=F, ipred=F, iwres=F, wres=F, idv=F,cwres=F)
    if(use.log){
      loglist['dv']<-T
      loglist['pred']<-T
      loglist['ipred']<-T
    }
    
    
    xplot1 <- dv.vs.pred(object,
                         main=NULL,
                         pass.plot.list=TRUE,
                         logx=loglist['pred'], logy=loglist['dv'],
                         ...)
                         
    xplot2 <- dv.vs.ipred(object,
                          main=NULL,
                          pass.plot.list=TRUE,
                          logx=loglist['ipred'], logy=loglist['dv'],
                          ...)
                          
    xplot3 <- absval.iwres.vs.ipred(object,
                                 main=NULL,
                                 #ids=FALSE,
                                 pass.plot.list=TRUE,
                                 logx=loglist['ipred'], logy=loglist['iwres'], 
                                 ...)


    xplot4 <- wres.vs.idv(object,
                          main=NULL,
                          pass.plot.list=TRUE,
                          logx=loglist['idv'], logy=loglist['wres'], 
                          ...)


    if(use.cwres){
      xplot4 <- cwres.vs.idv(object,
                             main=NULL,
                             pass.plot.list=TRUE,
                             logx=loglist['idv'], logy=loglist['cwres'],
                             ...) 
    }
              
    plotList[[1]] <- xplot1
    plotList[[2]] <- xplot2
    plotList[[3]] <- xplot3
    plotList[[4]] <- xplot4      

    default.plot.title <- "Basic goodness-of-fit plots"
    plotTitle <- xpose.multiple.plot.title(object=object,
                                           plot.text = default.plot.title,
                                           main=main,
                                           ...)
    obj <- xpose.multiple.plot(plotList,plotTitle,...)
    return(obj)

  }

