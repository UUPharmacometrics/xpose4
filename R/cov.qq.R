#' Plot the parameter or covariate distributions using quantile-quantile (Q-Q)
#' plots
#' 
#' These functions plot the parameter or covariate values stored in an Xpose
#' data object using Q-Q plots.
#' 
#' Each of the parameters or covariates in the Xpose data object, as specified
#' in \code{object@Prefs@Xvardef$parms}, \code{object@Prefs@Xvardef$ranpar} or
#' \code{object@Prefs@Xvardef$covariates}, is evaluated in turn, creating a
#' stack of Q-Q plots.
#' 
#' A wide array of extra options controlling Q-Q plots are available. See
#' \code{\link{xpose.plot.qq}} for details.
#' 
#' @param object An xpose.data object.
#' @param onlyfirst Logical value indicating if only the first row per
#' individual is included in the plot.
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  For \code{"Default"} the function
#' \code{\link{xpose.multiple.plot.title}} is used.
#' @param \dots Other arguments passed to \code{\link{xpose.plot.qq}}.
#' @return Delivers a stack of Q-Q plots.
#' @author Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.plot.qq}}, \code{\link{xpose.panel.qq}},
#' \code{\link[lattice]{qqmath}}, \code{\link{xpose.data-class}},
#' \code{\link{xpose.prefs-class}}
#' @keywords methods
#' @examples
#' 
#' ## Here we load the example xpose database 
#' xpdb <- simpraz.xpdb
#' 
#' ## parameter histograms
#' parm.qq(xpdb)
#' 
#' ## A stack of random parameter histograms
#' ranpar.qq(xpdb)
#' 
#' ## Covariate distribution, in green with red line of identity
#' cov.qq(xpdb, col=11, ablcol=2)
#' 
#' @name  par_cov_qq
#' @family specific functions 
NULL

#' @describeIn  par_cov_qq Covariate distributions
#' @export
cov.qq <-
  function(object,
           onlyfirst=TRUE,
           main="Default",
           ...) {
    
    
    ## is everything in place?
    if(any(is.null(xvardef("covariates",object)))) {
      return(cat("Covariates are not defined in the current database!\n"))
    } 
    
    
    ## create enpty list for plots
    number.of.plots <- 0
    for (i in xvardef("covariates", object)) {
      if(!is.factor(object@Data[[i]])){
        number.of.plots <- number.of.plots + 1
      }
    }    
    plotList <- vector("list",number.of.plots)
    plot.num <- 0 # initialize plot number
    
    ## loop (ranpar)
    for (i in xvardef("covariates", object)) {      
      
      if(!is.factor(object@Data[[i]])){
        xplot <- xpose.plot.qq(i,
                               object,
                               main=NULL,
                               onlyfirst=onlyfirst,
                               pass.plot.list=TRUE,
                               ...)
        
        plot.num <- plot.num+1
        plotList[[plot.num]] <- xplot
      }
    }
    
    default.plot.title <- "Distribution of covariates"
    plotTitle <- xpose.multiple.plot.title(object=object,
                                           plot.text = default.plot.title,
                                           main=main,
                                           ...)
    obj <- xpose.multiple.plot(plotList,plotTitle,...)
    return(obj)
    
  }

