#' Plot the parameter or covariate distributions using a histogram
#' 
#' These functions plot the parameter or covariate values stored in an Xpose
#' data object using histograms.
#' 
#' Each of the parameters or covariates in the Xpose data object, as specified
#' in \code{object@Prefs@Xvardef$parms}, \code{object@Prefs@Xvardef$covariates}
#' or \code{object@Prefs@Xvardef$ranpar} is evaluated in turn, creating a stack
#' of histograms.
#' 
#' A wide array of extra options controlling histograms are available. See
#' \code{\link{xpose.plot.histogram}} for details.
#' 
#' @param object An xpose.data object.
#' @param onlyfirst Logical value indicating if only the first row per
#' individual is included in the plot.
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  For \code{"Default"} the function
#' \code{\link{xpose.multiple.plot.title}} is used.
#' @param \dots Other arguments passed to \code{\link{xpose.plot.histogram}}.
#' @return Delivers a stack of histograms.
#' @author Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.plot.histogram}},
#' \code{\link{xpose.panel.histogram}}, \code{\link[lattice]{histogram}},
#' \code{\link{xpose.data-class}}, \code{\link{xpose.prefs-class}}
#' @keywords methods
#' @examples
#' 
#' ## Here we load the example xpose database 
#' xpdb <- simpraz.xpdb
#' 
#' ## Parameter histograms
#' parm.hist(xpdb)
#' 
#' ## Covariate distribution, in green
#' cov.hist(xpdb, hicol=11, hidcol="DarkGreen", hiborder="White")
#' 
#' ## Random parameter histograms
#' ranpar.hist(xpdb)
#' 
#' @name  par_cov_hist
#' @family specific functions 
NULL

#' @describeIn  par_cov_hist Covariate distributions
#' @export
cov.hist <-
  function(object,
           onlyfirst=TRUE,
           main="Default",
           
           ...) {
    
    
    if(any(is.null(xvardef("covariates",object)))) {
      return(cat("No covariates defined in the current database!\n"))
    }
    
    
    
    
    
    ## create enpty list for plots
    number.of.plots <- 0
    for (i in xvardef("covariates", object)) {
      number.of.plots <- number.of.plots + 1
    }    
    plotList <- vector("list",number.of.plots)
    plot.num <- 0 # initialize plot number
    
    ## loop 
    for (i in xvardef("covariates", object)) {      
      
      xplot <- xpose.plot.histogram(i,
                                    object,
                                    main=NULL,
                                    onlyfirst = onlyfirst,
                                    pass.plot.list=TRUE,
                                    ...)
      
      
      plot.num <- plot.num+1
      plotList[[plot.num]] <- xplot
    }
    
    
    default.plot.title <- "Distribution of covariates"
    plotTitle <- xpose.multiple.plot.title(object=object,
                                           plot.text = default.plot.title,
                                           main=main,
                                           ...)
    obj <- xpose.multiple.plot(plotList,plotTitle,...)
    return(obj)
    
  }
