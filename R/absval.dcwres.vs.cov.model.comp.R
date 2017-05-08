#' Model comparison plots, of absolute differences in goodness-of-fit
#' predictors against covariates, for Xpose 4
#' 
#' These functions plot absolute differences in PRED, IPRED, WRES, CWRES and
#' IWRES against covariates for two specified model fits.
#' 
#' Conditional weighted residuals (CWRES) may require some extra steps to
#' calculate. See \code{\link{compute.cwres}} for details.
#' 
#' A wide array of extra options controlling xyplots are available. See
#' \code{\link{xpose.plot.default}} for details.
#' 
#' @param object An xpose.data object.
#' @param object.ref An xpose.data object. If not supplied, the user will be
#' prompted.
#' @param type 1-character string giving the type of plot desired.  The
#' following values are possible, for details, see 'plot': '"p"' for points,
#' '"l"' for lines, '"o"' for overplotted points and lines, '"b"', '"c"') for
#' (empty if '"c"') points joined by lines, '"s"' and '"S"' for stair steps and
#' '"h"' for histogram-like vertical lines.  Finally, '"n"' does not produce
#' any points or lines.
#' @param ylb A string giving the label for the y-axis. \code{NULL} if none.
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  
#' @param \dots Other arguments passed to \code{link{xpose.plot.default}}.
#' @return Returns a stack of plots comprising comparisons of PRED, IPRED, WRES
#' (or CWRES) and IWRES for the two specified runs.
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.plot.default}},
#' \code{\link{xpose.panel.default}}, \code{\link[lattice]{xyplot}},
#' \code{\link{compute.cwres}}, \code{\link{xpose.prefs-class}},
#' \code{\link{xpose.data-class}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## We expect to find the required NONMEM run and table files for runs
#' ## 5 and 6 in the current working directory
#' xpdb5 <- xpose.data(5)
#' xpdb6 <- xpose.data(6)
#' 
#' ## A basic dWRES plot, without prompts
#' absval.dwres.vs.cov.model.comp(xpdb5, xpdb6)
#' 
#' ## Custom colours and symbols, no user IDs
#' absval.dpred.vs.cov.model.comp(xpdb5, xpdb6, cex=0.6, pch=8, col=1, ids=NULL)
#' }
#' 
#' 
#' @name  absval_delta_vs_cov_model_comp 
#' @family specific functions 
NULL

#' @describeIn  absval_delta_vs_cov_model_comp The absolute differences in individual predictions
#'  against covariates for two specified model fits.
#' @export
#' 
#' 
#' 
#' 
"absval.dcwres.vs.cov.model.comp" <-
  function(object, 
           object.ref = NULL,
           type = NULL,
           ylb=expression(paste("|", Delta, "CWRES|")),
           main="Default",
           #subset= xsubset(object),
           #ref.default = ".ref.db",
           ...) {
    
    if (is.null(object.ref)) {
      ref.list <- get.refrunno()
      if(exists(".ref.db")){
        object.ref <- eval(parse(text=".ref.db"))
      } else {
        return()
      }
      if(any(is.null(ref.list)))
        return()
    } 
    
                                        #ref.db <- ref.list$ref.db
                                        #ref.runno <- ref.list$ref.runno
    
    if(dim(object@Data)[1] != dim(object.ref@Data)[1]) {
      cat("The current database and the reference database do not have\n")
      cat("the same number of lines!\n")
      return()
    }

    if ((is.null(xvardef("idlab",object))) || (is.null(xvardef("cwres",object)))) {
      cat("The required variables (ID label, CWRES) are not set in the database!\n")
      return()
    } 
    
    if(any(is.null(xvardef("covariates",object)))) {
      return(cat("No covariates found in the current database!\n"))
    }

    object@Data$dCWRES <- abs(object@Data[,xvardef("cwres", object)] - object.ref@Data[,xvardef("cwres", object.ref)])
                                        #object@Data[,xvardef("pred", object)] <- abs(object@Data[,xvardef("pred", object)])


    ## create list for plots
    number.of.plots <- 0
    for (i in xvardef("covariates", object)) {
      number.of.plots <- number.of.plots + 1
    }
    plotList <- vector("list",number.of.plots)
    plot.num <- 0 # initialize plot number

    
    for (i in xvardef("covariates", object)) {
      
      
      ## |dPRED| vs covariates
      ##xlb <- i
      ##ylb <- paste("|dCWRES| (Run ", object@Runno, " - Run ",object.ref@Runno,")",sep="")
      ##main <- paste(ylb, " vs ", xlb, sep="")
      
      if (is.null(type)) {
        if (!is.factor(object@Data[,i])) {
          type <- "p"
        } else {
          type = object@Prefs@Graph.prefs$type
        }
      }
      
      xplot <- xpose.plot.default(i,
                                  "dCWRES",
                                  object,
                                  ##xlb = xlb,
                                  ylb = ylb,
                                  #main = NULL,
                                  type = type,
                                  pass.plot.list = TRUE,
                                  #subset=subset,
                                  ...)
      
      
      plot.num <- plot.num+1
      plotList[[plot.num]] <- xplot
    }
    
    
    ## |dPRED| vs covariates
    default.plot.title <- paste("|CWRES_(Run", object@Runno,
                                ") - CWRES_(Run",object.ref@Runno,
                                ")| vs. Covariates",sep="")

    plotTitle <- xpose.multiple.plot.title(object=object,
                                           plot.text = default.plot.title,
                                           no.runno=TRUE,
                                           main=main,
                                           ...)
    
    obj <- xpose.multiple.plot(plotList,plotTitle,...)
    return(obj)

}
