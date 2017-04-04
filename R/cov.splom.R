#' @describeIn  par_cov_splom A scatterplot matrix of covariates
#' @export 
cov.splom <- function(object,  
                      main = xpose.multiple.plot.title(object=object,
                        plot.text = "Scatterplot matrix of covariates",
                        ...),
                      varnames  = NULL,
                                        #xlb = NULL,
                                        #ylb = NULL,
                      onlyfirst=TRUE,
                                        #inclZeroWRES=FALSE,
                                        #subset=xsubset(object),
                      smooth = TRUE,
                      lmline = NULL,
                                        #groups = NULL,
                                        #main.cex=NULL,
                      ...) {
  
  if(any(is.null(xvardef("covariates",object)))) {
    return(cat("Covariates are not defined in the current database!\n"))
  }
  
  if(is.null(varnames)) {
    varnames <- c()
    for (i in xvardef("covariates", object)) {
      varnames <- c(varnames, xlabel(i, object))
    }
  }

  
  xplot <- xpose.plot.splom(xvardef("covariates", object),
                            object,
                            varnames=varnames,
                            main = main,
                            onlyfirst = onlyfirst,
                                        #inclZeroWRES = inclZeroWRES,
                                        #subset = subset,
                                        #groups = groups,
                            smooth = smooth,
                            lmline = lmline,
                                        #ylb = ylb,
                                        #xlb = xlb,
                            ...)
  
  return(xplot)

}
