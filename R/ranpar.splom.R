#' @describeIn  par_cov_splom A scatterplot matrix of random parameters
#' @export 
ranpar.splom <- function(object,  
                      main = xpose.multiple.plot.title(object=object,
                        plot.text = "Scatterplot matrix of random parameters",
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
  
  if(any(is.null(xvardef("ranpar",object)))) {
    return(cat("ETAs are not defined in the current database!\n"))
  }
  
  if(is.null(varnames)) {
    varnames <- c()
    for (i in xvardef("ranpar", object)) {
      varnames <- c(varnames, xlabel(i, object))
    }
  }
  
  xplot <- xpose.plot.splom(xvardef("ranpar", object),
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
