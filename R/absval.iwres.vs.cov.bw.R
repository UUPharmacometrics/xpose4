

#' box and whisker plots of the absolute value of the 
#' individual weighted residuals vs. covariates
#' 
#'
#' @inheritParams xpose.plot.default
#'
#' @return An xpose.multiple.plot object
#' @export
#'
#' 
#' @family specific functions 
absval.iwres.vs.cov.bw <-
  function(object,
           
           xlb  = "|iWRES|",
           #ylb  = NULL,
           #onlyfirst=FALSE,
           #inclZeroWRES=FALSE,
           #subset=xsubset(object),
           #seed  = NULL,
           #bins  = 10,
           #samp  = NULL,
           #prompt = TRUE,
           main="Default",
           ...) {
    

    
    if(any(is.null(xvardef("covariates",object)))) {
      return(cat("There are no covariates defined in the database!\n"))
    }
    
    ## create list for plots
    number.of.plots <- 0
    for (i in xvardef("covariates", object)) {
      number.of.plots <- number.of.plots + 1
    }
    plotList <- vector("list",number.of.plots)
    plot.num <- 0 # initialize plot number
    
    for (i in xvardef("covariates", object)) {
    
      xplot <- xpose.plot.bw(xvardef("iwres",object),
                             i,
                             xlb = xlb,
                             object,
                             main = NULL,
                             ids=FALSE,
                             binvar = i,
                             funx="abs",
                             pass.plot.list = TRUE,
                             ...)      

      plot.num <- plot.num+1
      plotList[[plot.num]] <- xplot
    }
    
    default.plot.title <- paste("|",xlabel(xvardef("iwres",object),object),
                                "| vs ",
                                "Covariates", sep="")
    
    plotTitle <- xpose.multiple.plot.title(object=object,
                                           plot.text = default.plot.title,
                                           main=main,
                                           ...)
    obj <- xpose.multiple.plot(plotList,plotTitle,...)
    return(obj)

  }

