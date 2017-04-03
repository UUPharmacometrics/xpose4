#' @describeIn  absval_delta_vs_cov_model_comp The absolute differences in individual predictions
#'  against covariates for two specified model fits.
#' @export
"absval.dipred.vs.cov.model.comp" <-
  function(object, 
           object.ref = NULL,
           type = NULL,
           ylb=expression(paste("|", Delta, "IPRED|")),
           main="Default",
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

    if ((is.null(xvardef("idlab",object))) || (is.null(xvardef("ipred",object)))) {
      cat("The required variables (ID label, IPRED) are not set in the database!\n")
      return()
    } 
    
    if(any(is.null(xvardef("covariates",object)))) {
      return(cat("No covariates found in the current database!\n"))
    }

    object@Data$dIPRED <- abs(object@Data[,xvardef("ipred", object)] -
                             object.ref@Data[,xvardef("ipred", object.ref)])
    ##object@Data[,xvardef("pred", object)] <- abs(object@Data[,xvardef("pred", object)])

    ## create list for plots
    number.of.plots <- 0
    for (i in xvardef("covariates", object)) {
      number.of.plots <- number.of.plots + 1
    }
    plotList <- vector("list",number.of.plots)
    plot.num <- 0 # initialize plot number
    
    for (i in xvardef("covariates", object)) {
      
            
      if (is.null(type)) {
        if (!is.factor(object@Data[,i])) {
          type <- "p"
        } else {
          type = object@Prefs@Graph.prefs$type
        }
      }
      
      xplot <- xpose.plot.default(i,
                                  "dIPRED",
                                  object,
                                  #xlb = xlb,
                                  ylb = ylb,
                                  #main = main,
                                  type = type,
                                  pass.plot.list=TRUE,,
                                  main=NULL,
                                  ...)

      plot.num <- plot.num+1
      plotList[[plot.num]] <- xplot
    }
    
    
    ## |dPRED| vs covariates
    default.plot.title <- paste("|IPRED_(Run", object@Runno,
                                ") - IPRED_(Run",object.ref@Runno,
                                ")| vs. Covariates",sep="")

    plotTitle <- xpose.multiple.plot.title(object=object,
                                           plot.text = default.plot.title,
                                           no.runno=TRUE,
                                           main=main,
                                           ...)
    
    obj <- xpose.multiple.plot(plotList,plotTitle,...)
    return(obj)

}

