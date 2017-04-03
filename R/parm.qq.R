#' @describeIn  par_cov_qq parameter distributions
#' @export
parm.qq <-
  function(object,
           onlyfirst=TRUE,
           main="Default",
           ...) {
    
    
    ## is everything in place?
    if(any(is.null(xvardef("parms",object)))) {
      return(cat("Parameters are not defined in the current database!\n"))
    } 
    
    
    ## create enpty list for plots
    number.of.plots <- 0
    for (i in xvardef("parms", object)) {
      if(!is.factor(object@Data[[i]])){
        number.of.plots <- number.of.plots + 1
      }
    }    
    plotList <- vector("list",number.of.plots)
    plot.num <- 0 # initialize plot number
    
    ## loop (ranpar)
    for (i in xvardef("parms", object)) {      
      
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
    
    default.plot.title <- "Distribution of parameters"
    plotTitle <- xpose.multiple.plot.title(object=object,
                                           plot.text = default.plot.title,
                                           main=main,
                                           ...)
    obj <- xpose.multiple.plot(plotList,plotTitle,...)
    return(obj)
    
  }

