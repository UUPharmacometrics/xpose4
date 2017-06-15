#' @describeIn GAM_summary_and_plot Individual influence on GAM terms.
#' @export

"xp.ind.inf.terms" <-
  function(gamobj=NULL,
           xlb = NULL,
           ylb = NULL,
           plot.ids=TRUE,
           idscex=0.7,
           ptscex=0.7,
           prompt=TRUE,
           ...){
    
    if(is.null(gamobj)){
      gamobj <- check.gamobj()
      if(is.null(gamobj)){
        return()
      } else {
      }
    } else {
      c1 <- call("assign",pos=1, "current.gam", gamobj,immediate=T)
      eval(c1)
    }
    
    if(length(names(coefficients(eval(parse(text="current.gam")))))==0){
      cat("\nNo covariates found for this parameter\n")
      return()
    }
    
    ## if (length(current.gam$terms@term.labels)==0){
    ##   cat("\nNo covariates found for this parameter\n")
    ##   return()
    ## }
    
    ##assign(fr=0,"form",current.gam$Start.mod)
    ##cook <- data.frame(xp.cook(current.gam))
    cook <- data.frame(dfbetas(eval(parse(text="current.gam")))^2)
    cook <- cook[,-1]
    xvals <- seq(length = length(cook[, 1]))
    
    ## get range for plots
    ylm <- range(cook)
    
    ## add 10% to range
    ylmm <- diff(ylm)*0.05
    ylm[1]= ylm[1]-ylmm
    ##if (ylm[1]<0) ylm[1]=0
    ylm[2]= ylm[2]+ylmm
    
    
    
    
    
    ## Get the idlabs
    if(any(is.null(eval(parse(text="current.gam$data$ID"))))){
      ids <- "n"
    } else {
      ids <- eval(parse(text="current.gam$data$ID"))
    }
    
    ## create enpty list for plots
    plotList <- vector("list",length(cook[1,]))
    
    ## Loop over the terms
    for(i in 1:length(cook[1,])) {
      
      
      title <- NULL
      
      
      
      if(is.null(xlb)){
        xlbb <- "Index number (ID)"
      } else {
        xlbb <- xlb
      }
      if(is.null(ylb)) {
        ylbb <- paste(names(cook)[i])
      } else {
        ylbb <- ylb
      }
      
      xplot <- xyplot(cook[,i]~xvals,
                      ylab=ylbb,
                      xlab=xlbb,
                      ylim=ylm,
                      main=title,
                      aspect="fill",#1,
                      ##scales = list(cex=0.7,tck=-0.01),
                      ids = ids,
                      panel=
                        function(x,y,ids,...) {
                          if(!any(ids == "n")&& plot.ids==TRUE) {
                            addid(x,y,ids=ids,
                                  idsmode=TRUE,
                                  idsext =0.05,
                                  idscex = idscex,
                                  idsdir = "both")
                          } else {
                            panel.xyplot(x,y,cex=ptscex,col="black",...)
                          }
                        }
      )
      plotList[[i]] <- xplot
      
    }
    
    plotTitle <- paste("Inidividual influence (Cooks distance) on each GAM term\n",
                       "for ",
                       eval(parse(text="current.gam$pars")),
                       " (Run ",
                       eval(parse(text="current.gam$runno")), ")",
                       sep="")
    obj <- xpose.multiple.plot(plotList,plotTitle,prompt,...)
    return(obj)
    
  }
