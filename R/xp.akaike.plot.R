#' GAM functions for Xpose 4
#' 
#' These are functions for summarizing and plotting the results of 
#' the generalized additive model within Xpose
#' 
#' @family GAM functions 
#'    
#' @param object An xpose.data object.
#' @param title A text string indicating plot title. If \code{NULL}, left
#' blank.
#' @param xlb A text string indicating x-axis legend. If \code{NULL}, left
#' blank.
#' @param ylb A text string indicating y-axis legend. If \code{NULL}, left
#' blank.
#' @param gam.object A GAM object (see \code{\link[gam]{gam}}.
#' @param plot.ids Logical, specifies whether or not ID numbers should be
#' displayed.
#' @param idscex ID label size.
#' @param ptscex Point size.
#' @param recur If dispersion should be used in the GAM object.
#' @param prompt Specifies whether or not the user should be prompted to press
#' RETURN between plot pages. Default is TRUE.
#' @param gamobj A GAM object to use in the plot. IF null then the user is
#' asked to choose from a list of GAM objects in memory.
#' @param \dots Other arguments passed to the GAM functions.
#' @return Plots or summaries.
#' @author Niclas Jonsson & Andrew Hooker
#' @seealso \code{\link[gam]{gam}}, \code{\link[lattice]{dotplot}}
#' @name GAM_summary_and_plot
NULL

#' @describeIn GAM_summary_and_plot An Akaike plot of the results.
#' @export
xp.akaike.plot <-
  function(gamobj=NULL,
           title = "Default",
           xlb = "Akaike value",
           ylb="Models",
           ...) {

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
    ##eval(parse(text=paste("current.gam","$steppit",sep="")))
    ##if(is.null(current.gam$steppit)) {
    if(is.null(eval(parse(text=paste("current.gam","$steppit",sep=""))))) {
      cat("This plot is not applicable without stepwise covariate selection.\n")
      return()
    }
    
    
    keep <- eval(parse(text=paste("current.gam","$keep",sep=""))) #current.gam$keep
    aic <- apply(keep, 2, function(x)
                 return(x$AIC))
    df.resid <- apply(keep, 2, function(x)
                      return(x$df.resid))
    term <- apply(keep, 2, function(x)
                  return(x$term))
    pdata <- data.frame(aic, df.resid, term)
    aic.ord <- order(pdata$aic)
    pdata <- pdata[aic.ord,  ]

    ##
    ## Select the 30 models with lowest AIC
    ##
    if(dim(pdata)[1] > 30){
      pdata1 <- pdata[1:30,  ]
      pdata2 <- pdata[1:30,  ]
    } else {
      pdata1 <- pdata
      pdata2 <- pdata
    }
    pdata1$term <- unclass(pdata1$term)
    pdata1$term <- reorder(as.factor(pdata1$term), pdata1$aic)
    names(pdata1$term) <- pdata2$term
    
    if(!is.null(title) && title == "Default") {
      title <- paste("AIC values from stepwise GAM search on ",
                     eval(parse(text=paste("current.gam","$pars",sep=""))),
                     #current.gam$pars,
                     " (Run ",
                     eval(parse(text=paste("current.gam","$runno",sep=""))),
                     #current.gam$runno,
                     ")",sep="")
    }
        
    xplot <- dotplot(term~aic,
                     pdata1,
                     main=title,
                     xlab=xlb,
                     ylab=ylb,
                     scales=list(cex=0.7,
                       tck=-0.01,
                       y=list(labels=pdata2$term,cex=0.6 )
                       ),
                     ...
                     )

    #print(xplot)
    return(xplot)
    #invisible()
    
    
  }
