#' Function to create mirror plots from the generic Xpose plotting commands
#' 
#' This function takes the generic plotting functions from Xpose 4 and calls
#' them multiple times with the current arguments to the functions, changing
#' the arguments as needed for mirror plotting.
#' 
#' mostly and internal function for Xpose
#' 
#' @param fun The function name that we will call multiple times
#' @param arg.list The arguments to that function
#' @param mirror The type of mirror plots desired (1 or 3 mirror plots can be
#' created)
#' @param plotTitle The title for the plots
#' @param fix.y.limits Should we fix all the y axes to be the same?
#' @param fix.x.limits Should we fix all the x axes to be the same?
#' @param \dots additional arguments passed to the function.
#' @return a list of plots, or NULL.
#' @author Andrew Hooker
#' @seealso \code{\link{xpose.plot.default}},
#' \code{\link{xpose.plot.histogram}}, \code{\link{xpose.plot.qq}},
#' \code{\link{xpose.plot.splom}}
#' @keywords internal
create.mirror <- function(fun,arg.list,mirror,plotTitle,
                               fix.y.limits=TRUE,
                               fix.x.limits=TRUE,
                               ...){
  
  ## what sort of mirror do we have?
  if(mirror) {      
    if(!is.logical(mirror)) {
      if(mirror != 1 && mirror !=3) {
        cat("The mirror should either be logical, 1 or 3!\n")
        invisible()
        return(NULL)
      }
    } else {
      mirror <- 1
    }
  }

  arg.list$mirror=FALSE
  arg.list$...=NULL
  
  if (length(grep("par.strip.text",deparse(match.call())))>0){
    par.strip.text.exists <- TRUE
  } else {
    par.strip.text.exists <- FALSE
  }

  if (length(grep("auto.key",deparse(match.call())))>0){
    auto.key.exists <- TRUE
  } else {
    auto.key.exists <- FALSE
  }


  ## if strip was not supplied by user and exists then get rid of argument
  if(!is.null(arg.list$strip)){
    if(!is.null(arg.list$mirror.internal$strip.missing)){
      if(arg.list$mirror.internal$strip.missing){
        arg.list$strip=NULL
      }
    }
  }
  
  ## Set the seed number and decide what simulated data sets to plot
  if(!is.null(arg.list$seed)) set.seed(arg.list$seed)
  rand.samp <- sample(1:arg.list$object@Nsim,mirror)

  ## size of labels
  if(mirror==3) {
    if(is.null(arg.list$y.cex)) arg.list$y.cex=0.6
    if(is.null(arg.list$x.cex)) arg.list$x.cex=0.6
  }          
  if(mirror==1) {
    arg.list$y.cex=0.8
    arg.list$x.cex=0.8
  }

  if(mirror==1){
    if(is.null(arg.list$scales$cex)) arg.list$scales$cex=0.7
    if(is.null(arg.list$scales$tck)) arg.list$scales$tck=0.7
    #if(is.null(arg.list$scales$cex)) arg.list$scales=list(cex=0.7,tck=0.7)
    if(is.null(arg.list$main.cex)) arg.list$main.cex =0.9
    if(!par.strip.text.exists) arg.list$par.strip.text$cex =0.9
    if(!is.null(arg.list$auto.key)){
      if(arg.list$auto.key=="Default") arg.list$auto.key <- list(cex=0.8)
    }
  }
  
  if(mirror==3){
    if(is.null(arg.list$scales$cex)) arg.list$scales$cex=0.5
    if(is.null(arg.list$scales$tck)) arg.list$scales$tck=0.5
    #if(is.null(arg.list$scales$cex)) arg.list$scales=list(cex=0.5,tck=0.5)
    if(is.null(arg.list$main.cex)) arg.list$main.cex =0.7
    if(!par.strip.text.exists) arg.list$par.strip.text$cex =0.6
    if(!is.null(arg.list$auto.key)){
      if(arg.list$auto.key=="Default") arg.list$auto.key <- list(cex=0.6)
    }
  }

  ## aspect of graphs
  arg.list$aspect=arg.list$mirror.aspect

  
  total.plot.number <- mirror+1
  full.plot.list <- vector("list",total.plot.number)

  xlb <- arg.list$xlb
  for (j in 0:mirror){
    
    ## The plot titles
    if (j==0) arg.list$main  <- "Original data"
    if (j!=0) arg.list$main <- paste("Simulated data (#",rand.samp[j],")",sep="")

    ## The axis labels
    arg.list$xlb  <- xlb
    if (mirror==3 & (j==1 | j==2) ) arg.list$xlb  <- " "
    if (mirror==1 & j==0) arg.list$xlb  <- " "

    
    ## the simulation samples
    if (j==0) arg.list$samp=NULL
    if (j!=0) arg.list$samp=rand.samp[j]
    

    arg.string=NULL
    for(argnam in names(arg.list)){
      if (is.null(arg.string)){
        arg.string="list("
      }
      arg.string=paste(arg.string,argnam,"=arg.list$",argnam,",",sep="")
    }
    arg.string=paste(arg.string,"...)",sep="")    
    evaluated.arg.string <- eval(parse(text=arg.string))
    plot.val <- do.call(fun,evaluated.arg.string)

    full.plot.list[[j+1]] <- plot.val
  }

  
  if(fix.x.limits) xlimits=c()
  if(fix.y.limits) ylimits=c()
  
 
  
  
  for (j in 0:mirror){
    xplot <- full.plot.list[[j+1]]
    if(fix.x.limits) xlimits <- c(xlimits,xplot$x.limits)
    if(fix.y.limits) ylimits <- c(ylimits,xplot$y.limits)
  }
  if(!is.numeric(xlimits)) fix.x.limits <- FALSE
  if(!is.numeric(ylimits)) fix.y.limits <- FALSE
  if(fix.x.limits) new.xlimits <- c(min(xlimits),max(xlimits))
  if(fix.y.limits) new.ylimits <- c(min(ylimits),max(ylimits))
  
  for (j in 0:mirror){
    if(fix.x.limits) full.plot.list[[j+1]]$x.limits <- new.xlimits
    if(fix.y.limits) full.plot.list[[j+1]]$y.limits <- new.ylimits
  }
  
  ##if(is.null(arg.list$max.plots.per.page)) arg.list$max.plots.per.page=4
  if(arg.list$pass.plot.list){
    return(full.plot.list)
  } else {

    obj <- xpose.multiple.plot(full.plot.list,plotTitle=plotTitle,
                               #prompt=FALSE,#arg.list$prompt,
                               #max.plots.per.page=arg.list$max.plots.per.page,
                               mirror=mirror,
                               ...)
    return(obj)
  }
}
