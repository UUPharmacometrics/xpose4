
#' Function to create histograms of results from the \code{bootstrap} tool in
#' \href{https://uupharmacometrics.github.io/PsN/}{PsN}
#' 
#' Reads results from the \code{bootstrap} tool in \href{https://uupharmacometrics.github.io/PsN/}{PsN}
#' and then creates histograms.
#' 
#' 
#' @param results.file The location of the results file from the
#' \code{bootstrap} tool in \href{https://uupharmacometrics.github.io/PsN/}{PsN}
#' @param incl.ids.file The location of the included ids file from the
#' \code{bootstrap} tool in \href{https://uupharmacometrics.github.io/PsN/}{PsN}
#' @param min.failed Should NONMEM runs that had failed minimization be
#' skipped? \code{TRUE} or \code{FALSE}
#' @param cov.failed Should NONMEM runs that had a failed covariance step be
#' skipped? \code{TRUE} or \code{FALSE}
#' @param cov.warnings Should NONMEM runs that had covariance step warnings be
#' skipped? \code{TRUE} or \code{FALSE}
#' @param boundary Should NONMEM runs that had boundary warnings be skipped?
#' \code{TRUE} or \code{FALSE}
#' @param showOriginal Should we show the value from the original NONMEM run in
#' the histograms? \code{TRUE} or \code{FALSE}
#' @param showMean Should we show the mean of the histogram data? \code{TRUE}
#' or \code{FALSE}
#' @param showMedian Should we show the median of the histogram data?
#' \code{TRUE} or \code{FALSE}
#' @param showPCTS Should we show the percentiles of the histogram data?
#' \code{TRUE} or \code{FALSE}
#' @param PCTS the percentiles to show.  Can be a vector of any length.  For
#' example, \code{c(0.05,0.2,0.5,0.7)}
#' @param excl.id Vector of id numbers to exclude.
#' @param layout Layout of plots.  A vector of number of rows and columns in
#' each plot. \code{c(3,3)} for example.
#' @param sort.plots Should the plots be sorted based on type of parameter.
#' Sorting on parameters, standard errors, shrinkage and eigenvalues.
#' @param main The title of the plot.
#' @param \dots Additional arguments that can be passed to
#' \link{xpose.plot.histogram}, \link{xpose.panel.histogram},
#' \link[lattice]{histogram} and other \link[lattice]{lattice-package}
#' functions.
#' @return A lattice object
#' @author Andrew Hooker
#' @seealso \link{xpose.plot.histogram}, \link{xpose.panel.histogram},
#' \link[lattice]{histogram} and other \link[lattice]{lattice-package}
#' functions.
#' @references \href{https://uupharmacometrics.github.io/PsN/}{PsN}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' boot.hist(results.file="./boot1/raw_results_run1.csv", 
#'           incl.ids.file="./boot1/included_individuals1.csv")
#' }
#' 
#' @export
#' @importFrom utils read.csv
boot.hist <-
  function(results.file="raw_results_run1.csv",
           incl.ids.file="included_individuals1.csv",
           min.failed=FALSE,	    # do we want to omit minimization failed runs?
           cov.failed=FALSE,	    # do we want to omit covariance failed runs?
           cov.warnings=FALSE,       # do we want to omit covariance failed runs?
           boundary=FALSE,	   # do we want to omit boundary runs?
           showOriginal=TRUE,       # show line for original estimate
           showMean=FALSE,	   # show line for mean
           showMedian=FALSE,	    # show line for median
           showPCTS=FALSE,	   # show line for confidence interval (percentile)
           PCTS = c(0.025,0.975),  # vector of percentiles to plot, can be any length
           excl.id=c(),	       # exclude samples that have this individual
           layout = NULL, # layout for plot, e.g. c(3,3)
           sort.plots=TRUE,
           main = "Default",
           ...)
  {
    
    ## for testing
    ## results.file="./boot1/raw_results_run1.csv"
    ## incl.ids.file="./boot1/included_individuals1.csv"
    
    ## min.failed=FALSE       # do we want to omit minimization failed runs?
    ## cov.failed=FALSE       # do we want to omit covariance failed runs?
    ## cov.warnings=FALSE	 # do we want to omit covariance failed runs?
    ## boundary=FALSE	     # do we want to omit boundary runs?
    ## showoriginal=TRUE	# show line for original estimate
    ## showmean=TRUE	    # show line for mean
    ## showmedian=FALSE       # show line for median
    ## showPCTS=TRUE	    # show line for confidence interval (percentile)
    ## CI.limits = c(0.025, 0.975)
    ## showquart=FALSE	     # show line for quartiles
    ## excl.id=c()		# exclude samples that have this individual
    ## layout=c(3,3)
    
    ## read files
    bootstrap.data <- read.csv(results.file)
    incl.ids	   <- read.csv(incl.ids.file, header=F)
    
    ## replace underscores
    for (i in 1:length(names(bootstrap.data))) {
      names(bootstrap.data)[i] <- gsub("_", ".", names(bootstrap.data)[i])
    }
    
    ## find ofv column index
    index <- 0
    seen  <- FALSE
    
    for (i in names(bootstrap.data)) {
      if (!seen) {
        index <- index + 1
      }
      if (i == "ofv") {
        seen <- TRUE
      }
    }
    
    ## get number of parameters
    n	    <- length(colnames(bootstrap.data)) - index
    nparams <- length(colnames(bootstrap.data))
    
    
    ## separate out original model fit
    p1 <- subset(bootstrap.data, bootstrap.data$model != 0)
    o1 <- subset(bootstrap.data, bootstrap.data$model == 0)
    
    ## get all names
    all.index <- c(index:nparams)
    all.names <- names(p1)[all.index]
    
    ## get ofv index and name
    ofv.index <- grep("^ofv$",names(p1[index:nparams]))
    ofv.names <- names(p1[index:nparams])[ofv.index]
    ##ofv.names
    ##ofv.index
    
    ## get SE indexexes and names
    se.index <- grep("^se",names(p1[index:nparams]))
    se.names <- names(p1[index:nparams])[se.index]
    ##se.names
    ##se.index
    
    ## get eigenvalue indexexes and names
    ei.index <- grep("^EI",names(p1[index:nparams]))
    ei.names <- names(p1[index:nparams])[ei.index]
    ##ei.index
    ##ei.names
    
    ## get shrinkage indexexes and names
    sh.index <- grep("^shrinkage",names(p1[index:nparams]))
    sh.names <- names(p1[index:nparams])[sh.index]
    ##sh.index
    ##sh.names
    
    ## get parameter names and index
    par.names <- names(p1[index:nparams])[-c(ofv.index,sh.index,ei.index,se.index)]
    par.index <- grep("*",names(p1[index:nparams]))[-c(ofv.index,sh.index,ei.index,se.index)]
    ##par.names
    ##par.index
    
    incl.flag <- rep(0,length(rownames(p1)))
    for( i in excl.id ) {
      incl.flag <- incl.flag + rowSums( incl.ids == i )
    }
    
    p1 <- p1[(incl.flag==0),]
    
    #names(p1)[2] <- "minimization.successful"
    #names(p1)[3] <- "covariance.step.successful"
    #names(p1)[4] <- "covariance.step.warnings"
    #names(p1)[5] <- "estimate.near.boundary"
    
    #cat(nrow(p1))
    
    if (min.failed) {
      p1 <- p1[p1$minimization.successful==1,]
    }
    if (cov.failed) {
      p1 <- p1[p1$covariance.step.successful==1,]
    }
    if (cov.warnings) {
      p1 <- p1[p1$covariance.step.warnings==0,]
    }
    if (boundary) {
      p1 <- p1[p1$estimate.near.boundary==0,]
    }
    
    ## check that classes are present
    #createXposeClasses()
    if (!isClass("xpose.data") || !isClass("xpose.prefs")) {
      createXposeClasses()
    }
    
    ## Create the object
    xpobj	<- new("xpose.data",
                 Runno="PsN Bootstrap",
                 Data = NULL)
    
    ## read local options
    if (is.readable.file("xpose.ini")) {
      xpobj <- xpose.read(xpobj, file="xpose.ini")
    } else {
      ## read global options
      rhome	<- R.home()
      xdefini <- paste(rhome, "\\library\\xpose4\\xpose.ini", sep="")
      if (is.readable.file(xdefini)) {
        xpobj <- xpose.read(xpobj, file=xdefini)
      }else{
        xdefini2 <- paste(rhome, "\\library\\xpose4\\xpose.ini", sep="")
        if (is.readable.file(xdefini2)) {
          xpobj <- xpose.read(xpobj, file=xdefini2)
        }
      }
    }
    
    
    p1$ID <-1
    p1$WRES <- 1
    Data(xpobj) <- p1
    
    
    if(sort.plots){
      
      plot1 <- xpose.plot.histogram(par.names,xpobj,
                                    bins.per.panel.equal=FALSE,
                                    layout=layout,
                                    vdline=if(showOriginal){c(o1[all.index][par.index])} else {NULL},
                                    showMean=showMean,
                                    showMedian=showMedian,
                                    showPCTS=showPCTS,
                                    main=if(main=="Default"){"Parameters in Bootstrap"}else{main},
                                    ...)
      plot2 <- NULL
      plot4 <- NULL
      plot5 <- NULL
      if(!all(is.na(p1[se.names]))){
        plot2 <- xpose.plot.histogram(se.names,xpobj,
                                      bins.per.panel.equal=FALSE,
                                      layout=layout,
                                      vdline=if(showOriginal){c(o1[all.index][se.index])} else {NULL},
                                      showMean=showMean,
                                      showMedian=showMedian,
                                      showPCTS=showPCTS,
                                      main=if(main=="Default"){"Standard Errors of Parameters in Bootstrap"}else{main},
                                      ...)
      }
      plot3 <- xpose.plot.histogram(ofv.names,xpobj,
                                    bins.per.panel.equal=FALSE,
                                    layout=layout,
                                    vdline=if(showOriginal){c(o1[all.index][ofv.index])} else {NULL},
                                    showMean=showMean,
                                    showMedian=showMedian,
                                    showPCTS=showPCTS,
                                    main=if(main=="Default"){"Objective Function in Bootstrap"}else{main},
                                    ...)
      if(!all(is.na(p1[ei.names]))){
        plot4 <- xpose.plot.histogram(ei.names,xpobj,
                                      bins.per.panel.equal=FALSE,
                                      layout=layout,
                                      vdline=if(showOriginal){c(o1[all.index][ei.index])} else {NULL},
                                      showMean=showMean,
                                      showMedian=showMedian,
                                      showPCTS=showPCTS,
                                      main=if(main=="Default"){"Eigenvalues in Bootstrap"}else{main},
                                      ...)
      }
      if(!all(is.na(p1[sh.names]))){
        plot5 <- xpose.plot.histogram(sh.names,xpobj,
                                      bins.per.panel.equal=FALSE,
                                      layout=layout,
                                      vdline=if(showOriginal){c(o1[all.index][sh.index])} else {NULL},
                                      showMean=showMean,
                                      showMedian=showMedian,
                                      showPCTS=showPCTS,
                                      main=if(main=="Default"){"Shrinkage in Bootstrap"}else{main},
                                      ...)
      }
      return(list(plot1,plot2,plot3,plot4,plot5))
      
    }
    
    xpose.plot.histogram(all.names,xpobj,
                         bins.per.panel.equal=FALSE,
                         layout=layout,
                         vdline=if(showOriginal){c(o1[all.index])} else {NULL},
                         showMean=showMean,
                         showMedian=showMedian,
                         showPCTS=showPCTS,
                         main=if(main=="Default"){"Bootstrap Histogram"}else{main},
                         ...)
    
    
    #xpose.plot.histogram(all.names,xpobj,bins.per.panel.equal=FALSE,vdline=c(o1[all.index]))
    ## xpose.plot.histogram(names(p1)[index:nparams],xpobj,bins.per.panel.equal=FALSE,layout=layout)
    ## xpose.plot.histogram(names(p1)[index:nparams],xpobj,bins.per.panel.equal=FALSE,layout=c(4,5),showMean=T,showMedian=T,showPCTS=T)
    ## xpose.plot.histogram(names(p1)[index:nparams],xpobj,bins.per.panel.equal=FALSE,layout=c(4,5),vdline=c(o1[index:nparams]))
    
    ## xpose.plot.histogram(par.names,xpobj,bins.per.panel.equal=FALSE,vdline=c(o1[index:nparams][par.index]))
    
    ## xpose.plot.histogram(c("X1.BASELINE"),xpobj)
    ## xpose.plot.histogram(c("X1.BASELINE","X2.PLACEBO.MAX"),xpobj,bins.per.panel.equal=FALSE)
    
    ## plot.names <- names(p1)[index:nparams]
    ## xpose.plot.histogram(plot.names,xpobj,bins.per.panel.equal=FALSE,layout=c(3,3))
    
    ## xpose.plot.histogram(c("X1.BASELINE","ofv","X4.SLP","X2.PLACEBO.MAX","X3.PLACEBO.HL"),xpobj,bins.per.panel.equal=FALSE)
    ## xpose.plot.histogram(c("X1.BASELINE","ofv","X4.SLP","X3.PLACEBO.HL"),xpobj,bins.per.panel.equal=FALSE)
    
    ## xpose.plot.histogram(c("se1.BASELINE"),xpobj)
    
    ## xpose.plot.histogram(c("se1.BASELINE","X2.PLACEBO.MAX"),xpobj,bins.per.panel.equal=FALSE)
    ## xpose.plot.histogram(c("X1.BASELINE","X2.PLACEBO.MAX"),xpobj,bins.per.panel.equal=FALSE)
    
    ## xpose.plot.histogram(c("X1.BASELINE","X2.PLACEBO.MAX"),xpobj,bins.per.panel.equal=FALSE)
    ## xpose.plot.histogram(c("X1.BASELINE","X3.PLACEBO.HL"),xpobj,bins.per.panel.equal=FALSE)
    
    ## library(reshape2)
    
    ## #stack.params <- stack(ree.aod,select=c(thCl,thV,thMax,thE50,thHill))
    ## #stack.fix <- stack(ree.fix,select=c(thCl,thV,thMax,thE50,thHill))
    
    ## library(ggplot2)
    ## p <- ggplot(data=p1,aes(X1.BASELINE))
    ## p +  geom_histogram()
    
    ## + geom_jitter(position=position_jitter(width=0.05)) + facet_grid(~type)
    
    
    
    ## ## stats and plots for each- single
    ## for (i in index:nparams) {
    ##	   if (mode(p1[[i]]) == "numeric" &&
    ##	       sum(p1[[i]],na.rm=T)) {
    ##	       sp <- summary(p1[[i]])
    ##					   # IQR <- diff(summary(p1[[i]])[c(5,2)])
    ##	       dp <- density(p1[[i]], na.rm=T)
    ##	       parmlabel <- names(p1)[i]
    
    ##	       #pdf(file=paste("bootstrap.", parmlabel, ".pdf", sep=""), paper="special",
    ##	       #    title=paste("Bootstrap results - ", parmlabel, sep=""),width=10,height=7 )
    
    ##	       qu <- quantile(p1[[i]], CI.limits, na.rm=T)
    
    ##	       legend=paste("n = ", nrow(p1), sep="")
    ##	       if (showmean) {
    ##		   legend=paste(legend, "; Mean = ", sp[4], sep="")
    ##	       }
    ##	       if (showmedian) {
    ##		   legend=paste(legend, "; Median = ", sp[3], sep="")
    ##	       }
    ##	       if (showoriginal) {
    ##		   legend=paste(legend, "; Orig = ", o1[[i]], sep="")
    ##	       }
    
    
    ##	       hist(p1[[i]],
    ##		    main = paste("Bootstrap results - ", parmlabel, sep=""),
    ##		    xlab = parmlabel,
    ##					   # ylim = c(0, max(dp$y)),
    ##					   # ylim = c(0, 1),
    ##		    xlim = c(min(dp$x), max(dp$x)),
    ##		    breaks = 20,
    ##					   # xlim = c(min(p1[[i]]) - min(p1[[i]])*0.15,max(p1[[i]]) + max(p1[[i]])*0.15),
    ##		    probability = T,
    ##		    sub=legend )
    ##					   #	      sub=paste(paste(paste("n = ", nrow(p1), sep=""), "; Median = ", sp[4], sep=""), "; Orig = ", o1[[i]], sep="") )
    
    ##					   #h <- hist(p1[[i]], prob=T, plot=F)
    
    ##	       lines(dp, lwd=2, lty=3, col="red")
    
    ##	       if (showquart) {
    ##		   abline(v=sp[2], lwd= 1, lty=3, col="red") ## 1st quartile
    ##		   abline(v=sp[5], lwd= 1, lty=3, col="red") ## 3rd quartile
    ##	       }
    ##	       if (showmean) {
    ##		   abline(v=sp[4], lty=2, lwd=1, col="red") ## mean
    ##	       }
    ##	       if (showmedian) {
    ##		   abline(v=sp[3], lty=1, lwd=2, col="red") ## median
    ##	       }
    ##	       if (showoriginal) {
    ##		   abline(v=o1[[i]], lty=2, lwd=1, col="red") ## original
    ##	       }
    ##	       if (show95CI) {
    ##		   abline(v=qu[1], lty=4, lwd=1, col="red") ## 2.5% CL
    ##		   abline(v=qu[2], lty=4, lwd=1, col="red") ## 97.5% CL
    ##		   text(qu[1], max(dp$y), labels=signif(qu[1], digits = 3), cex = .8, adj = c(0,0), pos='2')
    ##		   text(qu[2], max(dp$y), labels=signif(qu[2], digits = 3), cex = .8, adj = c(0,0), pos='4')
    ##	       }
    ##					   #	abline(v=sp[4], lty=1, lwd=2, col="red") ## median
    ##					   #	abline(v=o1[[i]], lty=2, lwd=1, col="red") ## original
    ##					   #	abline(v=qu[1], lty=4, lwd=1, col="red") ## 2.5% CL
    ##					   #	abline(v=qu[2], lty=4, lwd=1, col="red") ## 97.5% CL
    
    ##					   #	text(qu[1], max(dp$y), labels=signif(qu[1], digits = 3), cex = .8, adj = c(0,0), pos='2')
    ##					   #	text(qu[2], max(dp$y), labels=signif(qu[2], digits = 3), cex = .8, adj = c(0,0), pos='4')
    ##					   #text(sp[4], max(h$density), labels=paste("Med: ", signif(sp[4], digits = 3), sep=""), adj = c(-1,0), cex = .8, pos='2')
    ##	   }
    ## }
    
    ## stats and plots for each - 6 per sheet
    
    ## total  <- 0
    ## bspage <- 0
    
    ## for (i in index:nparams) {
    ##	   if (mode(p1[[i]]) == "numeric" &&
    ##	       sum(p1[[i]],na.rm=T)) {
    ##	       sp <- summary(p1[[i]])
    ##					   # IQR <- diff(summary(p1[[i]])[c(5,2)])
    ##	       dp <- density(p1[[i]], na.rm=T)
    ##	       parmlabel <- names(p1)[i]
    
    ##	       if (total == 0) {
    ##		   bspage <- bspage + 1
    ##		   pdf(file=paste("bootstrap.page", bspage, ".pdf", sep=""), paper="special",
    ##		       title="Bootstrap results",width=10,height=7)
    ##		   par(mfrow = c(3,3))
    ##	       }
    ##	       total <- total + 1
    
    ##	       qu <- quantile(p1[[i]], c(0.025, 0.975), na.rm=T)
    
    ##	       legend=paste("n = ", nrow(p1), sep="")
    ##	       if (showmean) {
    ##		   legend=paste(legend, "; Mean = ", sp[3], sep="")
    ##	       }
    ##	       if (showmedian) {
    ##		   legend=paste(legend, "; Median = ", sp[4], sep="")
    ##	       }
    ##	       if (showoriginal) {
    ##		   legend=paste(legend, "; Orig = ", o1[[i]], sep="")
    ##	       }
    
    ##	       hist(p1[[i]],
    ##		    main = paste("Bootstrap results - ", parmlabel, sep=""),
    ##		    xlab = parmlabel,
    ##					   # ylim = c(0, max(dp$y)),
    ##					   # ylim = c(0, 1),
    ##		    xlim = c(min(dp$x), max(dp$x)),
    ##		    breaks = 20,
    ##					   # xlim = c(min(p1[[i]]) - min(p1[[i]])*0.15,max(p1[[i]]) + max(p1[[i]])*0.15),
    ##		    probability = T,
    ##		    sub=legend )
    ##					   #=paste(paste(paste("n = ", nrow(p1), sep=""), "; Median = ", sp[4], sep=""), "; Orig = ", o1[[i]], sep="") )
    
    ##					   #h <- hist(p1[[i]], prob=T, plot=F)
    
    ##	       lines(dp, lwd=2, lty=3, col="red")
    
    ##	       if (showquart) {
    ##		   abline(v=sp[2], lwd= 1, lty=3, col="red") ## 1st quartile
    ##		   abline(v=sp[5], lwd= 1, lty=3, col="red") ## 3rd quartile
    ##	       }
    ##	       if (showmean) {
    ##		   abline(v=sp[3], lty=2, lwd=1, col="red") ## mean
    ##	       }
    ##	       if (showmedian) {
    ##		   abline(v=sp[4], lty=1, lwd=2, col="red") ## median
    ##	       }
    ##	       if (showoriginal) {
    ##		   abline(v=o1[[i]], lty=2, lwd=1, col="red") ## original
    ##	       }
    ##	       if (show95CI) {
    ##		   abline(v=qu[1], lty=4, lwd=1, col="red") ## 2.5% CL
    ##		   abline(v=qu[2], lty=4, lwd=1, col="red") ## 97.5% CL
    ##		   text(qu[1], max(dp$y), labels=signif(qu[1], digits = 3), cex = .8, adj = c(0,0), pos='2')
    ##		   text(qu[2], max(dp$y), labels=signif(qu[2], digits = 3), cex = .8, adj = c(0,0), pos='4')
    ##	       }
    
    ##					   #	abline(v=sp[4], lty=1, lwd=2, col="red") ## median
    ##					   #	abline(v=o1[[i]], lty=2, lwd=1, col="red") ## original
    ##					   #	abline(v=qu[1], lty=4, lwd=1, col="red") ## 2.5% CL
    ##					   #	abline(v=qu[2], lty=4, lwd=1, col="red") ## 97.5% CL
    
    ##					   #	text(qu[1], max(dp$y), labels=signif(qu[1], digits = 3), cex = .8, adj = c(0,0), pos='2')
    ##					   #	text(qu[2], max(dp$y), labels=signif(qu[2], digits = 3), cex = .8, adj = c(0,0), pos='4')
    ##					   #text(sp[4], max(h$density), labels=paste("Med: ", signif(sp[4], digits = 3), sep=""), adj = c(-1,0), cex = .8, pos='2')
    
    ##	       if (total == 9) {
    ##		   total <- 0
    ##		   dev.off()
    ##	       }
    ##	   }
    ## }
    ## while(names(dev.cur())=="pdf"){dev.off()}
    ##					   #for(i in 1:17){
    ##					   #	dev.off()
    ##					   #}
  }
