#' Function to create a histogram of results from the randomization test tool
#' (\code{randtest}) in \href{https://uupharmacometrics.github.io/PsN/}{PsN}
#' 
#' Reads results from the \code{randtest} tool in \href{https://uupharmacometrics.github.io/PsN/}{PsN}
#' and then creates a histogram.
#' 
#' 
#' @param results.file The location of the results file from the
#' \code{randtest} tool in \href{https://uupharmacometrics.github.io/PsN/}{PsN}
#' @param df The degrees of freedom between the full and reduced model used in
#' the randomization test.
#' @param p.val The p-value you would like to use.
#' @param main The title of the plot.
#' @param xlim The limits of the x-axis
#' @param PCTSlcol Color of the empirical line
#' @param vlcol Colors of the original and nominal line
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
#' randtest.hist(results.file="randtest_dir1/raw_results_run1.csv",df=2)
#' }
#' 
#' @export randtest.hist
randtest.hist <-
    function(results.file="raw_results_run1.csv",
             df=1,
             p.val=0.05,
             main="Default",
             xlim=NULL,
             PCTSlcol = "black",
             vlcol=c("red","orange"),
             ...)
{
    ##rm(list=ls())
    ##p.val <- 0.05
    ##df <- 2
    ##main="Default"
    ##results <- read.csv("randtest_dir1/raw_results_run1.csv")
    
    crit.val.nom <- qchisq(1-p.val, df=df)

    ##Read in all results
    results <- read.csv(results.file)

    ## check that classes are present
    #createXposeClasses()
    if (!isClass("xpose.data") || !isClass("xpose.prefs")) {
      createXposeClasses()
    }
    
    ## Create the object
    xpobj       <- new("xpose.data",
                       Runno="PsN Randomization Test",
                       Data = NULL)
    
    ## read local options
    if (is.readable.file("xpose.ini")) {
        xpobj <- xpose.read(xpobj, file="xpose.ini")
    } else {
        ## read global options
        rhome   <- R.home()
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

    results$ID <-1
    results$WRES <- 1
    num_na <- length(results$deltaofv[is.na(results$deltaofv)])
    if(num_na>0){
      warning("Removing ",num_na," NONMEM runs that did not result in OFV values")
      results <- results[!is.na(results$deltaofv),]
    }
    
    Data(xpobj,keep.structure=T) <- results[-c(1,2),]
        
    crit.val.emp <- quantile(xpobj@Data$deltaofv,probs=p.val)    

    orig = results$deltaofv[2]       #dOFV for original dataset

    xpose.plot.histogram("deltaofv",
                         xpobj,
                         bins.per.panel.equal=FALSE,
                                        #layout=layout,
                                        #vdline=if(showOriginal){c(o1[all.index])} else {NULL},
                                        #showMean=showMean,
                                        #showMedian=showMedian,
                         xlim = if(is.null(xlim)){c(min(c(orig,crit.val.emp,xpobj@Data$deltaofv))-1,
                             max(c(orig,crit.val.emp,xpobj@Data$deltaofv,0))+0.2)},
                         showPCTS=TRUE,
                         PCTS=c(p.val),
                         PCTSlcol = PCTSlcol,
                         vline=c(orig,-crit.val.nom),
                         vlcol=vlcol,
                         main=if(main=="Default"){"Change in OFV for Randomization Test"}else{main},
                         key=list(#title = "Critical value lines",
                             columns = 1,
                             lines = list(type="l",col =c(vlcol,PCTSlcol)),#,lty=c(vlty,PCTSlty)),
                             #lines = list(type="l",col =c("red","orange","black")),
                             text = list(c("Original data", "Nominal", "Empirical (rand. test)")),
                             ##space="right",
                             corner=c(0.05,0.95),
                             border=T,
                             #transparent=FALSE,
                             alpha.background=1,
                             background = "white"
                             ),
                         ...
                         )

}
                     



