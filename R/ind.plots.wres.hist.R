
#' Histograms of weighted residuals for each individual in an Xpose data
#' object, for Xpose 4
#' 
#' This is a compound plot consisting of histograms of the distribution of
#' weighted residuals (any weighted residual available from NONMEM) for every
#' individual in the dataset.  It is a wrapper encapsulating arguments to the
#' \code{\link{xpose.plot.histogram}} function.
#' 
#' Matrices of histograms of weighted residuals in each included individual are
#' displayed. \code{ind.plots.cwres.hist} is just a wrapper for
#' \code{ind.plots.wres.hist(object,wres="cwres").}
#' 
#' @param object An xpose.data object.
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  
#' @param wres Which weighted residual should we plot? Defaults to the WRES.
#' @param ylb A string giving the label for the y-axis. \code{NULL} if none.
#' @param layout A list giving the layout of the graphs on the plot, in columns
#' and rows. The default is 4x4.
#' @param inclZeroWRES Logical value indicating whether rows with WRES=0 is
#' included in the plot. The default is FALSE.
#' @param subset A string giving the subset expression to be applied to the
#' data before plotting. See \code{\link{xsubset}}.
#' @param scales see \code{\link{xpose.plot.histogram}}
#' @param aspect see \code{\link{xpose.plot.histogram}}
#' @param force.by.factor see \code{\link{xpose.plot.histogram}}
#' @param ids see \code{\link{xpose.plot.histogram}}
#' @param as.table see \code{\link{xpose.plot.histogram}}
#' @param hiborder the border colour of the histogram - an integer or string.
#' The default is black (see \code{\link[lattice]{histogram}}).
#' @param hicol the fill colour of the histogram - an integer or string.  The
#' default is blue (see \code{\link[lattice]{histogram}}).
#' @param hilty the border line type of the histogram - an integer.  The
#' default is 1 (see \code{\link[lattice]{histogram}}).
#' @param hilwd the border line width of the histogram - an integer.  The
#' default is 1 (see \code{\link[lattice]{histogram}}).
#' @param hidcol the fill colour of the density line - an integer or string.
#' The default is black (see \code{\link[lattice]{histogram}}).
#' @param hidlty the border line type of the density line - an integer.  The
#' default is 1 (see \code{\link[lattice]{histogram}}).
#' @param hidlwd the border line width of the density line - an integer.  The
#' default is 1 (see \code{\link[lattice]{histogram}}).
#' @param prompt Specifies whether or not the user should be prompted to press
#' RETURN between plot pages. Default is FALSE.
#' @param mirror Mirror plots are not yet implemented in this function and this
#' argument must contain a value of \code{NULL}
#' @param main.cex The size of the title.
#' @param max.plots.per.page Maximum number of plots per page
#' @param \dots Other arguments passed to \code{\link{xpose.plot.histogram}}.
#' @return Returns a compound plot comprising histograms of weighted residual
#' conditioned on individual.
#' @author E. Niclas Jonsson, Mats Karlsson, Justin Wilkins & Andrew Hooker
#' @seealso \code{\link{xpose.plot.histogram}},
#' \code{\link{xpose.panel.histogram}}, \code{\link[lattice]{histogram}},
#' \code{\link{xpose.prefs-class}}, \code{\link{xpose.data-class}}
#' @examples
#' ## Here we load the example xpose database 
#' xpdb <- simpraz.xpdb
#' 
#' ## A vanilla plot
#' ind.plots.wres.hist(xpdb)
#' 
#' ## subsets
#' ind.plots.wres.hist(xpdb, subset="ID<10 | ID>45",grid=TRUE)
#' 
#' ## plot the CWRES instead
#' ind.plots.cwres.hist(xpdb)
#' 
#' @export 
#' @family specific functions 

ind.plots.wres.hist <-
  function(object,
           main = "Default",
           wres="wres",
           #xlb  = NULL,
                                        # ylb  = xlabel(xvardef("dv",object),object),
           ylb = NULL,
           layout=c(4,4),
           inclZeroWRES=FALSE,
           subset=xsubset(object),
           scales=list(cex=0.7,tck=0.5),
           aspect="fill",
           force.by.factor=TRUE,
           ids=F,
           as.table=TRUE,
           hicol = object@Prefs@Graph.prefs$hicol,
           hilty = object@Prefs@Graph.prefs$hilty,
           hilwd = object@Prefs@Graph.prefs$hilwd,
           hidcol = object@Prefs@Graph.prefs$hidcol,
           hidlty = object@Prefs@Graph.prefs$hidlty,
           hidlwd = object@Prefs@Graph.prefs$hidlwd,
           hiborder = object@Prefs@Graph.prefs$hiborder,
           prompt = FALSE,
           mirror=NULL,
           main.cex=0.9,
           max.plots.per.page=1,
                                        #lty=c(0,1,1),
                                        #pch=c(21,32,32),
                                        #type="o",
                                        #col=c(1,object@Prefs@Graph.prefs$smcol,object@Prefs@Graph.prefs$lmcol),
                                        #lwd=1,
           ...) {

    ## Make sure we have the necessary variables defined in the ##
    ## object.                                                  ##
    if(is.null(check.vars(c("id",wres),object))) {
      return(NULL)
    }
    

    ## check for mirror
    if(!is.null(mirror)){
      cat("Mirror not currently implemented for individual plots\n")
      return()
    }

    data <- Data(object,inclZeroWRES,subset=subset)
    
    ## Bin them
    list.id   <- unique(data[[xvardef("id",object)]])
    length.id <- length(list.id)
    plots.per.page <- layout[1] * layout[2]
    plots.cur <- 0
    pages <- 1
    page.breaks <- c(0)
    old.obj <- object
    new.obj <- object
    new.obj@Data <- NULL
    
    for (i in list.id) {
      plots.cur <- plots.cur + 1
      if (plots.cur == plots.per.page) {
        pages <- pages + 1
        plots.cur <- 0
        page.breaks <- c(page.breaks, i)
      }
    }
    if (max(page.breaks) < max(list.id)) {
      page.breaks <- c(page.breaks, max(list.id))
    }
    data$bin <- cut(data$ID, page.breaks, include.lowest=T)
    id.levels <- levels(data$bin)
    

    plot.num <- 0
    plotList <- vector("list",length(id.levels))     
    for (i in id.levels) {    ## start loop
      
      new.obj@Data <- data[data$bin==i,] #subset(data, bin == i)

      ## Set up the data ##
      ## nobj <- new("xpose.data",
      ##             Runno=object@Runno,
      ##             Data = NULL 
      ##             )
      ## Data(nobj) <- Data(new.obj,inclZeroWRES=inclZeroWRES,
      ##                    subset=subset)

      if(is.null(xvardef(wres,object))){
        plotvar <- wres
      }else{
        plotvar <- xvardef(wres,object)
      }

      ## Fix any main and/or axis titles
      default.plot.title <- paste("Individual plots of", plotvar, sep=" ")
      plotTitle <- xpose.multiple.plot.title(object=object,
                                             plot.text = default.plot.title,
                                             main=main,
                                             ...)
      
                                        # Set y axis title
##       if (is.null(xlb)) {
##         xlb <- xlabel(xvardef("wres",object),object)
##       }
##       if (is.null(ylb)) {
##         ylb <- "Proportion"
##       }
      
      
      xplot <- xpose.plot.histogram(plotvar,#xvardef("wres",nobj),
                                    new.obj,
                                    #xlb = xlb,
                                    #ylb = ylb,
                                    by=xvardef("id",new.obj),
                                    main=plotTitle,
                                        #group="ind",
                                    layout=layout,
                                    scales=scales,
                                    aspect=aspect,
                                    xvar = plotvar,#xvardef("wres",object),
                                    force.by.factor=force.by.factor,
                                    ids=ids,
                                    subset=subset,
                                    as.table=as.table,
                                    hicol = hicol,
                                    hilty = hilty,
                                    hilwd = hilwd,
                                    hidcol = hidcol,
                                    hidlty = hidlty,
                                    hidlwd = hidlwd,
                                    hiborder = hiborder,
                                    main.cex=main.cex,
                                    ...)
      plot.num <- plot.num+1
      plotList[[plot.num]] <- xplot
      

    }

    obj <- xpose.multiple.plot(plotList,max.plots.per.page=max.plots.per.page,plotTitle=NULL,prompt=prompt,...)
    return(obj)


  }
