
#' Quantile-quantile plots of weighted residuals for each individual in an
#' Xpose data object, for Xpose 4
#' 
#' This is a compound plot consisting of QQ plots of the distribution of
#' weighted residuals (any weighted residual produced by NONMEM) for every
#' individual in the dataset.  The function is a wrapper encapsulating
#' arguments to the \code{\link{xpose.plot.qq}} function.
#' 
#' Matrices of Q-Q plots of weighted residuals in each included individual are
#' displayed.
#' 
#' A wide array of extra options controlling Q-Q plots are available. See
#' \code{\link{xpose.plot.qq}} for details.
#' 
#' @param object An xpose.data object.
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  
#' @param wres Which weighted residual should we plot? Defaults to the WRES.
#' @param layout A list giving the layout of the graphs on the plot, in columns
#' and rows. The default is 4x4.
#' @param inclZeroWRES Logical value indicating whether rows with WRES=0 is
#' included in the plot. The default is FALSE.
#' @param subset A string giving the subset expression to be applied to the
#' data before plotting. See \code{\link{xsubset}}.
#' @param scales See \code{\link{xpose.plot.qq}}.
#' @param aspect See \code{\link{xpose.plot.qq}}.
#' @param force.by.factor See \code{\link{xpose.plot.qq}}.
#' @param ids See \code{\link{xpose.plot.qq}}.
#' @param as.table See \code{\link{xpose.plot.qq}}.
#' @param type 1-character string giving the type of plot desired.  The
#' following values are possible, for details, see 'plot': '"p"' for points,
#' '"l"' for lines, '"o"' for over-plotted points and lines, '"b"', '"c"') for
#' (empty if '"c"') points joined by lines, '"s"' and '"S"' for stair steps and
#' '"h"' for histogram-like vertical lines.  Finally, '"n"' does not produce
#' any points or lines.
#' @param col The color for lines and points. Specified as an integer or a text
#' string. A full list is obtained by the R command \code{colours()}. The
#' default is blue (col=4).
#' @param pch The plotting character, or symbol, to use. Specified as an
#' integer. See R help on \code{\link{points}}. The default is an open circle.
#' @param cex The amount by which plotting text and symbols should be scaled
#' relative to the default. 'NULL' and 'NA' are equivalent to '1.0'.
#' @param abllwd Line width of the line of identity.
#' @param abllty Line type of the line of identity.
#' @param ablcol Line colour of the line of identity.
#' @param prompt Specifies whether or not the user should be prompted to press
#' RETURN between plot pages. Default is FALSE.
#' @param mirror Mirror plots are not yet implemented in this function and this
#' argument must contain a value of \code{NULL}
#' @param main.cex The size of the title.
#' @param max.plots.per.page Maximum number of plots per page
#' @param \dots Other arguments passed to \code{link{xpose.plot.qq}}.
#' @return Returns a compound plot comprising QQ plots of weighted residuals
#' conditioned on individual.
#' @author E. Niclas Jonsson, Mats Karlsson, Justin Wilkins & Andrew Hooker
#' @seealso \code{\link{xpose.plot.qq}}, \code{\link{xpose.panel.qq}},
#' \code{\link{qqplot}}, \code{\link[lattice]{qqmath}},
#' \code{\link{xpose.prefs-class}}, \code{\link{xpose.data-class}}
#' @keywords methods
#' @examples
#' 
#' ind.plots.cwres.qq(simpraz.xpdb,subset="ID<18")
#' 
#' @export 
#' @family specific functions 

ind.plots.wres.qq <-
  function(object,
           main = "Default",
           wres="wres",
                                        #xlb  = NULL,
                                        # ylb  = xlabel(xvardef("dv",object),object),
                                        #ylb = NULL,
           layout=c(4,4),
           inclZeroWRES=FALSE,
           subset=xsubset(object),
           scales=list(cex=0.7,tck=0.5),
           aspect="fill",
           force.by.factor=TRUE,
           ids=F,
           as.table=TRUE,
           type="o",
           pch=object@Prefs@Graph.prefs$pch,
           col=object@Prefs@Graph.prefs$col,
           cex=object@Prefs@Graph.prefs$cex,
           abllty = object@Prefs@Graph.prefs$abllty,
           abllwd = object@Prefs@Graph.prefs$abllwd,
           ablcol = object@Prefs@Graph.prefs$ablcol,
           prompt = FALSE,
           main.cex=0.9,
           mirror=NULL,
           max.plots.per.page=1,
           ...) {

    ## check for mirror
    if(!is.null(mirror)){
      cat("Mirror not currently implemented for individual plots\n")
      return()
    }

    ## Make sure we have the necessary variables defined in the ##
    ## object.                                                  ##
    if(is.null(check.vars(c("id",wres),object,silent=F))) {
      return(NULL)
    }

    ## subset the data
    data <- Data(object,inclZeroWRES,subset=subset)

    ## get plotvar
    if(is.null(xvardef(wres,object))){
      plotvar <- wres
    }else{
      plotvar <- xvardef(wres,object)
    }

    
    ## Fix any main and/or axis titles
    default.plot.title <- paste("Individual Q-Q plots of", plotvar, sep=" ")
    plotTitle <- xpose.multiple.plot.title(object=object,
                                           plot.text = default.plot.title,
                                           main=main,
                                           ...)
    
    
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

      #new.obj@Data <- subset(data, bin == i)
      new.obj@Data <- data[data$bin==i,] #subset(data, bin == i)

      
      ## Set up the data ##
      ## Figure out what variables we have defined
      ## select <- xvardef("wres",object)
      
      ## numpans <- length(select)

      ## nobj <- new("xpose.data",
      ##             Runno=object@Runno,
      ##             Data = NULL 
      ##             )
      ## Data(nobj) <- Data(new.obj,inclZeroWRES=inclZeroWRES,
      ##                    subset=subset)
      
      
      xplot <- xpose.plot.qq(plotvar,
                             new.obj,
                             ##xlb = xlb,
                             ##ylb = ylb,
                             by=xvardef("id",new.obj),
                             main=plotTitle,
                             ##group="ind",
                             layout=layout,
                             scales=scales,
                             aspect=aspect,
                             xvar = plotvar,
                             force.by.factor=force.by.factor,
                             ids=ids,
                             pch=pch,
                                        #col=col,
                             abllty=abllty,
                             abllwd=abllwd,
                             ablcol=ablcol,
                             subset=subset,
                             as.table=as.table,
                             main.cex=main.cex,
                             ...)
      plot.num <- plot.num+1
      plotList[[plot.num]] <- xplot
    }

    obj <- xpose.multiple.plot(plotList,max.plots.per.page=max.plots.per.page,plotTitle=NULL,prompt=prompt,...)
    return(obj)

  }
