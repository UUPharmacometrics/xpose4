# Xpose 4
# An R-based population pharmacokinetic/
# pharmacodynamic model building aid for NONMEM.
# Copyright (C) 1998-2004 E. Niclas Jonsson and Mats Karlsson.
# Copyright (C) 2005-2008 Andrew C. Hooker, Justin J. Wilkins, 
# Mats O. Karlsson and E. Niclas Jonsson.
# Copyright (C) 2009-2010 Andrew C. Hooker, Mats O. Karlsson and 
# E. Niclas Jonsson.

# This file is a part of Xpose 4.
# Xpose 4 is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation, either version 3
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.

# You should have received a copy of the GNU Lesser General Public License
# along with this program.  A copy can be cound in the R installation
# directory under \share\licenses. If not, see http://www.gnu.org/licenses/.



#' The generic Xpose functions for box-and-whisker plots
#' 
#' This is a wrapper function for the lattice \code{\link[lattice]{bwplot}}
#' function.
#' 
#' 
#' @param x Name(s) of the x-variable.
#' @param y Name(s) of the y-variable.
#' @param object An xpose.data object.
#' @param inclZeroWRES A logical value indicating whether rows with WRES=0
#' should be plotted.
#' @param onlyfirst A logical value indicating whether only the first row per
#' individual should be included in the plot.
#' @param samp An integer between 1 and object@Nsim
#' (see\code{\link{xpose.data-class}}) specifying which of the simulated data
#' sets to extract from SData.
#' @param panel The name of the panel function to use. This should in most
#' cases be left as \code{\link{xpose.panel.bw}}.
#' @param groups A string with the name of any grouping variable (used as the
#' groups argument to \code{\link[lattice]{panel.xyplot}}.
#' @param ids A logical value indicating whether text labels should be used as
#' plotting symbols (the variable used for these symbols indicated by the
#' \code{idlab} Xpose data variable).
#' @param logy Logical value indicating whether the y-axis should be
#' logarithmic.
#' @param logx Logical value indicating whether the x-axis should be
#' logarithmic.
#' @param aspect The aspect ratio of the display (see
#' \code{\link[lattice]{bwplot}}).
#' @param funy String with the name of a function to apply to the y-variable
#' before plotting, e.g. "abs".
#' @param funx String with the name of a function to apply to the x-variable
#' before plotting, e.g. "abs".
#' @param PI Either "lines", "area" or "both" specifying whether prediction
#' intervals (as lines, as a shaded area or both) should be computed from the
#' data in \code{SData} and added to the display. \code{NULL} means no
#' prediction interval.
#' @param by A string or a vector of strings with the name(s) of the
#' conditioning variables.
#' @param force.by.factor Logical value. If TRUE, and \code{by} is not
#' \code{NULL}, the variable specified by \code{by} is taken as categorical.
#' @param ordby A string with the name of a variable to be used to reorder any
#' factor conditioning variables (\code{by}). The variable is used in a call to
#' the \code{reorder} function.
#' @param byordfun The name of the function to be used when reordering a factor
#' conditioning variable (see argument \code{ordby}).
#' @param shingnum The number of shingles ("parts") a continuous conditioning
#' variable should be divided into.
#' @param shingol The amount of overlap between adjacent shingles (see argument
#' \code{shingnum})
#' @param strip The name of the function to be used as the strip argument to
#' the \code{\link[lattice]{bwplot}}.
#' @param main A string giving the plot title or \code{NULL} if none.
#' @param xlb A string giving the label for the x-axis. \code{NULL} if none.
#' @param ylb A string giving the label for the y-axis. \code{NULL} if none.
#' @param subset A string giving the subset expression to be applied to the
#' data before plotting. See \code{\link{xsubset}}.
#' @param scales A list to be used for the \code{scales} argument in
#' \code{bwplot}.
#' @param suline A string giving the variable to be used to construct a smooth
#' to superpose on the display. \code{NULL} if none. This argument is used if
#' you want to add a superpose line of a variable not present in the \code{y}
#' list of variables.
#' @param binvar Variable to be used for binning.
#' @param bins The number of bins to be used. The default is 10.
#' @param mirror Should we create mirror plots from simulation data?  Value can
#' be \code{FALSE}, \code{TRUE} or \code{1} for one mirror plot, or \code{3}
#' for three mirror plots.
#' @param max.plots.per.page The maximum number of plots per page that can be
#' created with the mirror plots.
#' @param mirror.aspect The aspect ratio of the plots used for mirror
#' functionality.
#' @param pass.plot.list Should we pass the list of plots created with mirror
#' or should we print them directly.  Values can be \code{TRUE/FALSE}.
#' @param x.cex The size of the x-axis label.
#' @param y.cex The size of the y-axis label.
#' @param main.cex The size of the title.
#' @param mirror.internal an internal mirror argument used in
#' \code{\link{create.mirror}}.  Checks if the \code{strip} argument from
#' \code{\link[lattice]{bwplot}} has been used.
#' @param \dots Other arguments passed to \code{\link{xpose.panel.bw}}.
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.data-class}}, Cross-references above.
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## xpdb5 is an Xpose data object
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' 
#' ## Box & whisker plot of WRES vs PRED
#' xpose.plot.bw("WRES", "PRED", xpdb5, binvar="PRED")
#' }
#' 
#' @export xpose.plot.bw
"xpose.plot.bw" <-
  function(x,y,object,
           inclZeroWRES = FALSE,
           onlyfirst    = FALSE,
           samp         = NULL,
           panel        = xpose.panel.bw,
                                        #groups       = xvardef("id",object),
           groups       = NULL,
                                        #ids          = object@Prefs@Graph.prefs$ids,
           ids          = FALSE,
           logy         = FALSE,
           logx         = FALSE,
           aspect       = object@Prefs@Graph.prefs$aspect,
           funy          = NULL,
           funx         = NULL,
                                        # xvar         = NULL,

           ## Prediction interval settings
           PI           = FALSE,
           
           ## Conditioning settings
           by           = object@Prefs@Graph.prefs$condvar,
           force.by.factor = FALSE,
           ordby     = object@Prefs@Graph.prefs$ordby,
           byordfun  = object@Prefs@Graph.prefs$byordfun,
           shingnum  = object@Prefs@Graph.prefs$shingnum,
           shingol   = object@Prefs@Graph.prefs$shingol,
           strip = function(...)
           strip.default(...,strip.names=c(TRUE,TRUE)),
           ##par.strip.text=trellis.par.get("add.text"),

           ## Subset stuff
           subset       = xsubset(object),

           ## Axes and titles
                                        #main         = NULL,
           #main         = xpose.create.title(x,y,object,subset,funx,funy,...),
           #xlb          = xlabel(x,object),
           #ylb          = xlabel(y,object),
           main         = xpose.create.title(x,y,object,subset,funx,funy,...),
           xlb          = xpose.create.label(x,object,funx,logx,...),
           ylb          = xpose.create.label(y,object,funy,logy,...),
                                        #xlb          = NULL,
                                        #ylb          = NULL,
           
           scales       = list(),

           ## Superpose smooth
           suline       = object@Prefs@Graph.prefs$suline,
           

           ## bins
           binvar       = NULL,
           bins         = 10,

           ## mirror stuff
           mirror       = FALSE,
           max.plots.per.page=4,
           mirror.aspect="fill",
           pass.plot.list=FALSE,
           x.cex=NULL,
           y.cex=NULL,
           main.cex=NULL,
           mirror.internal=list(strip.missing=missing(strip)),
           ...) {

    plotTitle <- main

    ## for MIRROR functionality
    arg.list <- formals(xpose.plot.bw)
    arg.names <- names(arg.list)
    new.arg.list <- vector("list",length(arg.names))
    names(new.arg.list) <- arg.names
    for (argnam in arg.names){
      if (argnam=="..."){
        next
      }
      tmp <- get(argnam)
      if (is.null(tmp)){
      } else {
        new.arg.list[[argnam]]=tmp
      }
    }
    if (mirror){
      create.mirror(xpose.plot.bw,
                         new.arg.list,mirror,plotTitle,
                         fix.y.limits=FALSE,...)
    } else { # end if mirror


      
      ##Get data
      if(!is.null(samp)) {
        data <- SData(object,inclZeroWRES,onlyfirst=onlyfirst,
                      subset=subset,samp=samp)
      } else {
        data <- Data(object,inclZeroWRES,onlyfirst=onlyfirst,subset=subset)
      }
      
      ## Strip "missing" data
      data <- subset(data, get(x) != object@Prefs@Miss)
      data <- subset(data, get(y) != object@Prefs@Miss)
      
      if(any(is.null(data))) return("The subset expression is invalid.")
      
      if(any(is.na(data))) return("Problem.")
      
      y.bw <- xpose.bin(data, binvar, bins)
                                        #binvar <- y

      ## Make sure by is a factor if requested
      if(!is.null(by) && force.by.factor) {
        for(b in by) {
          data[,b] <- as.factor(data[,b])
        }
      }


      ## Check to see if x and y are both longer than 1
      if(length(x)>1 && length(y)>1) {
        cat("x and y can not both be longer than 1!\n")
        return()
      }
      
      ## Check to see if more that one x-variable
      if(length(x) > 1) {
        reps <-c(xvardef("id",object),xvardef("idlab",object),
                 xvardef("wres",object),y)
        
        if(!is.null(by)) reps <- c(reps,by)
        data <- xpose.stack(data,object,x,reps)
        object <- new("xpose.data",
                      Runno=object@Runno,
                      Data = NULL)
        Data(object) <- data

        onlyfirst = FALSE
        if(is.null(by)) {
          by <- "ind"
        } else {
          by <- c("ind",by)
        }

        x <- "values"

        ## If scales is longer than one then the users has supplied it
        ##as an argument.
        if(length(scales)==0) {
          scales=list(x=list(relation="free"))
        }
      }

      ## Check to see if more that one y-variable
      if(length(y) > 1) {
        reps <- c(object@Prefs@Xvardef["id"],
                  object@Prefs@Xvardef["idlab"],
                  xvardef("wres",object),x)
                
        if(!is.null(by)) reps <- c(reps,by)
        data <- xpose.stack(data,object,y,reps)
        object <- new("xpose.data",
                      Runno=object@Runno,
                      Data = NULL)
        Data(object) <- data

        onlyfirst = FALSE

        if(is.null(by)) {
          by <- "ind"
        } else {
          by <- c("ind",by)
        }

        y <- "values"

        ## If scales is longer than one then the users has supplied it
        ##as an argument.
        if(length(scales)==0) {
          scales=list(y=list(relation="free"))
        }
      }

      
      ## Collect the basic plot formula
      bb <- NULL
      groups <- groups
      if(any(is.null(by))) {
        formel <- paste("y.bw~",x,sep="")
                                        #cat(formel)
      } else {
        for(b in by) {
                                        #b <- by[bs]

          bb <- c(bb,xlabel(b,object))

          if(!is.factor(data[,b])) {
            data[,b] <- equal.count(data[,b],number=shingnum,overl=shingol)
          } else {

            if(any(!is.null(ordby))) {
              data[,b] <- reorder(data[,b],data[,ordby],byordfun)
            }

            if(names(data[,b,drop=F])!="ind") {
              levels(data[,b]) <-
                paste(xlabel(names(data[,b,drop=F]),object),":",   ## Needs to be fixed
                      levels(data[,b]),sep="")
            }
            
          }
        }
        bys    <- paste(by,collapse="*")
        formel <-  paste("y.bw~",x,"|",bys,sep="")
      }

      if(missing(strip)) {
        strip <- function(var.name,...)
          strip.default(var.name=bb,strip.names=c(F,T),...)
      }

      ## Check to see if panel.superpose should be used
      if(any(!is.null(groups))) groups <- data[,groups]

      ## CHeck to see if a superpose smooth is to be used.
      if(!is.null(suline)) {
        suline <- data[,suline]
      }
      
      ## Check for id-numbers as plotting symbols
      if(!is.null(ids)) ids <- data[,xvardef("idlab",object)]

      ## Apply function to y-variable
      if(!is.null(funy)) {
        data[,y] <- do.call(funy,list(data[,y]))
        
        if(ylb[1]==xlabel(y,object)) {
          ylb <- paste(funy," (",xlabel(y,object),")",sep="")
        }
      }
      ## Apply function to x-variable
      if(!is.null(funx)) {
        data[,x] <- do.call(funx,list(data[,x]))
        
        if(xlb[1]==xlabel(x,object)) {
          xlb <- paste(funx," (",xlabel(x,object),")",sep="")
        }
      }
      
      ## Sort out the scales
      if(logy) {
        scales$y$log <- TRUE
      }
      if(logx) {
        scales$x$log <- TRUE
      }

      xvarnam <- x
      yvarnam <- y

      if(!is.null(x.cex)) {
        if (is.list(xlb)){
          xlb$cex=x.cex
        } else {
          xlb <- list(xlb,cex=x.cex)
        }
      }
      if(!is.null(y.cex)) {
        if (is.list(ylb)){
          ylb$cex=y.cex
        } else {
          ylb <- list(ylb,cex=y.cex)
        }
      }

      if(is.null(main)) {
      } else {
        if(!is.null(main.cex)) {
          if (is.list(main)){
            main$cex=main.cex
          } else {
            main <- list(main,cex=main.cex)
          }
        }
      }

      xplot <- bwplot(formula(formel),data,obj=object,
                      prepanel = function(x,y) {
                        if(length(levs <- unique(x)) < object@Prefs@Cat.levels) {
                          xlim <- as.character(sort(levs))
                        } else {
                          xlim <- range(x)
                        }
                        list(xlim=xlim)
                      },
                      onlyfirst = onlyfirst,
                      bins = bins,
                      binvar = binvar,
                      samp   = samp,
                      panel = panel,
                      strip = strip,
                      ##par.strip.text = par.strip.text,
                      groups=groups,
                      inclZeroWRES=inclZeroWRES,
                      PI    = PI,
                      logy=logy,
                      logx=logx,
                      xvarnam = xvarnam,
                      yvarnam = yvarnam,
                      ids   = FALSE,
                      main=main,
                      xlab=xlb,
                      ylab=ylb,
                      aspect=aspect,
                      suline=suline,
                      scales=scales,
                      ...)
      return(xplot)

    }

  }
