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



#' The Xpose 4 generic functions for continuous y-variables.
#' 
#' This function is a wrapper for the lattice xyplot function.
#' 
#' \code{y} must be numeric (continuous) while \code{x} can be either numeric
#' of factor. If \code{x} is numeric then a regular xy-plot is drawn. If x is a
#' factor, on the other hand, a box and whiskers plot is constructed.
#' 
#' \code{x} and \code{y} can be either single valued strings or vector of
#' strings. \code{x} and \code{y} can not both be vectors inthe same call to
#' the function.
#' 
#' If \code{ids} is \code{TRUE}, text labels are added to the plotting symbols.
#' The labels are taken from the \code{idlab} xpose data variable. The way the
#' text labels are plotted is governed by the \code{idsmode} argument (passed
#' down to the panel function). \code{idsmode=NULL} (the default) means that
#' only extreme data points are labelled while a non-\code{NULL} value adds
#' labels to all data points (the default in Xpose 3).
#' \code{xpose.panel.default} identifies extreme data points by fitting a loess
#' smooth (\code{y~x}) and looking at the residuals from that fit. Points that
#' are associated with the highest/lowest residuals are labelled. "High" and
#' "low" are judged by the panel function parameter \code{idsext}, which gives
#' the fraction of the total number of data points that are to be judged
#' extreme in the "up" and "down" direction. The default value for
#' \code{idsext} is 0.05 (see \code{\link{xpose.prefs-class}}). There is also a
#' possibility to label only the high or low extreme points. This is done
#' through the \code{idsdir} argument to \code{xpose.panel.default}. A value of
#' "both" (the default) means that both high and low extreme points are
#' labelled while "up" and "down" labels the high and low extreme points
#' respectively.
#' 
#' Data dilution is useful is situations when there is an excessive amount of
#' data. \code{xpose.plot.default} can dilute data in two different ways. The
#' first is a completely random dilution in which all individuals are elegible
#' of exclusion from the plot. In this case the argument \code{dilfrac}
#' determines the fraction of individuals that are excluded from the plot. The
#' second type of dilution uses stratification to make sure that none of the
#' extreme individuals are omitted from the plot. Extreme individuals are
#' identified in a similar manner as extreme data points are identified for
#' text labelling. A smooth is fitted to the data and the extreme residuals
#' from that fit is used to inform about extremeness. What is judged as extreme
#' is determined by the argument \code{dilci}, which defaults to 0.95 (Note
#' that the meaning of this is the opposite to \code{idsext}). \code{dilci}
#' give the confidence level of the interval around the fitted curve outside of
#' which points are deemed to be extreme. Extreme individuals are those that
#' have at least one point in the "extremeness" interval. Individuals that do
#' not have any extreme points are elegible for dilution and \code{dilfrac}
#' give the number of these that should be omitted from the graph. This means
#' that \code{dilfrac} should usually be grater for stratified dilution than in
#' completely random dilution. Any smooths added to a diluted plot is based on
#' undiluted data.
#' 
#' More graphical parameters may be passed to
#' \code{\link{xpose.panel.default}}.
#' 
#' @param x A string or a vector of strings with the name(s) of the
#' x-variable(s).
#' @param y A string or a vector of strings with the name(s) of the
#' y-variable(s).
#' @param object An "xpose.data" object.
#' @param inclZeroWRES A logical value indicating whether rows with WRES=0
#' should be plotted.
#' @param onlyfirst A logical value indicating whether only the first row per
#' individual should be included in the plot.
#' @param samp An integer between 1 and object@Nsim
#' (see\code{\link{xpose.data-class}}) specifying which of the simulated data
#' sets to extract from SData.
#' @param panel The name of the panel function to use.
#' @param groups A string with the name of any grouping variable (used as the
#' groups argument to \code{panel.xyplot}.
#' @param ids A logical value indicating whether text labels should be used as
#' plotting symbols (the variable used for these symbols indicated by the
#' \code{idlab} xpose data variable).
#' @param logy Logical value indicating whether the y-axis should be
#' logarithmic.
#' @param logx Logical value indicating whether the x-axis should be
#' logarithmic.
#' @param yscale.components Used to change the way the axis look if \code{logy}
#' is used. Can be a user difined function or
#' \code{link{xpose.yscale.components.log10}}.  If the axes are not log
#' transformed then
#' \code{\link[lattice:axis.default]{yscale.components.default}} is used.
#' @param xscale.components Used to change the way the axis look if \code{logx}
#' is used. Can be a user difined function or
#' \code{link{xpose.xscale.components.log10}}.  If the axes are not log
#' transformed then
#' \code{\link[lattice:axis.default]{xscale.components.default}} is used.
#' @param aspect The aspect ratio of the display (see
#' \code{\link[lattice]{xyplot}}).
#' @param funx String with the name of a function to apply to the x-variable
#' before plotting, e.g. "abs".
#' @param funy String with the name of a function to apply to the y-variable
#' before plotting, e.g. "abs".
#' @param iplot Is this an indvidual plots matrix? Internal use only.
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
#' the \code{reorder.factor} function.
#' @param byordfun The name of the function to be used when reordering a factor
#' conditioning variable (see argument \code{ordby})
#' @param shingnum The number of shingles ("parts") a continuous conditioning
#' variable should be divided into.
#' @param shingol The amount of overlap between adjacent shingles (see argument
#' \code{shingnum})
#' @param by.interval The intervals to use for conditioning on a continuous
#' variable with \code{by}.
#' @param strip The name of the function to be used as the strip argument to
#' the \code{\link[lattice]{xyplot}}. An easy way to change the strip
#' appearance is to use \code{\link[lattice]{strip.custom}}.  For example, if
#' you want to change the text in the strips you can use
#' \code{strip=strip.custom(factor.levels=c("Hi","There"))} if the \code{by}
#' variable is a factor and \code{strip=strip.custom(var.name=c("New Name"))}
#' if the \code{by} variable is continuous.
#' @param use.xpose.factor.strip.names Use factor names in strips of
#' conditioning plots..
#' @param main A string giving the plot title or \code{NULL} if none.
#' @param xlb A string giving the label for the x-axis. \code{NULL} if none.
#' @param ylb A string giving the label for the y-axis. \code{NULL} if none.
#' @param subset A string giving the subset expression to be applied to the
#' data before plotting. See \code{\link{xsubset}}.
#' @param autocorr Is this an autocorrelation plot?  Values can be
#' \code{TRUE/FALSE}.
#' @param scales A list to be used for the \code{scales} argument in
#' \code{\link[lattice]{xyplot}}.
#' @param suline A string giving the variable to be used to construct a smooth
#' to superpose on the display. \code{NULL} if none. This argument is used if
#' you want to add a superpose line of a variable not present in the \code{y}
#' list of variables.
#' @param bwhoriz A logical value indicating if box and whiskers bars should be
#' plotted horizontally or not. Used when the x-variable(s) is categorical.
#' @param dilution Logical value indicating whether data dilution should be
#' used.
#' @param diltype Indicating what type of dilution to apply. \code{NULL} means
#' random dilution without stratification. A non\code{NULL} value means
#' stratified dilution.
#' @param dilfrac Dilution fraction indicating the expected fraction of
#' individuals to display in the plots. The exact meaning depends on the type
#' of dilution (see below).
#' @param dilci A number between 0 and 1 giving the range elegible for dilution
#' in a stratified dilution (see below).
#' @param seed Seed number used for random dilution. \code{NULL} means no seed.
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
#' \code{\link[lattice]{xyplot}} has been used.
#' @param \dots Other arguments passed to \code{\link{xpose.panel.default}}.
#' @return Returns a xyplot graph object.
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.panel.default}}, \code{\link[lattice]{xyplot}},
#' \code{\link[lattice]{panel.xyplot}}, \code{\link{xpose.prefs-class}},
#' \code{\link{xpose.data-class}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## xpdb5 is an Xpose data object
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' 
#' ## A spaghetti plot of DV vs TIME
#' xpose.plot.default("TIME", "DV", xpdb5) 
#' 
#' ## A conditioning plot
#' xpose.plot.default("TIME", "DV", xpdb5, by = "SEX")
#'  
#' ## Multiple x-variables
#' xpose.plot.default(c("WT", "SEX"), "CL", xpdb5)
#' 
#' ## Multiple y-variables
#' xpose.plot.default("WT", c("CL", "V"), xpdb5)
#' xpose.plot.default("WT", c("CL", "V"), xpdb5, by=c("SEX", "HCTZ"))
#' 
#' ## determining the interval for the conditioning variable
#' wt.ints <- matrix(c(50,60,60,70,70,80,80,90,90,100,100,150),nrow=6,ncol=2,byrow=T)
#' xpose.plot.default("TIME","DV",xpdb5,by="WT", by.interval=wt.ints)
#' }
#' 
#' 
#' @export xpose.plot.default
"xpose.plot.default" <-
  function(x,y,object,
           inclZeroWRES = FALSE,
           onlyfirst    = FALSE,
           samp         = NULL,
           panel        = xpose.panel.default,
           groups       = object@Prefs@Xvardef$id,
           ids          = object@Prefs@Graph.prefs$ids,
           logy         = FALSE,
           logx         = FALSE,
           yscale.components= "default",#function(...) yscale.components.default(...),
           xscale.components= "default",#function(...) xscale.components.default(...),
           
           aspect       = object@Prefs@Graph.prefs$aspect,
           funx         = NULL,
           funy         = NULL,
           iplot        = NULL,
           
           ## Prediction interval settings
           PI           = NULL,
           
           ## Conditioning settings
           by           = object@Prefs@Graph.prefs$condvar,
           force.by.factor = FALSE,
           ordby        = object@Prefs@Graph.prefs$ordby,
           byordfun     = object@Prefs@Graph.prefs$byordfun,
           shingnum     = object@Prefs@Graph.prefs$shingnum,
           shingol      = object@Prefs@Graph.prefs$shingol,
           by.interval  = NULL,
           ##par.strip.text=trellis.par.get("add.text"),
           ##mirror.par.strip.text=trellis.par.get("add.text"),
           strip = function(...){
             strip.default(...,strip.names=c(TRUE,TRUE))
           },
           use.xpose.factor.strip.names=TRUE,
           ##strip.nams=T,
           ##strip=strip.custom(strip.names=c(T,T)),
           ##par.strip.text = mirror.par.strip.text),
           ##par.strip.text = trellis.par.get("add.text"),
           ##par.strip.text=NULL,
           
           ## Subset stuff
           subset       = xsubset(object),
           
           autocorr=FALSE,
           
           ## Axes and titles
           main         = xpose.create.title(x,y,object,subset,funx,funy,...),
           #main         = NULL,
           xlb          = xpose.create.label(x,object,funx,logx,autocorr.x=autocorr,...),
           ylb          = xpose.create.label(y,object,funy,logy,autocorr.y=autocorr,...),
           ##xlb          = ifelse((length(x)>1),"Value",xlabel(x,object)),
           ##ylb          = ifelse((length(y)>1),"Value",xlabel(y,object)),
           scales       = list(),           
           
           ## Superpose smooth
           suline       = object@Prefs@Graph.prefs$suline,
           
           ## Categorical stuff
           bwhoriz      = object@Prefs@Graph.prefs$bwhoriz,
           
           ## Dilution stuff
           dilution     = FALSE,
           dilfrac      = object@Prefs@Graph.prefs$dilfrac,
           diltype      = object@Prefs@Graph.prefs$diltype,
           dilci        = object@Prefs@Graph.prefs$dilci,
           seed         = NULL,
           
           
           
           mirror       = FALSE,
           max.plots.per.page=4,
           mirror.aspect="fill",
           pass.plot.list=FALSE,
           x.cex        = NULL,
           y.cex        = NULL,
           main.cex     = NULL,
           mirror.internal=list(strip.missing=missing(strip)),
           ...
  ) {
    
    ## CHecks if use.xpose.factor.strip.names is a logical a length 1
    if (!(class(use.xpose.factor.strip.names)=="logical" & 
            length(use.xpose.factor.strip.names)==1)){
      stop("The provided use.xpose.factor.strip.names argument is not a logical of length 1")
    }
    
    plotTitle <- main
    
    ## for MIRROR functionality
    arg.list <- formals(xpose.plot.default)
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
      if(is.null(object@Nsim)) {
        cat(paste("The current Xpose database does not have any simulation data.\n"))
        cat(paste("The mirror option cannot be used.\n"))
        return(NULL)
      }
      create.mirror(xpose.plot.default,
                    new.arg.list,mirror,plotTitle,...)
    } else { # end if mirror
      
      
      ##Get data
      if(any(is.null(iplot))) {
        if(!is.null(samp)) {
          #cat(samp)
          data <- SData(object,inclZeroWRES,onlyfirst=onlyfirst,
                        subset=subset,samp=samp)
        } else {
          data <- Data(object,inclZeroWRES,onlyfirst=onlyfirst,subset=subset)
        }
      } else {
        data <- Data(object,inclZeroWRES,onlyfirst=onlyfirst,subset=NULL)
      }
      
      ## Strip "missing" data
      data <- subset(data, get(x) != object@Prefs@Miss)
      data <- subset(data, get(y) != object@Prefs@Miss)
      
      if(any(is.null(data))) return("The subset expression is invalid.")
      
      ## Make sure by is a factor if requested
      if(!is.null(by) && force.by.factor) {
        for(b in by) {
          data[,b] <- as.factor(data[,b])
        }
      }
      
      ## Sort out dilution
      dilsubset <- TRUE
      dilname   <- NULL
      if(dilution) {
        if(is.null(diltype)) { # Standard random dilution
          data <- create.rand(data,object,dilfrac,seed=seed)
          if(is.null(seed)) {
            dilsubset <- parse(text="Rnoseed==0")
            dilname   <- "Rnoseed"
          } else {
            dilsubset <- parse(text=paste("R",seed,"==0",sep=""))
            dilname   <- paste("R",seed,"==0",sep="")
          }
        } else {               # Stratified random dilution
          data <-create.strat.rand(data,object,x,y,dilfrac,dilci,seed=seed)
          if(is.null(seed)) {
            dilsubset <- parse(text="RSnoseed==0")
            dilname   <- "RSnoseed"
          } else {
            dilsubset <- parse(text=paste("RS",seed,,"==0",sep=""))
            dilname   <- paste("RS",seed,,"==0",sep="")
          }
        }
      }
      
      ## Check to see if x and y are both longer than 1
      if(length(x)>1 && length(y)>1) {
        cat("x and y can not both be longer than 1\n")
        return()
      }
      
      
      ## Check to see if more than one x-variable
      if(length(x) > 1) {
        reps <-c(xvardef("id",object),xvardef("idlab",object),
                 xvardef("wres",object),y,groups)
        if(!is.null(dilname)) reps <- c(reps,dilname)
        
        if(!is.null(by)) reps <- c(reps,by)
        #data <- stack.xpose(data,object,x,reps)
        data <- xpose.stack(data,object,x,reps)
        object <- new("xpose.data",
                      Runno=object@Runno,
                      Data = NULL,
                      Prefs = object@Prefs)
        
        Data(object) <- data
        #cat(object@Prefs@Graph.prefs$type)
        
        if(is.null(main.cex)) main.cex <- 0.9
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
      
      ## Check to see if more than one y-variable
      if(length(y) > 1) {
        reps <- c(object@Prefs@Xvardef["id"],
                  object@Prefs@Xvardef["idlab"],
                  xvardef("wres",object),x,groups)
        if(!is.null(dilname)) reps <- c(reps,dilname)
        
        if(!is.null(by)) reps <- c(reps,by)
        #data <- stack.xpose(data,object,y,reps)
        data <- xpose.stack(data,object,y,reps)
        object <- new("xpose.data",
                      Runno=object@Runno,
                      Data = NULL,
                      Prefs = object@Prefs)
        
        Data(object) <- data
        
        if(is.null(main.cex)) main.cex <- 0.9
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
        if(bwhoriz) {
          formel <- paste(x,"~",y,sep="")
        } else {
          formel <- paste(y,"~",x,sep="")
        }
      } else {
        for(b in by) {
          
          ##b <- by[bs]
          
          bb <- c(bb,xlabel(b,object))
          
          if(!is.factor(data[,b])) {
            if(is.null(by.interval)){
              data[,b] <- equal.count(data[,b],number=shingnum,overl=shingol)
            } else {
              data[,b] <- shingle(data[,b],intervals=by.interval)
            }
          } else {
            
            if(any(!is.null(ordby))) {
              data[,b] <- reorder(data[,b],data[,ordby],byordfun)
            }
            
            if(names(data[,b,drop=F])!="ind") {
              if(use.xpose.factor.strip.names){
                levels(data[,b]) <-
                  paste(xlabel(names(data[,b,drop=F]),object),":",   ## Needs to be fixed
                        levels(data[,b]),sep="")
              }
            }
          }
        }
        bys    <- paste(by,collapse="*")
        if(bwhoriz) {
          formel <-  paste(x,"~",y,"|",bys,sep="")
        } else {
          formel <-  paste(y,"~",x,"|",bys,sep="")
        }
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
      ##if(!is.null(ids)) ids <- data[,xvardef("idlab",object)]
      if(ids){
        ids <- data[,xvardef("idlab",object)]
      } else {
        ids <- NULL
      }
      
      ## Apply function to x-variable
      if(!is.null(funx)) {
        data[,x] <- do.call(funx,list(data[,x]))
      }
      
      ## Apply function to y-variable
      if(!is.null(funy)) {
        data[,y] <- do.call(funy,list(data[,y]))
        ##         if(!is.null(ylb[1])){
        ##           ##if(ylb[1]==xlabel(y,object)) {
        ##           if(missing(ylb)) {
        ##             if (fun=="abs"){
        ##               ylb <- paste("|",ylb,"|",sep="")
        ##             } else {
        ##               ylb <- paste(fun,"(",ylb,")",sep="")
        ##             }
        ##           }
        ##         }
      }
      
      ## Sort out the scales
      yscale.components.defined <- T
      xscale.components.defined <- T
      
      if(!is.function(yscale.components)){
        if(!is.na(match(yscale.components,"default"))) {
          yscale.components= function(...) yscale.components.default(...)
          yscale.components.defined <- F
        }
      }
      
      if(!is.function(xscale.components)){
        if(!is.na(match(xscale.components,"default"))) {
          xscale.components= function(...) xscale.components.default(...)
          xscale.components.defined <- F
        }
      }
      
      if(logy) {
        scales$y$log <- TRUE
        if(!yscale.components.defined){
          yscale.components=xpose.yscale.components.log10
        }
      }
      if(logx) {
        scales$x$log <- TRUE
        if(!xscale.components.defined){
          xscale.components=xpose.xscale.components.log10
        }
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
      
      
      ## for autocorrelation (not working completely yet)
      if(autocorr){
        auto.ids <- unique(data[[xvardef("id",object)]])
        auto.n <- 0
        xplt1 <- 0
        xplt2 <- 0
        xgrps <- 0
        for(i in 1:length(auto.ids)) {
          i <- 1
          seli <- data[[xvardef("id",object)]] == ids[i]
          nobs <- length(data[[x]][seli])
          xplt <- matrix(data[[x]][seli], 1, nobs)
          if(nobs > 1) {
            for(j in 1:(nobs - 1)) {
              auto.n <- auto.n + 1
              xplt1[auto.n] <- xplt[1, j]
              xplt2[auto.n] <- xplt[1, j + 1]
              xgrps[auto.n] <- auto.ids[i]
            }
          }
        }
        
        #xlb <- paste(xlb,"(i)",sep="")
        #ylb <- paste(ylb,"(i+1)",sep="")
        
        #x <- xplt1
        #y <- xplt2
        #groups <- xgrps
      }

      xplot <- xyplot(formula(formel),data,obj=object,
                      prepanel = function(x,y) {
                        xlim <- NULL
                        ylim <- NULL
                        ret <- list()
                        if(is.factor(x)){#length(levs <- unique(x)) < object@Prefs@Cat.levels) {
                          if(length(grep("[A-Z,a-z]",levels(x)))==0) {
                            xlim <- as.character(sort(as.numeric(levels(x))))
                          } else {
                            xlim <- sort(levels(x))
                          }
                        } else {
                          #xlim <- range(x)
                        }
                        ret[["xlim"]] <- xlim
                        if(is.factor(y)){#length(levs <- unique(x)) < object@Prefs@Cat.levels) {
                          if(length(grep("[A-Z,a-z]",levels(y)))==0) {
                            ylim <- as.character(sort(as.numeric(levels(y))))
                          } else {
                            ylim <- sort(levels(y))
                          }
                        } else {
                          #ylim <- range(y)
                        }
                        ret[["ylim"]] <- ylim
                        #list(xlim=xlim,ylim=ylim)
                        return(ret)
                      },
                      onlyfirst = onlyfirst,
                      samp   = samp,
                      panel = panel,
                      strip = strip,
                      ##par.strip.text = par.strip.text,
                      groups=groups,
                      inclZeroWRES=inclZeroWRES,
                      PI    = PI,
                      logy=logy,
                      logx=logx,
                      xscale.components=xscale.components,
                      yscale.components=yscale.components,
                      xvarnam = xvarnam,
                      yvarnam = yvarnam,
                      ids   = ids,
                      main=main,
                      xlab=xlb,
                      ylab=ylb,
                      aspect=aspect,
                      suline=suline,
                      bwhoriz = bwhoriz,
                      subset=eval(dilsubset),
                      scales=scales,
                      iplot=iplot,
                      autocorr=autocorr,
                      #autocorr=FALSE,
                      PI.subset=subset,
                      #drop.unused.levels=FALSE,
                      ...)
      return(xplot)
    }
  }

