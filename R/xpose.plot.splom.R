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



#' The Xpose 4 generic functions for scatterplot matrices.
#' 
#' This function is a wrapper for the lattice splom function.
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
#' \code{idsext} is 0.05 (see \code{link{xpose.prefs-class}}). There is also a
#' possibility to label only the high or low extreme points. This is done
#' through the \code{idsdir} argument to \code{xpose.panel.default}. A value of
#' "both" (the default) means that both high and low extreme points are
#' labelled while "up" and "down" labels the high and low extreme points
#' respectively.
#' 
#' More graphical parameters may be passed to \code{\link{xpose.panel.splom}}.
#' for example, if you want to adjust the size of the \code{varnames} and
#' \code{axis tick labels} you can use the parameters \code{varname.cex=0.5}
#' and \code{axis.text.cex=0.5}.
#' 
#' @param plist A vector of strings containing variable names for the
#' scatterplot matrix.
#' @param object An "xpose.data" object.
#' @param varnames A vector of strings containing labels for the variables in
#' the scatterplot matrix.
#' @param inclZeroWRES A logical value indicating whether rows with WRES=0
#' should be plotted.
#' @param onlyfirst A logical value indicating whether only the first row per
#' individual should be included in the plot.
#' @param panel The name of the panel function to use.
#' @param lmline logical variable specifying whether a linear regression line
#' should be superimposed over an \code{\link[lattice]{xyplot}}. \code{NULL} ~
#' FALSE. (\code{y~x})
#' @param smooth A \code{NULL} value indicates that no superposed line should
#' be added to the graph. If \code{TRUE} then a smooth of the data will be
#' superimposed.
#' @param groups A string with the name of any grouping variable (used as the
#' groups argument to \code{panel.xyplot}.
#' @param ids A logical value indicating whether text labels should be used as
#' plotting symbols (the variable used for these symbols indicated by the
#' \code{idlab} xpose data variable).
#' @param aspect The aspect ratio of the display (see
#' \code{\link[lattice]{xyplot}}).
#' @param by A string or a vector of strings with the name(s) of the
#' conditioning variables.
#' @param force.by.factor Logical value. If TRUE, and \code{by} is not
#' \code{NULL}, the variable specified by \code{by} is taken as categorical.
#' @param include.cat.vars Logical value.
#' @param ordby A string with the name of a variable to be used to reorder any
#' factor conditioning variables (\code{by}). The variable is used in a call to
#' the \code{reorder.factor} function.
#' @param byordfun The name of the function to be used when reordering a factor
#' conditioning variable (see argument \code{ordby})
#' @param shingnum The number of shingles ("parts") a continuous conditioning
#' variable should be divided into.
#' @param shingol The amount of overlap between adjacent shingles (see argument
#' \code{shingnum})
#' @param strip The name of the function to be used as the strip argument to
#' the \code{\link[lattice]{xyplot}}.
#' @param main A string giving the plot title or \code{NULL} if none.
#' @param xlb A string giving the label for the x-axis. \code{NULL} if none.
#' @param ylb A string giving the label for the y-axis. \code{NULL} if none.
#' @param subset A string giving the subset expression to be applied to the
#' data before plotting. See \code{\link{xsubset}}.
#' @param scales A list to be used for the \code{scales} argument in
#' \code{xyplot}.
#' @param mirror Should we create mirror plots from simulation data?  Value can
#' be \code{FALSE}, \code{TRUE} or \code{1} for one mirror plot, or \code{3}
#' for three mirror plots.
#' @param max.plots.per.page The maximum number of plots per page that can be
#' created with the mirror plots.
#' @param mirror.aspect The aspect ratio of the plots used for mirror
#' functionality.
#' @param samp An integer between 1 and object@Nsim
#' (see\code{\link{xpose.data-class}}) specifying which of the simulated data
#' sets to extract from SData.
#' @param pass.plot.list Should we pass the list of plots created with mirror
#' or should we print them directly.  Values can be \code{TRUE/FALSE}.
#' @param x.cex The size of the x-axis label.
#' @param y.cex The size of the y-axis label.
#' @param main.cex The size of the title.
#' @param mirror.internal an internal mirror argument used in
#' \code{\link{create.mirror}}.  Checks if the \code{strip} argument from
#' \code{\link[lattice]{qqmath}} has been used.
#' @param \dots Other arguments passed to \code{\link{xpose.panel.default}}.
#' @return Returns a scatterplot matrix graph object.
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.panel.splom}}, \code{\link[lattice]{splom}},
#' \code{\link[lattice]{panel.splom}}, \code{\link{xpose.prefs-class}},
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
#' ## CL, WT, HT, SEX with a regression line
#' xpose.plot.splom(c("CL", "WT", "HT", "SEX"), xpdb5, lmline = TRUE) 
#' }
#' 
#' 
#' @export xpose.plot.splom
"xpose.plot.splom" <-
  function(plist, object,
           varnames=NULL,
           main = "Scatterplot Matrix",
           xlb = NULL,
           ylb = NULL,
           scales = list(),
           onlyfirst=TRUE,
           inclZeroWRES=FALSE,
           subset = xsubset(object),
           by           = object@Prefs@Graph.prefs$condvar,
           force.by.factor=FALSE,
           include.cat.vars = FALSE,
           ordby     = NULL,
           byordfun  = object@Prefs@Graph.prefs$byordfun,
           shingnum  = object@Prefs@Graph.prefs$shingnum,
           shingol   = object@Prefs@Graph.prefs$shingol,
           strip = function(...)
           strip.default(...,strip.names=c(TRUE,TRUE)),
           #par.strip.text=trellis.par.get("add.text"),
           groups = NULL,
           ids = object@Prefs@Graph.prefs$ids,
           smooth       = TRUE,
           lmline = NULL,
           panel        = xpose.panel.splom,
           aspect = object@Prefs@Graph.prefs$aspect,
                                        #varname.cex=NULL,
                                        #axis.text.cex=NULL,
           ## mirror stuff
           samp=NULL,
           max.plots.per.page=4,
           mirror       = FALSE,
           mirror.aspect="fill",
           pass.plot.list=FALSE,
           x.cex=NULL,
           y.cex=NULL,
           main.cex=NULL,
           mirror.internal=list(strip.missing=missing(strip)),
           ...) {

    
    plotTitle <- main

    ## for MIRROR functionality
    arg.list <- formals(xpose.plot.splom)
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
      create.mirror(xpose.plot.splom,
                         new.arg.list,mirror,plotTitle,...)
    } else { # end if mirror
      
      ##Get data
                                        #data <- object@Data[, xvardef("parms", object), drop = F]
                                        #mlist <- c(plist, by, groups)
                                        #data <- Data(object,inclZeroWRES,onlyfirst=onlyfirst,subset=subset)[, mlist, drop = F]
      if(!is.null(samp)) {
        data <- SData(object,inclZeroWRES=inclZeroWRES,onlyfirst=onlyfirst,
                      subset=subset,samp=samp)
      } else {
        data <- Data(object,inclZeroWRES=inclZeroWRES,onlyfirst=onlyfirst,subset=subset)
      }

      ## Strip "missing" data
      for (i in plist) {
        data <- subset(data, get(i) != object@Prefs@Miss)
      }

      if(any(is.null(data))) return("The data or subset expression is invalid.")
      
      ## if the parameter or variable in the list has only one value don't plot it
      remove.from.plist=c()

      for (i in 1:length(plist)) {
        if(!is.factor(data[,plist[i]])){
          if(length(unique(data[,plist[i]])) < 2){
            remove.from.plist=c(remove.from.plist,i)
            cat(paste(plist[i],
                      "has only one value and will not be\n",
                      "shown in the scatterplot\n"))
          } 
        } else {
          if(!include.cat.vars){
            remove.from.plist=c(remove.from.plist,i)
            cat(paste(plist[i],
                      "is categorical and will not be\n",
                      "shown in the scatterplot\n"))
          } else {
            if(length(levels(data[,plist[i]])) < 2){
              remove.from.plist=c(remove.from.plist,i)
              cat(paste(plist[i],
                        " has only one value and will not be\n",
                        "shown in the scatterplot\n",sep=""))
            }
          }
        }
      }
      if(length(remove.from.plist)>0){
        plist <- plist[-remove.from.plist]
        if(!is.null(varnames))  varnames <- varnames[-remove.from.plist]
      }
      
      if(is.null(varnames)) {
        varnames <- c()
        for (i in plist) {
          varnames <- c(varnames, xlabel(i, object))
        }
      }
      
      ## Make sure by is a factor if requested
      if(!is.null(by) && force.by.factor) {
        for(b in by) {
          data[,b] <- as.factor(data[,b])
        }
      }
      
      ## Collect the basic plot formula
      bb <- NULL

      if(any(is.null(by))) {
        formel <- paste("~data[, plist]", sep="") 
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
        formel <-  paste("~data[, plist] | ", bys, sep="")
      }

      if(missing(strip)) {
        strip <- function(var.name,...)
          strip.default(var.name=bb,strip.names=c(F,T),...)
      }

      ## Check to see if panel.superpose should be used
      if(any(!is.null(groups))) groups <- data[,groups]

      ## CHeck to see if a superpose smooth is to be used.
      suline <- NULL
      if(!is.null(suline)) {
        suline <- data[,suline]
      }
      
      ## Check for id-numbers as plotting symbols
      if(!is.null(ids)) ids <- data[,xvardef("idlab",object)]

                                        #cat(formel)
                                        #readline()
                                        #browser()

      ##     if(length(plist)>7) {
      ##       varname.cex=0.6
      ##       axis.text.cex=0.6
      ##     }

      
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

      xplot <- splom(formula(formel), data, obj=object,
                                        #prepanel.limits = function(x) 
                                        #  if (is.factor(x)) levels(x) else
                                        #    extend.limits(range(as.numeric(x), na.rm = TRUE)),
                     varnames=varnames,
                     onlyfirst = onlyfirst,
                     panel=panel,
                     strip = strip,
                                        #par.strip.text = par.strip.text,
                     groups=groups,
                     inclZeroWRES=inclZeroWRES,
                     ids   = ids,
                     main=main,
                                        #aspect=aspect,
                     smooth=smooth,
                     lmline = lmline,
                     ylab = ylb,
                     xlab = xlb,
                     scales = scales,
                                        #varname.cex=varname.cex,
                                        #axis.text.cex=axis.text.cex,
                     ...)
      return(xplot)
      
    }

  }
