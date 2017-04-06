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



#' Categorical observations vs. idependent variable using stacked bars.
#' 
#' Categorical observations vs. idependent variable using stacked bars.
#' 
#' 
#' @param object Xpose data object.
#' @param dv The dependent variable (e.g. \code{"DV"} or \code{"CP"}.)
#' @param idv The indenpent variable (e.g. \code{"TIME"}.)
#' @param by Conditioning variable
#' @param groups How we should group values in each conditional plot.
#' @param force.by.factor Should we force the data to be treated as factors?
#' @param recur Not used.
#' @param xlb A string giving the label for the x-axis. \code{NULL} if none.
#' @param ylb A string giving the label for the y-axis. \code{NULL} if none.
#' @param subset Subset of data.
#' @param vary.width Should we vary the width of the bars to match amount of
#' information?
#' @param level.to.plot Which levels of the DV to plot.
#' @param refactor.levels Should we refactor the levels?
#' @param main The title of the plot.
#' @param stack Should we stack the bars?
#' @param horizontal Should the bars be horizontal?
#' @param strip Defining how the strips should appear in the conditioning
#' plots.
#' @param scales Scales argument to \code{\link[lattice]{xyplot}}.
#' @param inclZeroWRES Include rows with WRES=0?
#' @param onlyfirst Only include first data point for each individual?
#' @param samp Sample to use in mirror plot (a number).
#' @param aspect Aspect argument to \code{\link[lattice]{xyplot}}.
#' @param auto.key Make a legend.
#' @param mirror Mirror can be \code{FALSE}, \code{TRUE}, 1 or 3.
#' @param mirror.aspect Aspect for mirror.
#' @param pass.plot.list Should the plot list be passsed back to user?
#' @param x.cex Size of x axis label.
#' @param y.cex Size of Y axis label.
#' @param main.cex Size of Title.
#' @param mirror.internal Internal stuff.
#' @param \dots Other arguments passed to function.
#' @author Andrew Hooker
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## read in table files
#' runno <- 45
#' xpdb <- xpose.data(runno)
#' 
#' ## make some stacked bar plots
#' cat.dv.vs.idv.sb(xpdb,idv=NULL,stack=F)
#' cat.dv.vs.idv.sb(xpdb,idv=NULL,stack=F,by="DOSE")
#' cat.dv.vs.idv.sb(xpdb,idv="DOSE")
#' cat.dv.vs.idv.sb(xpdb,idv=NULL,stack=F,by="TIME")
#' cat.dv.vs.idv.sb(xpdb,idv="TIME")
#' cat.dv.vs.idv.sb(xpdb,idv="CAVH")
#' cat.dv.vs.idv.sb(xpdb,idv="TIME",by="DOSE",scales=list(x=list(rot=45)))
#' 
#' ## make some mirror plots
#' cat.dv.vs.idv.sb(xpdb,idv="DOSE",mirror=1)
#' cat.dv.vs.idv.sb(xpdb,idv="CAVH",mirror=1,auto.key=F)
#' }
#' 
#' @export cat.dv.vs.idv.sb
#' @family specific functions 
"cat.dv.vs.idv.sb"  <-
  function(object,
           dv=xvardef("dv",object),
           idv=xvardef("idv",object),
           by=NULL,
           groups=dv,
           force.by.factor = FALSE,
           recur=F,
           xlb=idv,
           ylb="Proportion",
           subset=NULL,
           vary.width=T,
           level.to.plot=NULL,
           refactor.levels=TRUE,
           main=xpose.create.title.text(idv,dv,
             "Proportions of",object,subset=subset,...),
           stack=TRUE,
           horizontal=FALSE,



           strip = function(...)
           strip.default(...,strip.names=c(TRUE,TRUE)),
           scales       = list(),
           inclZeroWRES = TRUE,
           onlyfirst    = FALSE,
           samp         = NULL,
           aspect       = object@Prefs@Graph.prefs$aspect,
           auto.key = "Default",#TRUE,
           
           ## mirror stuff
           mirror       = FALSE,
           
           mirror.aspect="fill",
           pass.plot.list=FALSE,
           x.cex=NULL,
           y.cex=NULL,
           main.cex=NULL,
           mirror.internal=list(strip.missing=missing(strip)),
           ...){
  if(is.null(check.vars(c(dv,idv),
                        object,silent=FALSE))) {
    return()
  }
  
  plotTitle <- main
  
  ## for MIRROR functionality
  arg.list <- formals(cat.dv.vs.idv.sb)
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
    create.mirror(cat.dv.vs.idv.sb,
                  new.arg.list,mirror,plotTitle,...)
  } else { # end if mirror
    
    ##Get data
    if(!is.null(samp)) {
      data <- SData(object,inclZeroWRES,onlyfirst=onlyfirst,
                    subset=subset,samp=samp)
    } else {
      data <- Data(object,inclZeroWRES,onlyfirst=onlyfirst,subset=subset)
    }
    
    ## Strip "missing" data
    data <- subset(data, get(dv) != object@Prefs@Miss)    
    if(any(is.null(data))) return("The subset expression is invalid!\n")

    ## Make sure by is a factor if requested
    if(!is.null(by) && force.by.factor) {
      for(b in by) {
        data[,b] <- as.factor(data[,b])
      }
    }

##     ## Set up the data
##     retlist <- make.sb.data(data,idv,dv,by=by,...)
##     ret <- retlist$ret
##     dvs   <- unique(data[,dv])
##     if(vary.width) {
##       wdths <- retlist$wdths
##     } else {
##       wdths <- rep(1,length(retlist$wdths))
##     }

    ## Set up the data
    retlist <- make.sb.data(data,idv,dv,by=by,...)
    ret <- retlist$ret
    if(!is.null(level.to.plot)){
      ret <- ret[ret["dv"]==level.to.plot,]
      if(refactor.levels){
        ret["dv"] <- factor(ret[["dv"]])
      }
    }
    dvs   <- unique(ret[,"dv"])
    if(is.null(idv)) ret$idv <- "All Values"
##     if(vary.width) {
##       wdths <- retlist$wdths
##     } else {
##       wdths <- rep(1,length(retlist$wdths))
##     }


    ## x and y labels
    nams <- names(ret)
    if(is.null(xlb)){
      xlb <- idv
    }
    if(is.null(ylb)) ylb <- paste("Proportions of ",dv,sep="")

    ## For sizes of labels
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

##     ##barplot(as.matrix(ret),width=wdths,xlab=xlb,ylab=ylb,legend.text=T)
##     ret.stack <- stack(ret)
##     ret.stack$level <- row.names(ret)
##     ret.stack$ind <- factor(ret.stack$ind,levels=names(ret))
##     if(is.null(idv)) ret.stack$ind <- "All Values"
    
##     if(!is.null(level.to.plot)){
##       ret.stack <- subset(ret.stack,level==paste(dv,"=",level.to.plot,sep=""))
##     }

    ##if(is.null(idv)) ret.stack$ind <- "All Values"


    
    
    if(auto.key=="Default"){
      auto.key <- list(#title=dv,
                       #text= paste(dv,"=",dvs, sep = ""),
                       cex=0.8)
    }
    
    
##     ## Collect the basic plot formula
##     bb <- NULL
##     if(any(is.null(by))) { ## No conditioning
##       formel <- paste("values~ind",sep="")
##     } else {
##       bys    <- paste(by,collapse="*")
##       formel <-  paste("values~ind","|",bys,sep="")
##     }

    
    if(!is.null(by)){
      by.loc <- grep("by.var",names(ret))
      names(ret)[by.loc] <- by
    }
    ## Collect the basic plot formula
    bb <- NULL
    if(any(is.null(by))) { ## No conditioning
      formel <- paste("proportion~","idv",sep="")
    } else {
      bys    <- paste(by,collapse="*")
      #formel <-  paste("proportion~","idv","|",bys,sep="")
      formel <-  paste("proportion~","idv","|",by,sep="")
    }


                                        #wdths <- c(12,2,24,90)
                                        #wdths <- wdths/100
                                        #wdths <- c(20,20,2,20)
    ## values~ind
##     xplot <- barchart(formula(formel),data=ret.stack,
##                       groups=level,stack=T,
##                                         #box.ratio=wdths,
##                       auto.key=auto.key,#list(columns=length(dvs)),
##                       xlab=xlb,ylab=ylb,main=main,
##                       scales=scales,
##                       aspect=aspect,
##                       ...)
    
    ret$levs <- ret[["dv"]]
    levels(ret$levs) <- paste(groups,"=",levels(ret$levs))

    xplot <- barchart(formula(formel),groups=ret$levs,
                      data=ret,
                      stack=stack,
                      horizontal=horizontal,#
                      #box.ratio=ret$wdth,
                      auto.key=auto.key,#list(columns=length(dvs)),
                      xlab=xlb,ylab=ylb,main=main,
                      scales=scales,
                      aspect=aspect,
                      strip=strip,
                      ...)
    
    return(xplot)
  }
}

