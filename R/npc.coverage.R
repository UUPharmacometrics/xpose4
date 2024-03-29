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



#' Function to plot the coverage of the Numerical Predictive Check
#' 
#' This function takes the output from the \code{npc} command in Perl Speaks
#' NONMEM (PsN) and makes a coverage plot.  A coverage plot for the NPC looks
#' at different prediction intervals (PIs) for each data point and calculates
#' the total number of data points in the data set lying outside of these PIs.
#' The plot shows the relative amount of data points outside of their PI
#' compared to the expected amount at that PI.  In addition a confidence
#' interval around these values are computed based on the simulated data.
#' 
#' 
#' @param npc.info The results file from the \code{npc} command in PsN. For
#' example, \file{npc_results.csv}, or if the file is in a separate directory
#' \file{./npc_dir1/npc_results.csv}.
#' @param main A string giving the plot title or \code{NULL} if none.
#' \code{"Default"} creates a default title.
#' @param main.sub Used for names above each plot when using multiple plots.
#' Should be a vector \code{c("Group 1","Group 2")}
#' @param main.sub.cex The size of the \code{main.sub} titles.
#' @param \dots Other arguments passed to
#' \code{\link{xpose.multiple.plot.default}}, \code{\link[lattice]{xyplot}} and
#' others. Please see these functions (and below) for more descriptions of what you can do.
#' @return A list of plots
#' @section Additional arguments for the NPC coverage plots:
#' 
#' \strong{Additional plot features}
#' 
#' \describe{ 
#' \item{\code{CI}}{Specifies
#' whether confidence intervals (as lines, a shaded area or both) should be
#' added to the plot. Allowed values are: \code{"area"}, 
#' \code{"lines"}, \code{"both"}, or \code{NULL}.} 
#' \item{\code{mark.outside.data}}{Should the points outside the CI be marked in a different
#' color to identify them. Allowed values are \code{TRUE} or \code{FALSE}.} 
#' \item{\code{abline}}{Should there be a line
#' to mark the value of y=1? Possible values are \code{TRUE}, \code{FALSE} and
#' \code{NULL}.}
#' }
#' 
#' \strong{Line and area control. See \code{\link[graphics]{plot}},
#' \code{\link[grid]{grid.polygon}} and \code{\link[lattice]{xyplot}} for more
#' details.}
#' 
#' \describe{ \item{\code{CI.area.col}}{Color of the area
#' for the CI. Defaults to \code{"blue"}} 
#' \item{\code{CI.area.alpha}}{Transparency of the \code{CI.area.col}. Defaults to 0.3.}
#' \item{\code{ab.lwd}}{The width of the abline. Default is 1.}
#' \item{\code{ab.lty}}{Line type of the abline. Default is \code{"dashed"}}
#' \item{\code{CI.upper.lty}}{Line type of the line at the upper
#' edge of the CI.}
#' \item{\code{CI.upper.col}}{Color of the line at the upper edge of the CI.} 
#' \item{\code{CI.upper.lwd}}{The line width of
#' the line at the upper edge of the CI.} 
#' \item{\code{CI.lower.lty}}{The line type at
#' the lower edge of the CI.}
#' \item{\code{CI.lower.col}}{The color of the line at the
#' lower edge of the CI.}
#' \item{\code{CI.lower.lwd}}{The line width of the line at the lower
#' edge of the CI.}
#' \item{\code{obs.col}}{The color of the observed values.}
#' \item{\code{obs.pch}}{The type of point to use for the observed values.}
#' \item{\code{obs.lty}}{The type of line to use for the observed values.} 
#' \item{\code{obs.type}}{The combination of lines and points
#' to use for the observed values. Default is \code{"b"} for both.} 
#' \item{\code{obs.cex}}{The size of the
#' points to use for the observed values.} 
#' \item{\code{obs.lwd}}{The line
#' width to use for the observed values.} 
#' \item{\code{out.col}}{The color of the observed values that lie outside of the CI. Only used if
#' \code{mark.outside.data=TRUE}.} 
#' \item{\code{out.pch}}{The type of point
#' to use for the observed values that lie outside of the CI. Only used if
#' \code{mark.outside.data = TRUE}.} 
#' \item{\code{out.cex}}{The size of the
#' points of the observed values that lie outside of the CI. Only used if
#' \code{mark.outside.data = TRUE}.} 
#' \item{\code{out.lwd}}{The line width of
#' the observed values that lie outside of the CI. Only used if
#' \code{mark.outside.data = TRUE}.} }
#' @author Andrew Hooker
#' @seealso \code{\link{read.npc.vpc.results}}
#' \code{\link{xpose.multiple.plot.default}} \code{\link[lattice]{xyplot}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' library(xpose4)
#' 
#' npc.coverage()
#' 
#' ## to read files in a directory different than the current working directory 
#' npc.file <- "./another_directory/npc_results.csv"
#' npc.coverage(npc.info=npc.file)
#' }
#' 
#' @export npc.coverage
#' @family PsN functions 
"npc.coverage" <-
  function(npc.info="npc_results.csv",  #name of PSN file to use
           #object = NULL,
           #by=NULL,
           main="Default",
           main.sub=NULL,  # used for names above each plot when
                                        #using multiple plots
                                        #Should be a vector c("","")
           main.sub.cex=0.85, # size of main.sub 
           
           ...) {

    #npc.info <- npc.file
    #file.info <- read.npc.vpc.results(npc.results=npc.info)

    file.info <- read.npc.vpc.results(npc.results=npc.info,...)
    num.tables <- file.info$num.tables
    npc.tables <- file.info$result.tables

    myPanel <- function(x,y,
                        npc.table,
                        subscripts,
                        CI="area",
                        #CI.lines=NULL,
                        mark.outside.data=TRUE,
                        CI.area.col = "blue",
                        CI.area.alpha = 0.3,
                        ab.lty="dashed",
                        ab.lwd=1,
                        CI.upper.lty="dotted",
                        CI.upper.col="brown",
                        CI.upper.lwd="2",
                        CI.lower.lty="dotted",
                        CI.lower.col="brown",
                        CI.lower.lwd="2",
                        obs.col="black",
                        obs.pch=19,
                        obs.lty="solid",
                        obs.type="b",
                        obs.cex=1,
                        obs.lwd=1,
                        out.col="red",
                        out.pch=8,
                        out.cex=1.3,
                        out.lwd=1,
                        abline = TRUE,
                        ...){
      
      npc.table <- npc.table[subscripts,]
      x.poly <- c(npc.table$PI,rev(npc.table$PI))
      y.poly <- c(npc.table$upper.CI,rev(npc.table$lower.CI))
      
      if(!is.null(CI) & (CI=="area" | CI=="both")){
        grid.polygon(x.poly,y.poly,
                     default.units="native",
                     gp=gpar(fill=CI.area.col,#"burlywood1",#
                       alpha=CI.area.alpha,
                       col=NULL,lty=0),
                     ...)
      }
      
      if(abline==TRUE) panel.abline(lm(1~1),lty=ab.lty,lwd=ab.lwd,...)

      if(!is.null(CI) & (CI=="lines" | CI=="both")){
        ##if(!is.null(CI.lines)){
        panel.lines(npc.table$PI,npc.table$upper.CI,
                    type="l",
                    lty=CI.upper.lty,#"dotted",
                    col=CI.upper.col,#"brown",
                    lwd=CI.upper.lwd,#"2",
                    ...)
        panel.lines(npc.table$PI,npc.table$lower.CI,
                    type="l",
                    lty=CI.lower.lty,#"dotted",
                    col=CI.lower.col,#"brown",
                    lwd=CI.lower.lwd,#"2",
                    ...)
      }
      
      panel.xyplot(x,y,
                   col=obs.col,#"black",
                   pch=obs.pch,#19,
                   lty = obs.lty,#"solid",
                   type = obs.type,#"b",
                   cex = obs.cex,#1,
                   lwd = obs.lwd,#1,
                   ...)

      if (mark.outside.data){
        outside.data <- npc.table[npc.table$outside.PI == "*",]
        #outside.data <- subset(npc.table,outside.PI == "*")
        if(dim(outside.data)[1]>0){
          panel.xyplot(outside.data$PI,outside.data$observed,
                       col=out.col,#"red",
                       pch=obs.pch,
                       type = "p",
                       cex = obs.cex,
                       lwd = obs.lwd,
                       ...)
          panel.xyplot(outside.data$PI,outside.data$observed,
                       col=out.col,
                       pch=out.pch,#8,
                       type = "p",
                       cex = out.cex,#1.3,
                       lwd = out.lwd,#1,
                       ...)
        }
      }

    }

    myPrePanel <- function(x,y,
                           npc.table,
                           subscripts,...) {

      npc.table <- npc.table[subscripts,]
      x.poly <- c(npc.table$PI,rev(npc.table$PI))
      y.poly <- c(npc.table$upper.CI,rev(npc.table$lower.CI))

      ylim <- c(min(y.poly,y),
                max(y.poly,y))
      xlim <- c(min(x)-1,max(x)+1)
      list(xlim=xlim,ylim=ylim)
    }

    plotList <- vector("list",num.tables)
    plot.num <- 0 # initialize plot number      
    for (i in 1:num.tables){ 
          
      if(num.tables==1) final.table <- npc.tables
      if(num.tables!=1) final.table <- npc.tables[[i]]

      
      final.table$expected.pct <- (100 - final.table$PI)/2
      final.table$lower.y <- final.table$points.below.PI/final.table$expected.pct
      final.table$upper.y <- final.table$points.above.PI/final.table$expected.pct

      final.table$y.poly.lowPI.upCI <- final.table[["95.CI.below.to"]]/final.table$expected.pct
      final.table$y.poly.lowPI.lowCI <- final.table[["95.CI.below.from"]]/final.table$expected.pct
      final.table$y.poly.upPI.upCI <- final.table[["95.CI.above.to"]]/final.table$expected.pct
      final.table$y.poly.upPI.lowCI <- final.table[["95.CI.above.from"]]/final.table$expected.pct

      npc.table <- reshape(final.table, direction= "long",
                           varying=list(
                             c("lower.y","upper.y"),
                             c("y.poly.lowPI.lowCI","y.poly.upPI.lowCI"),
                             c("y.poly.lowPI.upCI","y.poly.upPI.upCI"),
                             c("outside.CI.for.below.PI","outside.CI.for.above.PI")
                             ),
                           v.names=c("observed","lower.CI","upper.CI","outside.PI"),
                           idvar="PI",times=c("Lower PI Limit","Upper PI Limit"))
      
      
      sub.main <- NULL
      if(num.tables>1) sub.main <-  file.info$result.tables[[num.tables+1]][i]
      if(!is.null(main.sub)) sub.main <- main.sub[i]
      
      xplot <- xyplot(observed ~ PI | time,
                      data=npc.table,
                      npc.table=npc.table,
                      prepanel=myPrePanel,
                      panel=myPanel,
                      layout=c(1,2),
                      ylab="Observed/Expected",
                      xlab="Prediction Interval",
                      main=list(sub.main,cex=main.sub.cex),
                      ##auto.key=list(title="foobar"),
                      ##scales=list(y="free"),
                      ...)
      plot.num <- plot.num+1
      plotList[[plot.num]] <- xplot
    }


    default.plot.title.1 <- "Coverage for Numerical Predictive Check\n"
    default.plot.title.2 <- paste("For",file.info$dv.var,"in",file.info$model.file,sep=" ")
    default.plot.title <- paste(default.plot.title.1,default.plot.title.2,sep="")
    
    if (is.null(main)){
      plotTitle <- NULL
    } else {
      if(!is.na(match(main,"Default"))) {
        plotTitle <- default.plot.title
      } else {
        plotTitle <- main
      }
    }

    
    obj <- xpose.multiple.plot(plotList,plotTitle,...)
    return(obj)

  }
  
