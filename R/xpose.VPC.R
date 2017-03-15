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



#' Visual Predictive Check (VPC) using XPOSE
#' 
#' This Function is used to create a VPC in xpose using the output from the
#' \code{vpc} command in Pearl Speaks NONMEM (PsN).  The function reads in the
#' output files created by PsN and creates a plot from the data.  The dependent
#' variable, independent variable and conditioning variable are automatically
#' determined from the PsN files.
#' 
#' 
#' @param vpc.info The results file from the \code{vpc} command in PsN. for
#' example \file{vpc\_results.csv}, or if the file is in a separate directory
#' \file{./vpc\_dir1/vpc\_results.csv}.
#' @param vpctab The \file{vpctab} from the \code{vpc} command in PsN.  For
#' example \file{vpctab5}, or if the file is in a separate directory
#' \file{./vpc\_dir1/vpctab5}.  Can be \code{NULL}.  The default looks in the
#' current working directory and takes the first file that starts with
#' \file{vpctab} that it finds.  Note that this default can result in the wrong
#' files being read if there are multiple \file{vpctab} files in the directory.
#' One of \code{object} or \code{vpctab} is required.  If both are present then
#' the information from the \code{vpctab} will over-ride the xpose data object
#' \code{object} (i.e. the values from the vpctab will replace any matching
#' values in the \code{object\@Data} portion of the xpose data object).
#' @param object An xpose data object. Created from \code{\link{xpose.data}}.
#' One of \code{object} or \code{vpctab} is required.  If both are present then
#' the information from the \code{vpctab} will over-ride the xpose data object
#' \code{object} (i.e. the values from the vpctab will replace any matching
#' values in the \code{object\@Data} portion of the xpose data object).
#' @param ids A logical value indicating whether text ID labels should be used
#' as plotting symbols (the variable used for these symbols indicated by the
#' \code{idlab} xpose data variable). Can be \code{FALSE} or \code{TRUE}.
#' @param type Character string describing the way the points in the plot will
#' be displayed. For more details, see \code{\link[graphics]{plot}}. Use
#' \code{type="n"} if you don't want observations in the plot.
#' @param by A string or a vector of strings with the name(s) of the
#' conditioning variables. For example \code{by = c("SEX","WT")}.  Because the
#' function automatically determines the conditioning variable from the PsN
#' input file specified in \code{vpc.info}, the \code{by} command can control
#' if separate plots are created for each condition (\code{by=NULL}), or if a
#' conditioning plot should be created (\code{by="WT"} for example).  If the
#' \code{vpc.info} file has a conditioning variable then \code{by} must match
#' that variable.  If there is no conditioning variable in \code{vpc.info} then
#' the \code{PI} for each conditioned plot will be the \code{PI} for the entire
#' data set (not only for the conditioning subset).
#' @param PI Either "lines", "area" or "both" specifying whether prediction
#' intervals (as lines, a shaded area or both) should be added to the plot.
#' \code{NULL} means no prediction interval.
#' @param PI.ci Plot the prediction interval of the simulated data's
#' percentiles for each bin. Values can be \code{"both", "area" or "lines"}
#' This can be thought of as a prediction interval about the \code{PI.real} or
#' a confidence interval about the \code{PI}.  However, note that with
#' increasing number of simulations the CI will not go towards zero because the
#' interval is also dependent on the size of the data set.
#' @param PI.real Plot the percentiles of the real data in the various bins.
#' values can be NULL or TRUE.  Note that for a bin with few actual
#' observations the percentiles will be approximate.  For example, the 95th
#' percentile of 4 data points will always be the largest of the 4 data points.
#' @param PI.ci.med.arcol The color of the median \code{PI.ci}.
#' @param force.x.continuous Logical value indicating whether x-values should
#' be taken as continuous, even if categorical.
#' @param funy String of function to apply to Y data. For example "abs"
#' @param logy Logical value indicating whether the y-axis should be
#' logarithmic, base 10.
#' @param ylb Label for the y-axis
#' @param subset A string giving the subset expression to be applied to the
#' data before plotting. See \code{\link{xsubset}}.
#' @param main A string giving the plot title or \code{NULL} if none.
#' \code{"Default"} creates a default title.
#' @param main.sub Used for names above each plot when using multiple plots.
#' Should be a vector \code{c("Group 1","Group 2")}
#' @param main.sub.cex The size of the \code{main.sub} titles.
#' @param inclZeroWRES Logical value indicating whether rows with WRES=0 is
#' included in the plot.
#' @param verbose Text messages passed to screen or not.
#' @param \dots Other arguments passed to \code{\link{xpose.panel.default}},
#' \code{\link{xpose.plot.default}} and others. Please see these functions for
#' more descriptions of what you can do.
#' @return A plot or a list of plots.
#' @section Additional arguments:
#' 
#' \strong{Additional graphical elements available in the VPC plots\cr}
#' 
#' \describe{ \item{list("PI.real = NULL or TRUE")}{Plot the percentiles of the
#' real data in the various bins.  Note that for a bin with few actual
#' observations the percentiles will be approximate.  For example, the 95th
#' percentile of 4 data points will always be the largest of the 4 data
#' points.} \item{list("PI.mirror = NULL, TRUE or AN.INTEGER.VALUE")}{Plot the
#' percentiles of one simulated data set in each bin. \code{TRUE} takes the
#' first mirror from \file{vpc\_results.csv} and \code{AN.INTEGER.VALUE} can be
#' \code{1, 2, \dots{} n} where \code{n} is the number of mirror's output in
#' the \file{vpc\_results.csv} file.} \item{list("PI.ci = \"both\", \"area\" or
#' \"lines\"")}{Plot the confidence interval for the simulated data's
#' percentiles for each bin (for each simulated data set compute the
#' percentiles for each bin, then, from all of the percentiles from all of the
#' simulated datasets compute the 95\% CI of these percentiles).  These CIs can
#' be used to asses the \code{PI.real} values for model misspecification.
#' Again, as with the \code{PI.real}, note that with few observations per bin
#' the CIs will be approximate because the percentiles in each bin will be
#' approximate. For example, the 95th percentile of 4 data points will always
#' be the largest of the 4 data points.} \item{list("PI.limits = c(0.025,
#' 0.975)")}{A vector of two values that describe the limits of the prediction
#' interval that should be displayed.  These limits should be found in the
#' \file{vpc\_results.csv} file. These limits are also used as the percentages
#' for the \code{PI.real, PI.mirror} and \code{PI.ci}.  However, the confidence
#' interval in \code{PI.ci} is always the one defined in the
#' \file{vpc\_results.csv} file.} }
#' 
#' \strong{Additional options to control the look and feel of the \code{PI}.
#' See See \code{\link[grid]{grid.polygon}} and \code{\link[graphics]{plot}}
#' for more details.\cr}
#' 
#' \describe{ \item{list("PI.arcol")}{The color of the \code{PI} area}
#' \item{list("PI.up.lty")}{The upper line type. can be "dotted" or "dashed",
#' etc.} \item{list("PI.up.type")}{The upper type used for plotting.  Defaults
#' to a line.} \item{list("PI.up.col")}{The upper line color}
#' \item{list("PI.up.lwd")}{The upper line width}
#' \item{list("PI.down.lty")}{The lower line type. can be "dotted" or "dashed",
#' etc.} \item{list("PI.down.type")}{The lower type used for plotting.
#' Defaults to a line.} \item{list("PI.down.col")}{The lower line color}
#' \item{list("PI.down.lwd")}{The lower line width}
#' \item{list("PI.med.lty")}{The median line type. can be "dotted" or "dashed",
#' etc.} \item{list("PI.med.type")}{The median type used for plotting.
#' Defaults to a line.} \item{list("PI.med.col")}{The median line color}
#' \item{list("PI.med.lwd")}{The median line width} }
#' 
#' \strong{Additional options to control the look and feel of the \code{PI.ci}.
#' See See \code{\link[grid]{grid.polygon}} and \code{\link[graphics]{plot}}
#' for more details.\cr}
#' 
#' \describe{ \item{list("PI.ci.up.arcol")}{The color of the upper
#' \code{PI.ci}.} \item{list("PI.ci.med.arcol")}{The color of the median
#' \code{PI.ci}.} \item{list("PI.ci.down.arcol")}{The color of the lower
#' \code{PI.ci}.} \item{list("PI.ci.up.lty")}{The upper line type. can be
#' "dotted" or "dashed", etc.} \item{list("PI.ci.up.type")}{The upper type used
#' for plotting.  Defaults to a line.} \item{list("PI.ci.up.col")}{The upper
#' line color} \item{list("PI.ci.up.lwd")}{The upper line width}
#' \item{list("PI.ci.down.lty")}{The lower line type. can be "dotted" or
#' "dashed", etc.} \item{list("PI.ci.down.type")}{The lower type used for
#' plotting.  Defaults to a line.} \item{list("PI.ci.down.col")}{The lower line
#' color} \item{list("PI.ci.down.lwd")}{The lower line width}
#' \item{list("PI.ci.med.lty")}{The median line type. can be "dotted" or
#' "dashed", etc.} \item{list("PI.ci.med.type")}{The median type used for
#' plotting.  Defaults to a line.} \item{list("PI.ci.med.col")}{The median line
#' color} \item{list("PI.ci.med.lwd")}{The median line width}
#' \item{PI.ci.area.smooth}{Should the "area" for \code{PI.ci} be smoothed to
#' match the "lines" argument? Allowed values are \code{TRUE/FALSE}. The "area"
#' is set by default to show the bins used in the \code{PI.ci} computation.  By
#' smoothing, information is lost and, in general, the confidence intervals
#' will be smaller than they are in reality.} }
#' 
#' \strong{Additional options to control the look and feel of the
#' \code{PI.real}. See See \code{\link[grid]{grid.polygon}} and
#' \code{\link[graphics]{plot}} for more details.\cr}
#' 
#' \describe{ \item{list("PI.real.up.lty")}{The upper line type. can be
#' "dotted" or "dashed", etc.} \item{list("PI.real.up.type")}{The upper type
#' used for plotting.  Defaults to a line.} \item{list("PI.real.up.col")}{The
#' upper line color} \item{list("PI.real.up.lwd")}{The upper line width}
#' \item{list("PI.real.down.lty")}{The lower line type. can be "dotted" or
#' "dashed", etc.} \item{list("PI.real.down.type")}{The lower type used for
#' plotting.  Defaults to a line.} \item{list("PI.real.down.col")}{The lower
#' line color} \item{list("PI.real.down.lwd")}{The lower line width}
#' \item{list("PI.real.med.lty")}{The median line type. can be "dotted" or
#' "dashed", etc.} \item{list("PI.real.med.type")}{The median type used for
#' plotting.  Defaults to a line.} \item{list("PI.real.med.col")}{The median
#' line color} \item{list("PI.real.med.lwd")}{The median line width} }
#' 
#' \strong{Additional options to control the look and feel of the
#' \code{PI.mirror}. See See \code{\link[graphics]{plot}} for more details.\cr}
#' 
#' \describe{ \item{list("PI.mirror.up.lty")}{The upper line type. can be
#' "dotted" or "dashed", etc.} \item{list("PI.mirror.up.type")}{The upper type
#' used for plotting.  Defaults to a line.} \item{list("PI.mirror.up.col")}{The
#' upper line color} \item{list("PI.mirror.up.lwd")}{The upper line width}
#' \item{list("PI.mirror.down.lty")}{The lower line type. can be "dotted" or
#' "dashed", etc.} \item{list("PI.mirror.down.type")}{The lower type used for
#' plotting.  Defaults to a line.} \item{list("PI.mirror.down.col")}{The lower
#' line color} \item{list("PI.mirror.down.lwd")}{The lower line width}
#' \item{list("PI.mirror.med.lty")}{The median line type. can be "dotted" or
#' "dashed", etc.} \item{list("PI.mirror.med.type")}{The median type used for
#' plotting.  Defaults to a line.} \item{list("PI.mirror.med.col")}{The median
#' line color} \item{list("PI.mirror.med.lwd")}{The median line width} }
#' @author Andrew Hooker
#' @seealso \code{\link{read.vpctab}} \code{\link{read.npc.vpc.results}}
#' \code{\link{xpose.panel.default}} \code{\link{xpose.plot.default}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' library(xpose4)
#' 
#' xpose.VPC()
#' 
#' ## to be more clear about which files should be read in
#' vpc.file <- "vpc_results.csv"
#' vpctab <- "vpctab5"
#' xpose.VPC(vpc.info=vpc.file,vpctab=vpctab)
#' 
#' ## with lines and a shaded area for the prediction intervals
#' xpose.VPC(vpc.file,vpctab=vpctab,PI="both")
#' 
#' ## with the percentages of the real data
#' xpose.VPC(vpc.file,vpctab=vpctab,PI.real=T)
#' 
#' ## with mirrors (if supplied in 'vpc.file')
#' xpose.VPC(vpc.file,vpctab=vpctab,PI.real=T,PI.mirror=5)
#' 
#' ## with CIs
#' xpose.VPC(vpc.file,vpctab=vpctab,PI.real=T,PI.ci="area")
#' xpose.VPC(vpc.file,vpctab=vpctab,PI.real=T,PI.ci="area",PI=NULL)
#' 
#' ## stratification (if 'vpc.file' is stratified)
#' cond.var <- "WT"
#' xpose.VPC(vpc.file,vpctab=vpctab)
#' xpose.VPC(vpc.file,vpctab=vpctab,by=cond.var)
#' xpose.VPC(vpctab=vpctab,vpc.info=vpc.file,PI="both",by=cond.var,type="n")
#' 
#' ## with no data points in the plot
#' xpose.VPC(vpc.file,vpctab=vpctab,by=cond.var,PI.real=T,PI.ci="area",PI=NULL,type="n")
#' 
#' ## with different DV and IDV, just read in new files and plot
#' vpc.file <- "vpc_results.csv"
#' vpctab <- "vpctab5"
#' cond.var <- "WT"
#' xpose.VPC(vpctab=vpctab,vpc.info=vpc.file,PI="both",by=cond.var)
#' xpose.VPC(vpctab=vpctab,vpc.info=vpc.file,PI="both")
#' 
#' ## to use an xpose data object instead of vpctab
#' ##
#' ## In this example
#' ## we expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' runnumber <- 5
#' xpdb <- xpose.data(runnumber)
#' xpose.VPC(vpc.file,object=xpdb)
#' 
#' ## to read files in a directory different than the current working directory 
#' vpc.file <- "./vpc_strat_WT_4_mirror_5/vpc_results.csv"
#' vpctab <- "./vpc_strat_WT_4_mirror_5/vpctab5"
#' xpose.VPC(vpc.info=vpc.file,vpctab=vpctab)
#' 
#' ## to rearrange order of factors in VPC plot
#' xpdb@Data$SEX <- factor(xpdb@Data$SEX,levels=c("2","1"))
#' xpose.VPC(by="SEX",object=xpdb)
#' 
#' }
#' 
#' 
#' @export xpose.VPC
"xpose.VPC" <-
  function(vpc.info="vpc_results.csv",  #name of PSN file to use
           vpctab = dir(pattern="^vpctab")[1],
           object = NULL,
           ids=FALSE,
           type="p",
           by=NULL,
           PI=NULL,#"area",
           PI.ci="area",
           PI.real=T,
           PI.ci.med.arcol="red",
           subset=NULL,
           main="Default",
           main.sub=NULL,  # used for names above each plot when using multiple plots
                                        #Should be a vector c("","")
           main.sub.cex=0.85, # size of main.sub 
           inclZeroWRES=FALSE,
           force.x.continuous=FALSE,
           #strip="Default",
           #dont.plot=F,
           funy=NULL,
           logy=FALSE,
           ylb = "Default",
           verbose=FALSE,
           ...) {

    ## for testing
    ##vpctab="./vpc_strat_WT_4_mirror_5/vpctab5"
    ##vpctab="./vpc_strat_SEX_mirror_5/vpctab5"
    ##object <- xpdb
    ##inclZeroWRES <- FALSE
    
    ## Make sure we have the necessary variables defined
    if(is.null(object) & is.null(vpctab)){
      cat(paste("Both the arguments object and vpctab are NULL\n"))
      cat(paste("At least one of these must be defined\n"))
      return(NULL)
    }
    
    if(!is.null(vpctab)){
      tmp <- FALSE
      if(is.null(object)) tmp <- TRUE
      object <- read.vpctab(vpctab=vpctab,
                            object=object,
                            inclZeroWRES=inclZeroWRES,
                            verbose=verbose,
                            ...)
      if(tmp==TRUE) inclZeroWRES=TRUE
    }

    file.info <- read.npc.vpc.results(vpc.results=vpc.info,verbose=verbose,...)
    num.tables <- file.info$num.tables
    dv.var <- file.info$dv.var
    idv.var <- file.info$idv.var
    ##bin.table <- file.info$result.tables

    
    tmp <- c()
    if(is.null(object@Data[[dv.var]])) tmp <- c(tmp,dv.var)
    if(is.null(object@Data[[idv.var]])) tmp <- c(tmp,idv.var)
    if (!is.null(tmp)){
      cat("\n-----------Variable(s) not defined!-------------\n",
          tmp, "is/are not defined in the current database\n",
          "and must be defined for this command to work!\n",
          "------------------------------------------------\n")
      return(NULL)
    }
  
    if(is.factor(object@Data[[dv.var]])){
      change.cat.cont(object) <- c(dv.var)
    }
    
    if(force.x.continuous){
      if(is.factor(object@Data[[idv.var]])){
        change.cat.cont(object) <- c(idv.var)
      }
    }

    ## decide on the conditioning
    if (is.null(by) && num.tables!=1){
      ## get conditioning veriable name
      # for future use to automatically start conditioning
      #for (i in 1:num.tables){
      #  tmp.strata <- strata.names[i]
      #  strata.loc <- regexpr(strata.start.pat,strata.line)+7
      #  strata.names <- c(strata.names,substring(strata.line,strata.loc))
      #}

      ## use subsetting to get things working
      if(!is.null(subset)){ # this can be fixed below
        if(verbose) cat(paste("Overwriting the subset expression to handle multiple STRATA\n"))
      }

      plotList <- vector("list",num.tables)
      plot.num <- 0 # initialize plot number
      

      ## this can be updated as in npc.coverage.R
      for (i in 1:num.tables){ 
        ##subset <- file.info$result.tables[[num.tables+1]][i] # this can be fixed to aviod overwriting subsets
        subset <- file.info$strata.names[i] # this can be fixed to aviod overwriting subsets 
        final.bin.table <- file.info$result.tables[[i]]
        if(!is.null(main.sub)){
          sub.main=main.sub[i]
        } else {
          sub.main=subset
        }

        if(!is.character(ylb)){
        } else if(ylb != "Default"){
        } else {
          tmp.label <- xpose.create.label(dv.var,
                                          object,
                                          funy,
                                          logy,...)
        
          if(file.info$pred.corr && !file.info$var.corr){
            tmp.label <- paste(tmp.label,"\n(Pred Corr)")
          }
          if(file.info$pred.corr && file.info$var.corr){
            tmp.label <- paste(tmp.label,"\n(Pred and Var Corr)")
          }
          ylb=tmp.label
        }

        ## make the VPC
        xplot <- xpose.plot.default(idv.var,#xvardef("idv",object),
                                    dv.var,#xvardef("dv",object),
                                    object,
                                    ids=ids,
                                    type=type,
                                    subset=subset,
                                    PI=PI,
                                    PI.ci=PI.ci,
                                    PI.real=PI.real,
                                    PI.ci.med.arcol=PI.ci.med.arcol,
                                    PI.bin.table=final.bin.table,
                                    pass.plot.list=TRUE,
                                    main=sub.main,
                                    main.cex=main.sub.cex,
                                    inclZeroWRES=inclZeroWRES,
                                    ylb = ylb,
                                    funy=funy,
                                    logy=logy,
                                    ...)
        plot.num <- plot.num+1
        plotList[[plot.num]] <- xplot
      }
        
      default.plot.title <- "Visual Predictive Check\n"
      if(file.info$pred.corr && !file.info$var.corr){
        default.plot.title <- "Visual Predictive Check\n (Prediction Corrected)\n"
      }
      if(file.info$pred.corr && file.info$var.corr){
        default.plot.title <- "Visual Predictive Check\n (Prediction and Variance Corrected)\n"
      }

      default.plot.title <- paste(default.plot.title,
                                  xpose.create.title(idv.var,dv.var,object,
                                                     no.runno=T,...),sep="")
      plotTitle <- xpose.multiple.plot.title(object=object,
                                             plot.text = default.plot.title,
                                             main=main,
                                             #subset=subset,
                                             ...)

#      if(!dont.plot){
#        xpose.multiple.plot.default(plotList,plotTitle=plotTitle,...)
#      }
      obj <- xpose.multiple.plot(plotList,plotTitle,...)
#      return(invisible(plotList))
      return(obj)
      
    } else { ## either plot stratification with by or only one strata 
      ## check structure of stratification variable
      if(!is.null(by) && num.tables!=1){
        if(all(is.null(file.info$by.interval))){
          ## categorical variable
          if(!is.factor(object@Data[[by]])) change.cat.cont(object) <- by
        } else {
          ## continuous variable
          if(is.factor(object@Data[[by]])) change.cat.cont(object) <- by
        }
      }

      default.plot.title <- "Visual Predictive Check\n"
      if(file.info$pred.corr && !file.info$var.corr){
        default.plot.title <- "Visual Predictive Check\n (Prediction Corrected)\n"
      }
      if(file.info$pred.corr && file.info$var.corr){
        default.plot.title <- "Visual Predictive Check\n (Prediction and Variance Corrected)\n"
      }
      default.plot.title <- paste(default.plot.title,
                                  xpose.create.title(idv.var,dv.var,object,
                                                     no.runno=T,subset=subset,...),sep="")
      plotTitle <- xpose.multiple.plot.title(object=object,
                                             plot.text = default.plot.title,
                                             main=main,
                                             subset=subset,
                                             ...)

      if(!is.character(ylb)){
      } else if(ylb != "Default"){
      } else {
        tmp.label <- xpose.create.label(dv.var,
                                        object,
                                        funy,
                                        logy,...)
        
        if(file.info$pred.corr && !file.info$var.corr){
          tmp.label <- paste(tmp.label,"\n(Pred Corr)")
        }
        if(file.info$pred.corr && file.info$var.corr){
          tmp.label <- paste(tmp.label,"\n(Pred and Var Corr)")
        }
        ylb=tmp.label
      }

      ## make the VPC
      xplot <- xpose.plot.default(idv.var,#xvardef("idv",object),
                                  dv.var,#xvardef("dv",object),
                                  object,
                                  ids=ids,
                                  type=type,
                                  by=by,
                                  subset=subset,
                                  PI=PI,
                                  PI.ci=PI.ci,
                                  PI.real=PI.real,
                                  PI.ci.med.arcol=PI.ci.med.arcol,
                                  PI.bin.table=file.info$result.tables,
                                  #force.by.factor=TRUE,
                                  main=plotTitle,
                                  by.interval=file.info$by.interval,
                                  inclZeroWRES=inclZeroWRES,
                                  ylb = ylb,#tmp.label,
                                  funy=funy,
                                  logy=logy,
                                  ...)
      return(xplot)
    }
  }
