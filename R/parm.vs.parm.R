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

## Added by Andrew Hooker
## 21/12/2005



#' Plot parameters vs other parameters
#' 
#' This function plots the parameter values stored in an Xpose data object
#' versus each other in a series of graphs.  The mirror functionality is
#' available for this function.
#' 
#' Each of the parameters in the Xpose data object, as specified in
#' \code{object@Prefs@Xvardef$parms}, is plotted against the rest, creating a
#' stack of plots.
#' 
#' A wide array of extra options controlling \code{xyplots} are available. See
#' \code{\link{xpose.plot.default}} and \code{\link{xpose.panel.default}} for
#' details.
#' 
#' @param object An xpose.data object.
#' @param onlyfirst Logical value indicating whether only the first row per
#' individual is included in the plot.
#' @param abline Allows for a line of identity.
#' @param smooth Logical value indicating whether an x-y smooth should be
#' superimposed.  The default is TRUE.
#' @param type The plot type - defaults to points only.
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  
#' @param \dots Other arguments passed to \code{xpose.plot.default}.
#' @return Returns a stack of xyplots and histograms of parameters against
#' parameters.
#' @author Andrew Hooker
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb <- xpose.data(5)
#' 
#' 
#' parm.vs.parm(xpdb)
#' 
#' 
#' parm.vs.parm(xpdb,mirror=3)
#' }
#' 
#' @export parm.vs.parm
"parm.vs.parm" <-
  function(object,
           #xlb  = NULL,
           #ylb  = NULL,
           onlyfirst=TRUE,
           #inclZeroWRES=FALSE,
           #subset=xsubset(object),
           #mirror=FALSE,
           #seed  = NULL,
           #bins  = NULL,
           #samp  = NULL,
           abline= FALSE,
           smooth=TRUE,
           #prompt = FALSE,
           type="p",
           
           main="Default",
           ...) {
    
    ## is everything in place?
    for (i in xvardef("parms", object)) {      
      if(is.null(i)) {
        cat("Parameters are not properly set in the database!\n")
        return()
      }
    }


    ## create enpty list for plots
    number.of.plots <- 0
    for (i in xvardef("parms", object)) {
      for (j in xvardef("parms", object)) {
         if (j!=i) {
           if(!is.factor(object@Data[[i]])){
             number.of.plots <- number.of.plots + 1
           }
         }
       }
    }
    plotList <- vector("list",number.of.plots)
    plot.num <- 0 # initialize plot number
    
    ## loop over params
    for (i in xvardef("parms", object)) {  
    
      for (j in xvardef("parms", object)) {
      
        if (j!=i) {
          
          if(!is.factor(object@Data[[i]])){
                
            xplot <- xpose.plot.default(j,
                                        i,
                                        object,
                                        main=NULL,
                                        #xlb = xlb,
                                        #ylb = ylb,
                                        abline=abline,
                                        #abllwd=abllwd,
                                        onlyfirst = onlyfirst,
                                        #inclZeroWRES = inclZeroWRES,
                                        #subset = subset,
                                        smooth=smooth,
                                        type=type,
                                        pass.plot.list=TRUE,
                                        ...)
            
            plot.num <- plot.num+1
            plotList[[plot.num]] <- xplot
          }
        }
      } 
    } 

    default.plot.title <- "Parameters vs. parameters "
    plotTitle <- xpose.multiple.plot.title(object=object,
                                           plot.text = default.plot.title,
                                           main=main,
                                           ...)
    obj <- xpose.multiple.plot(plotList,plotTitle,...)
    return(obj)

  }



