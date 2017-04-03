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

## Added by Justin Wilkins 6 Dec 2005

## BUG: mirror=3 repeats the same simulated plot 3 times
##      with warning "longer object length is not a multiple of shorter object length in: data$iter == samp"
##      Traced to SData.R, line 9




#' Autocorrelation of weighted residuals for Xpose 4
#' 
#' This is an autocorrelation plot of weighted residuals.  Most of the options
#' take their default values from the xpose.data object but may be overridden
#' by supplying them as arguments.
#' 
#' A wide array of extra options controlling xyplots are available. See
#' \code{\link{xpose.plot.default}} for details.
#' 
#' @aliases autocorr.wres autocorr.iwres
#' @param object An xpose.data object.
#' @param smooth Logical value indicating whether a smooth should be
#' superimposed.
#' @param type 1-character string giving the type of plot desired. The
#' following values are possible, for details, see \code{\link{plot}}: '"p"'
#' for points, '"l"' for lines, '"o"' for overplotted points and lines, '"b"',
#' '"c"') for (empty if '"c"') points joined by lines, '"s"' and '"S"' for
#' stair steps and '"h"' for histogram-like vertical lines.  Finally, '"n"'
#' does not produce any points or lines.
#' @param ids A logical value indicating whether text labels should be used as
#' plotting symbols (the variable used for these symbols indicated by the
#' \code{idlab} xpose data variable).
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  For \code{"Default"} the function
#' \code{\link{xpose.multiple.plot.title}} is used.
#' @param \dots Other arguments passed to \code{link{xpose.plot.default}}.
#' @return Returns an aotocorrelation plot for weighted population residuals
#' (WRES) or individual weighted residuals (IWRES).
#' @author E. Niclas Jonsson, Mats Karlsson, Justin Wilkins & Andrew Hooker
#' @seealso \code{\link[lattice]{xyplot}}, \code{\link{xpose.prefs-class}},
#' \code{\link{xpose.data-class}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' }
#' 
#' ## Here we load the example xpose database 
#' data(simpraz.xpdb)
#' xpdb <- simpraz.xpdb
#' 
#' ## A vanilla plot
#' autocorr.wres(xpdb)
#' 
#' ## A conditioning plot
#' autocorr.wres(xpdb, dilution=TRUE)
#' 
#' ## Custom heading and axis labels
#' autocorr.wres(xpdb, main="My conditioning plot", ylb="|CWRES|", xlb="PRED")
#' 
#' ## Custom colours and symbols, IDs
#' autocorr.wres(xpdb, cex=0.6, pch=3, col=1, ids=TRUE)
#' 
#' ## A vanilla plot with IWRES
#' autocorr.iwres(xpdb)
#' 
#' @export autocorr.wres
"autocorr.wres" <-
  function(object,
           #ylb  = "|WRES|",
           #idsdir="up",
           type="p",
           smooth=TRUE,
           ids=F,
           main = "Default",
           ...) {

    if(is.null(check.vars(c("wres"),
                          object,silent=FALSE))) {
      return()
    }
    

    default.plot.title <- paste("Autocorrelation of ",xlabel(xvardef("wres",object),object),
                                sep="")
    plotTitle <- xpose.multiple.plot.title(object=object,
                                           plot.text = default.plot.title,
                                           main=main,
                                           ...)

    xplot <- xpose.plot.default(xvardef("wres",object),
                                xvardef("wres",object),
                                object,
                                #ylb=ylb,
                                #idsdir=idsdir,
                                type=type,
                                smooth=smooth,
                                ids=ids,
                                autocorr=TRUE,
                                main=plotTitle,
                                ...)

    return(xplot)
  }
