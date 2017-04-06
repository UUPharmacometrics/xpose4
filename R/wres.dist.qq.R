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

## Added by Justin Wilkins
## 28/11/2005



#' Quantile-quantile plot of weighted residuals (WRES), for Xpose 4
#' 
#' This is a QQ plot of the distribution of weighted residuals (WRES) in the
#' dataset, a specific function in Xpose 4. It is a wrapper encapsulating
#' arguments to the \code{xpose.plot.qq} function.
#' 
#' Displays a QQ plot of the weighted residuals (WRES).
#' 
#' @param object An xpose.data object.
#' @param \dots Other arguments passed to \code{link{xpose.plot.qq}}.
#' @return Returns a QQ plot of weighted residuals (WRES).
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.plot.qq}}, \code{\link{xpose.panel.qq}},
#' \code{\link[lattice]{qqmath}}, \code{\link{xpose.prefs-class}},
#' \code{\link{compute.cwres}}, \code{\link{xpose.data-class}}
#' @keywords methods
#' @examples
#' ## Here we load the example xpose database 
#' xpdb <- simpraz.xpdb
#' 
#' wres.dist.qq(xpdb)
#' 
#' @export wres.dist.qq
#' @family specific functions 
"wres.dist.qq" <-
  function(object,
           ...) {
    
    if(is.null(xvardef("wres",object))) {
      cat("WRES is not set in the database!\n")
      return()
    }
    
    xplot <- xpose.plot.qq(xvardef("wres",object),
                           object,
                           ...)
    
    return(xplot)
  }

