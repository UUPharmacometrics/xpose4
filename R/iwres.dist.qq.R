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



#' Quantile-quantile plot of individual weighted residuals (IWRES), for Xpose 4
#' 
#' This is a QQ plot of the distribution of individual weighted residuals
#' (IWRES) in the dataset, a specific function in Xpose 4. It is a wrapper
#' encapsulating arguments to the \code{xpose.plot.qq} function.
#' 
#' Displays a QQ plot of the individual weighted residuals (IWRES).
#' 
#' @param object An xpose.data object.
#' @param \dots Other arguments passed to \code{link{xpose.plot.qq}}.
#' @return Returns a QQ plot of individual weighted residuals (IWRES).
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.plot.qq}}, \code{\link{xpose.panel.qq}},
#' \code{\link[lattice]{qqmath}}, \code{\link{xpose.prefs-class}},
#' \code{\link{xpose.data-class}}
#' @keywords methods
#' @examples
#' iwres.dist.qq(simpraz.xpdb)
#' 
#' @export iwres.dist.qq
#' @family specific functions 
"iwres.dist.qq" <-
  function(object,
           ...) {
    
    
    ## Make sure we have the necessary variables defined
    if(is.null(check.vars(c("iwres"),object))) {
      return(NULL)
    }
    

    xplot <- xpose.plot.qq(xvardef("iwres",object),
                           object,
                           ...)

    return(xplot)
  }
   
