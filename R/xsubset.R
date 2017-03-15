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



#' Extract or set the value of the Subset slot.
#' 
#' Extract or set the value of the Subset slot of an "xpose.data" object.
#' 
#' The subset string has the same syntax as the subset argument to, e.g.
#' \code{panel.xyplot}. Note, however, that the "xpose.data" subset is not used
#' as an argument to \code{panel.xyplot}. It is intended as the subset argument
#' to the \code{Data} and \code{SData} functions.
#' 
#' @aliases xsubset xsubset<-
#' @param object An "xpose.data" object.
#' @param value A string with the subset expression.
#' @return A string representing the subset expression.
#' @author Niclas Jonsson
#' @seealso \code{\link{Data}}, \code{\link{SData}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## xpdb5 is an Xpose data object
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' 
#' xsubset(xpdb5) <- "DV > 0"
#' xsubset(xpdb5)
#' }
#' 
#' @export xsubset
xsubset <- function(object) {
  return(object@Prefs@Subset)
}

"xsubset<-" <- function(object,value) {
  if(is.null(value)) return(object)

  object@Prefs@Subset <- value
  return(object)
}
