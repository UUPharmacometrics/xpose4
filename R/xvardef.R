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



#' Extracts and set Xpose variable definitions.
#' 
#' This function extracts and set Xpose variable definitions in "xpose.data"
#' objects.
#' 
#' The Xpose variable definitions are used to map particular variable types to
#' column names in the data.frame in the Data slot of the "xpose.data" object.
#' The single-valued Xpose variable definitions are: \code{id, idlab, idv, occ,
#' dv, pred, ipred, iwres, res}. The (potentially) vector-valued Xpose variable
#' definitions are: \code{parms, covariates, ranpar, tvparms} (parameters,
#' covariates, random effects parameters=etas, typical value parameters). The
#' default values of these can be found in the \code{createXposeClasses}
#' function.
#' 
#' @aliases xvardef xvardef<-
#' @param x The name of an xpose variable (see below).
#' @param object An \code{xpose.data} object.
#' @param value A two element vector of which the first element is the name of
#' the variable and the second the column name in the Data slot of the object.
#' @return Returns a string with the name of the data variable defined as the
#' Xpose data variable.
#' @author Niclas Jonsson
#' @seealso \code{\link{xpose.data-class}},\code{\link{xpose.prefs-class}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## xpdb5 is an Xpose data object
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' 
#' ## get the column name in the Data slot of object xpdb5
#' ## corresponding to the label dv
#' xvardef("dv", xpdb5)
#' 
#' ## reset the which column the label dv points to in the Data slot of
#' ## object xpdb5
#' xvardef(xpdb5) <- c("dv", "DVA")
#' }
#' 
#' @export xvardef
xvardef <- function(x,object) {

  return(object@Prefs@Xvardef[[x]])
}

"xvardef<-" <- function(object,value) {

  ## value is a two element vector of which the first element is the
  ## name of the variable and the second the label
  object@Prefs@Xvardef[value[1]] <- value[2]

  return(object)
}
