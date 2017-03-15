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



#' Imports Xpose variable definitions from a file to an Xpose data object.
#' 
#' This function imports variable definitions for a specified Xpose data object
#' from a file.
#' 
#' This function imports variable defintions (contents of object@Prefs@Xvardef)
#' for a given \code{xpose.data} object from a file, typically
#' 'xpose.vardefs.ini'.  It returns an \code{xpose.data} object. Note that file
#' format is not the same as used for graphics settings. It is a wrapper for
#' the R function \code{\link{dget}}.
#' 
#' @param object An \code{xpose.data} object.
#' @param classic A logical operator specifying whether the function should
#' assume the classic menu system. This is an internal option and need never be
#' called from the command line.
#' @return An \code{\link{xpose.data}} object (classic == FALSE) or null
#' (classic == TRUE).
#' @author Niclas Jonsson & Justin Wilkins
#' @seealso \code{\link{export.variable.definitions}},
#' \code{\link{xpose.prefs-class}} \code{\link{dget}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## xpdb5 is an Xpose data object
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' 
#' xpdb5 <- import.variable.definitions(xpdb5)
#' }
#' 
#' @export import.variable.definitions
"import.variable.definitions"  <- function(object, classic = FALSE)
{
  
  cat("Please type the name of the file that hold the definitions to be\n")
  cat("imported. Note that backslashes need to be escaped, e.g. \"C:\\\\Xpose\\\\defs.txt\".\n")
  ans <- readline()

  data <- object
  if(is.readable.file(ans)) {
    data@Prefs@Xvardef <- dget(ans)
        if (classic==TRUE) {
          c1<-call("assign",paste("xpdb", object@Runno, sep = ""), data, immediate=T, envir = .GlobalEnv)
          eval(c1)
          c2<-call("assign",pos = 1, ".cur.db", eval(as.name(paste("xpdb", object@Runno, sep = ""))))
          eval(c2)
          return(cat(""))
          
        } else {
          return(data)
        }
  } else {
    cat("Couldn't find a file with that name!\n")
    return(cat(""))
  }

}
