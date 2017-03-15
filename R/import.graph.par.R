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



#' Imports Xpose graphics settings from a file to an Xpose data object.
#' 
#' This function imports graphics settings for a specified Xpose data object
#' from a file.
#' 
#' This function imports graphics settings (contents of
#' object@Prefs@Graph.prefs) for a given \code{xpose.data} object from a file,
#' typically 'xpose.ini'. It is a wrapper for \code{xpose.read}. It returns an
#' \code{xpose.data} object.  Note that the file format is not the same as is
#' used in \code{\link{import.variable.definitions}} and
#' \code{\link{export.variable.definitions}}.
#' 
#' @aliases import.graph.par xpose.read
#' @param object An \code{xpose.data} object.
#' @param classic A logical operator specifying whether the function should
#' assume the classic menu system. This is an internal option and need never be
#' called from the command line.
#' @param file The file where default graphical settings are located.
#' @return An \code{\link{xpose.data}} object (classic = FALSE) or null
#' (classic = TRUE).
#' @author Niclas Jonsson & Justin Wilkins
#' @seealso \code{\link{export.graph.par}}, \code{\link{xpose.prefs-class}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## xpdb5 is an Xpose data object
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' 
#' ## Import graphics preferences you saved earlier using export.graph.par
#' xpdb5 <- import.graph.par(xpdb5)
#' 
#' ## Command-line driven
#' xpdb5 <- xpose.read(xpdb5, "c:/XposeSettings/mytheme.ini")
#' }
#' 
#' @export import.graph.par
"import.graph.par" <- function(object, classic=FALSE) {

  cat("\nPlease type a filename to import the current graphics settings from.\n")
  cat("Note that backslashes need to be escaped, e.g. \"C:\\\\Xpose\\\\\".\n")

  ans <- readline()

  if (ans!="") {
    if (is.readable.file(ans)) {
      xpobj <- xpose.read(object, file=ans)
    } else {
      cat("This file does not appear to exist, or is unreadable!\n")
      return(cat(""))   
    }
  } else {
    if (is.readable.file("xpose.ini")) {
      xpobj <- xpose.read(object, file="xpose.ini")
      cat("Using 'xpose.ini' in the current working directory.\n")
    } else {
      cat("Using factory settings.\n")
      xpobj <- reset.graph.par(object)
    }
  }
  
  if (classic==TRUE) {
    c1<-call("assign",paste("xpdb", object@Runno, sep = ""), data, immediate=T, envir = .GlobalEnv)
    eval(c1)
    c2<-call("assign",pos = 1, ".cur.db", eval(as.name(paste("xpdb", object@Runno, sep = ""))))
    eval(c2)
    return(cat(""))
    
  } else {
    return(xpobj)
  }

}
