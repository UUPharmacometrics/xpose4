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



#' Exports Xpose variable definitions to a file from an Xpose data object.
#' 
#' This function exports variable definitions for a specified Xpose data object
#' to a file.
#' 
#' This function exports variable defintions (contents of object@Prefs@Xvardef)
#' for a given \code{xpose.data} object to a file, typically
#' 'xpose.vardefs.ini'.  Note that file format is not the same as used for
#' graphics settings. It is a wrapper for the R function \code{\link{dput}}.
#' 
#' @param object An \code{xpose.data} object.
#' @param file A file name as a string.
#' @return Null.
#' @author Niclas Jonsson & Justin Wilkins
#' @seealso \code{\link{import.variable.definitions}},
#' \code{\link{xpose.prefs-class}} \code{\link{dput}}
#' @keywords methods
#' @examples
#' od = setwd(tempdir()) # move to a temp directory
#' (cur.files <- dir()) # current files in temp directory
#' 
#' export.variable.definitions(simpraz.xpdb,file="xpose.vardefs.ini")
#' (new.files <- dir()[!(dir() %in% cur.files)])  # what files are new here?
#' 
#' file.remove(new.files) # remove this file
#' setwd(od)  # restore working directory
#' 
#' 
#' @export export.variable.definitions
#' @family data functions 
"export.variable.definitions"  <- function(object,file="")
{
  if(file==""){
    cat("Please type a filename to export the current variable definitions to.\n")
    cat("Note that backslashes need to be escaped, e.g. \"C:\\\\Xpose\\\".\n")
    
    ans <- readline()
  } else {
    ans <- file
  }
  if (ans!="") {
    dput(object@Prefs@Xvardef,file=ans)
  } else {
    dput(object@Prefs@Xvardef,file="xpose.vardefs.ini")
  }
  return(cat(""))
}
