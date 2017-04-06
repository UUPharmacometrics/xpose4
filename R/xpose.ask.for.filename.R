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



#' Function to ask the user for the name of a file
#' 
#' Asks the user for the name of a file.
#' 
#' Function checks if the file exists, if it does then the filename is returned
#' from the function.
#' 
#' @aliases xpose.ask.for.filename xpose.ask.for.mod xpose.ask.for.lst
#' @param object An \code{\link{xpose.data}} object.
#' @param listfile A NONMEM output file
#' @param modfile A NONMEM model file
#' @param \dots Additional arguments passed to the function
#' @return The name of the file if it exists, otherwise nothing is returned.
#' @author Niclas Jonsson, Justin Wilkins, Mats Karlsson and Andrew Hooker
#' @keywords internal
# @export xpose.ask.for.filename
xpose.ask.for.filename <-
  function(object,
           listfile=paste("run",object@Runno,".lst",sep=""),
           modfile=paste("run",object@Runno,".mod",sep=""),
           ...) {

    cat("Type the name of the output file (0=cancel, return=",listfile,")\n",sep="")
    ans <- readline()

    lstfile <- NULL
    if(ans==0) {
      return()
    } else if (ans=="") {
      if(is.readable.file(listfile)) {
        lstfile <- listfile
      }
    } else {
      if(is.readable.file(ans)) {
        lstfile <- ans
      }
    }
    
    if(is.null(lstfile)) {
      cat("The specified file couldn't be found in the current directory.\n")
      return()
    }

    
    
  }

xpose.ask.for.lst <-
  function(object,
           listfile=paste("run",object@Runno,".lst",sep=""),
           ...) {
    cat("Type the name of the output file (0=cancel, return=",
        listfile,")\n",sep="")
    ans <- readline()

    lstfile <- NULL
    if(ans==0) {
      return(NULL)
    } else if (ans=="") {
      if(is.readable.file(listfile)) {
        lstfile <- listfile
      }
    } else {
      if(is.readable.file(ans)) {
        lstfile <- ans
      }
    }
    
    if(is.null(lstfile)) {
      cat("The specified file couldn't be found in the current directory.\n")
      return(NULL)
    } else {
      return(lstfile)
    }
  }


xpose.ask.for.mod <-
  function(object,
           modfile=paste("run",object@Runno,".mod",sep=""),
           ...) {
    cat("Type the name of the model file (0=cancel, return=",
        modfile,")\n",sep="")
    ans <- readline()

    cmdfile <- NULL
    if(ans==0) {
      return(NULL)
    } else if (ans=="") {
      if(is.readable.file(modfile)) {
        cmdfile <- modfile
      }
    } else {
      if(is.readable.file(ans)) {
        cmdfile <- ans
      }
    }
    
    if(is.null(cmdfile)) {
      cat("The specified file couldn't be found in the current directory.\n")
      return(NULL)
    } else {
      return(cmdfile)
    }
  }
