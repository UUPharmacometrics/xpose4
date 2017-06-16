
#' Imports Xpose variable definitions from a file to an Xpose data object.
#' 
#' This function imports variable definitions for a specified Xpose data object
#' from a file.
#' 
#' This function imports variable definitions (contents of object@Prefs@Xvardef)
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
#' @family data functions 
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
