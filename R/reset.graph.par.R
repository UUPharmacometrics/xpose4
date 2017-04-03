#' Resets Xpose variable definitions to factory settings
#' 
#' Function to reset Xpose's graphics parameters definitions to the default.
#' 
#' This functions is used to reset Xpose's graphic settings definitions to
#' their default values. Graphical settings are read from the file 'xpose.ini'
#' in the root of the 'xpose4' package.
#' 
#' @param object An \code{xpose.data} object.
#' @param classic A logical operator specifying whether the function should
#' assume the classic menu system. This is an internal option and need never be
#' called from the command line.
#' @return An \code{\link{xpose.data}} object (classic == FALSE) or null
#' (classic == TRUE).
#' @author Niclas Jonsson & Justin Wilkins
#' @seealso \code{\link{xpose.prefs-class}}, \code{\link{import.graph.par}},
#' \code{\link{change.xvardef}}
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
#' ## Reset to default values
#' xpdb5 <- reset.graph.par(xpdb5)
#' 
#' ## Change WRES definition
#' xpdb5 <- change.wres(xpdb5)
#' }
#' 

"reset.graph.par" <- function(object, classic=FALSE) {

  xpobj <- object

    rhome   <- R.home()
    xdefini <- paste(rhome, "\\library\\xpose4\\xpose.fac", sep="")

    # read global options
    if (is.readable.file(xdefini)) {
      xpobj <- xpose.read(object, file=xdefini)
    } else {
      cat("No factory settings found! Check that the file 'xpose.fac'\n")
      cat("is available and readable in your Xpose package root folder\n")
      cat(paste("(", xdefini, ").", sep=""))
      return(cat(""))
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
