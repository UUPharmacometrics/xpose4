
#' Prints the contents of an Xpose data object
#' 
#' These functions print a summary of the specified Xpose object to the R
#' console.
#' 
#' These functions return a detailed summary of the contents of a specified
#' \code{\link{xpose.data}} object.
#' 
#' @param object An \code{xpose.data} object.
#' @return A detailed summary of the contents of a specified
#' \code{\link{xpose.data}} object.
#' @author Niclas Jonsson & Justin Wilkins
#' @seealso \code{\link{xpose.data}}
#' @keywords methods
#' @examples
#' db.names(simpraz.xpdb)
#' 
#' @export 
#' @family data functions 
db.names<-
  function(object)
  {
    ### Displays the name of the current database.
    if(exists("object")) {
      cat(paste("\nThe current run number is ", object@Runno, ".\n\n", sep=""))
      
      xpose.print(object,long=T)
    }
    else {
      cat("The current run number is", object@Runno, 
          "but no matching database was found.\n")
    }
    return()
  }
