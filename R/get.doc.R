#' @describeIn change_misc_parameters get the documentation field in the Xpose data object.
#' @export
"get.doc" <- function(object, classic=FALSE)
{
  if(is.null(doc <- object@Doc)) {
    cat("The current database does not contain any documentation.\n")
    if (classic == T) {
      cat("Do you wish to add some? (y/n)\n")
      ans <- readline()
      if(ans == "y" || ans == "" || ans == "Y") {
        get.doc(object, classic=classic)  
      }
      invisible()
      return()
    }
  }

  cat(doc,fill=60)
  invisible()
  return(cat(""))

}
