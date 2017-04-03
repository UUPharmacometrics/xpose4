#' Data functions for Xpose 4
#' 
#' These functions perform various tasks in managing Xpose data objects.
#' 
#' These are internal Xpose functions, not intended for direct use.
#' 
#' @param vars List of variables to be checked.
#' @param object An \code{xpose.data} object.
#' @param silent A logical operator specifying whether output should be
#' displayed.
#' @param filename A filename to be checked for readability.
#' @return TRUE, FALSE or NULL.
#' @author Niclas Jonsson and Andrew Hooker
#' @seealso \code{\link{xpose.prefs-class}}, \code{\link{xvardef}}
#' @keywords internal

check.vars <- function(vars,object,silent=FALSE) {

  for (v in vars) {

    if(is.null(xvardef(v,object))) {
      if(is.na(pmatch(v,names(object@Data)))){
        if(!silent) {
          cat(paste("\n","-----------Variable(s) not defined!-------------\n",
                    v, "is/are not defined in the current database\n",
                    "and must be defined for this command to work!\n",
                    "------------------------------------------------\n"))
        }
        return(NULL)
      }
    }
  }
  return(TRUE)
}

