
#' Extract or set the value of the Subset slot.
#' 
#' Extract or set the value of the Subset slot of an "xpose.data" object.
#' 
#' The subset string has the same syntax as the subset argument to, e.g.
#' \code{panel.xyplot}. Note, however, that the "xpose.data" subset is not used
#' as an argument to \code{panel.xyplot}. It is intended as the subset argument
#' to the \code{Data} and \code{SData} functions.
#' 
#' @aliases xsubset xsubset<-
#' @param object An "xpose.data" object.
#' @param value A string with the subset expression.
#' @return A string representing the subset expression.
#' @author Niclas Jonsson
#' @seealso \code{\link{Data}}, \code{\link{SData}}
#' @keywords methods
#' @examples
#' xpdb <- simpraz.xpdb
#' xsubset(xpdb) <- "DV > 0"
#' xsubset(xpdb)
#' 
#' @export 
#' @family data functions 
xsubset <- function(object) {
  return(object@Prefs@Subset)
}

#' @describeIn xsubset assign value with a string representing the subset expression
#' @export
"xsubset<-" <- function(object,value) {
  if(is.null(value)) return(object)

  object@Prefs@Subset <- value
  return(object)
}
