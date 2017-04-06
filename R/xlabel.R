#' Extract and set labels for Xpose data items.
#' 
#' This function extracts and sets label definitions in Xpose data objects.
#' 
#' \code{x} should be a string exactly matching the name of a column in the
#' data.frame in the Data slot of an xpose.data object. The name of columns
#' defined through xpose variable definitions (see \code{\link{xpose.data}})
#' can be extracted using the \code{xvardef} function and to be used in the
#' \code{xlabel} function, e.g. \code{xlabel(xvardef("dv",object),object)},
#' which would give the label for the \code{dv} variable.
#' 
#' @param x Name of the variable to assign a label to.
#' @param object An \code{xpose.data} object.
#' @param value A two element vector of which the first element is the name of
#' the variable and the second the label
#' @return The label of the specified column.
#' @author Niclas Jonsson
#' @seealso \code{\link{xpose.prefs-class}}, \code{\link{xvardef}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## xpdb5 is an Xpose data object
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' 
#' ## Display label for dependent variable in the Xpose data object
#' xlabel(xvardef("dv", object), object)
#' 
#' ## Set label for dependent variable
#' xlabel(xpdb5) <- c(xvardef("dv", object), "Concentration (mg/L)")
#' }
#' 
#' @export xlabel
xlabel <- function(x,object) {

  if(length(x)==1) {
    return(object@Prefs@Labels[[x]])
  } else {
    return(unlist(object@Prefs@Labels[x]))
  }
}

#' @describeIn xlabel sets label definitions in Xpose data objects.  assigned value should be a two-element vector 
#' of which the first element is the name of
#' the variable and the second the label
"xlabel<-" <- function(object,value) {

  ## value is a two element vector of which the first element is the
  ## name of the variable and the second the label
  object@Prefs@Labels[value[1]] <- value[2]

  return(object)
}
  
  
