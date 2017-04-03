
#' Exports Xpose graphics settings to a file.
#' 
#' This function exports graphics settings for a specified Xpose data object to
#' a file.
#' 
#' This function exports the graphics settings (contents of
#' object@Prefs@Graph.prefs) for a given \code{xpose.data} object to a file,
#' typically 'xpose.ini'. It is a wrapper for \code{xpose.write}. Note that the
#' file format is not the same as is used in
#' \code{\link{import.variable.definitions}} and
#' \code{\link{export.variable.definitions}}.
#' 
#' @aliases export.graph.par xpose.write
#' @param object An \code{xpose.data} object.
#' @param file The file to contain exported Xpose settings.
#' @return Null.
#' @author Niclas Jonsson & Justin Wilkins
#' @seealso \code{\link{import.graph.par}}, \code{\link{xpose.prefs-class}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## xpdb5 is an Xpose data object
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' 
#' ## For a filename prompt
#' export.graph.par(xpdb5)
#' 
#' ## Command-line driven
#' xpose.write(xpdb5, "c:/XposeSettings/mytheme.ini")
#' }
#' 
#' @export export.graph.par
"export.graph.par" <- function(object) {

  cat("\nPlease type a filename to export the current graphics settings to.\n")

  ans <- readline()

  if (ans!="") {
    xpose.write(object, file=ans)
  } else {
    xpose.write(object, file="xpose.ini")
  }
  return(cat(""))

}
