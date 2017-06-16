#' Simulated prazosin Xpose database.
#' 
#' Xpose database from the NONMEM output of a model for prazosin using
#' simulated data (and NONMEM 7.3).
#' 
#' The database can be used to test functions in Xpose 4.  This database is
#' slightly different than the database that is created when reading in the
#' files created by \code{\link{simprazExample}} using
#' \code{\link{xpose.data}}.
#' 
#' @name simpraz.xpdb
#' @docType data
#' @format an xpose.data object  
#' @seealso \code{\link{simprazExample}}
#' @keywords datasets
#' @examples
#' 
#' xpose.print(simpraz.xpdb)
#' Data(simpraz.xpdb)
#' str(simpraz.xpdb)
#' 
"simpraz.xpdb"
