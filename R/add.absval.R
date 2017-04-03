#' Column-transformation functions for Xpose 4
#' 
#' These functions transform existing Xpose 4 data columns, adding new columns.
#' 
#' These functions may be used to create new data columns within the Xpose data
#' object by transforming existing ones.
#' 
#' @param object An \code{xpose.data} object.
#' @param listall A logical operator specifying whether the items in the
#' database should be listed.
#' @param classic A logical operator specifying whether the function should
#' assume the classic menu system.  This is an internal option and need never
#' be called from the command line.
#' @return An \code{\link{xpose.data}} object (classic == FALSE) or null
#' (classic == TRUE).
#' @author Niclas Jonsson, Justin Wilkins and Andrew Hooker
#' @seealso \code{\link{xpose.data}}
#' @examples
#' 
#' \dontrun{
#' ## xpdb5 is an Xpose data object
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' 
#' ## Create a column containing the absolute values of data in another 
#' ## column
#' add.absval(xpdb5)
#' 
#' ## Create a categorical data column based on a continuous data column, 
#' ## and do not list variables 
#' add.dichot(xpdb5, listall = FALSE)
#' 
#' ## Create a column containing the exponentiated values of data in 
#' ## another column
#' add.exp(xpdb5)
#' 
#' ## Create a column containing log-transformations of data in another 
#' ## column
#' add.log(xpdb5)
#' 
#' ## Create a time-after-dose column
#' add.tad(xpdb5)
#' }
#' 
#' @family data functions
#' @name add_transformed_columns
NULL

 
#' @export
#' @describeIn add_transformed_columns Create a column containing the absolute values of data 
#' in another column.
"add.absval" <- function(object, listall=TRUE, classic=FALSE )
{
  if(listall) db.names(object)

  cat("Please type the names of the items to be converted to\n")
  cat("absolute values, one per line, and finish with a blank line.\n")

  items <- scan(what=character())
  data <- object@Data
  sdata <- object@SData
  nams <- names(data)
  for(i in items) {
    if(is.na(match(i, nams))) {
      cat("No match: ", i, "\n", sep="")
      next
    }

    nam <- paste("abs", i, sep="")
    #cat(nam)
    newit <- abs(data[[i]])
    if(any((data[[i]]-newit) !=0)) {
      data[[nam]] <- newit
      #vname(data[,nam]) <- nam
    }
    if(!all(is.null(sdata))){ 
       newits <- abs(sdata[[i]])
       if(any((sdata[[i]]-newits) !=0)) {
         sdata[[nam]] <- newits
         #vname(data[,nam]) <- nam
       }
    }
  }
  object@Data <- data
  object@SData <- sdata
  
  for (i in items) {
    expitem <- paste("abs", i, sep="")
    object@Prefs@Labels[[expitem]] <- c(paste("abs(", i, ")", sep=""))
  }
  
  if (classic==TRUE) {
    c1<-call("assign",paste("xpdb", object@Runno, sep = ""),object,envir=.GlobalEnv)
    eval(c1)
    #assign(paste("xpdb", object@Runno, sep = ""), object, immediate=T, envir = .GlobalEnv)
    c2<-call("assign",pos = 1, ".cur.db", eval(as.name(paste("xpdb", object@Runno, sep = ""))))
    eval(c2)
    #assign(pos = 1, ".cur.db", eval(as.name(paste("xpdb", object@Runno, sep = ""))))    
    return(cat(""))
  } else {
    return(object)
  }

}



