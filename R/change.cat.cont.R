#' Functions changing miscellaneous parameter settings in Xpose 4
#' 
#' These functions allow viewing and changing of settings relating to subsets,
#' categorical threshold values, documentation and numbers indicating missing
#' data values.
#' 
#' @param object An \code{xpose.data} object.
#' @param classic A logical operator specifying whether the function should
#' assume the classic menu system. This is an internal option and need never be
#' called from the command line.
#' @param dv.cat.limit The limit for which we treat DV as categorical.  If
#' there are \code{dv.cat.limit} or less unique dv values then dv is treated as
#' categorical.
#' @param cat.limit The limit for which we treat a list of values as
#' categorical.  If there are \code{cat.limit} or less unique values then the
#' list is treated as categorical.
#' @param listall A logical operator specifying whether the items in the
#' database should be listed.
#' @param to.cat.vec A vector of strings specifying the names of the
#' categorical variables that should be transformed to continuous.
#' @param to.cont.vec A vector of strings specifying the names of the
#' continuous variables that should be transformed to categorical.
#' @param change.type.vec A vector of strings specifying the names of the
#' variables that should be transformed to/from continuous/categorical.
#' @param \dots arguments passed to other functions.
#' @param value This is the value that will be replaced in the xpose data
#' object \code{object}.  \code{value} is used in the \dQuote{replacement
#' function} version of these functions.  That is the form where we have
#' \code{function.name(object) <- value}.  If \code{value} is \code{NULL} then
#' the functions prompt the user for a value.  For \code{change.cat.levels},
#' \code{value} is the categorical limit \code{cat.limit}.  For
#' \code{change.dv.cat.levels}, \code{value} is the DV categorical limit
#' \code{dv.cat.limit}.  For \code{change.cat.cont}, \code{value} is the
#' \code{change.type.vec}.  See the examples below.
#' @return An \code{\link{xpose.data}} object, except \code{get.doc}, which
#' returns the value of object@Doc.
#' @author Andrew Hooker, Niclas Jonsson & Justin Wilkins
#' @seealso \code{\link{Data}}, \code{\link{SData}}, \code{\link{subset}},
#' \code{\link{xpose.data}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## xpdb5 is an Xpose data object
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' 
#' ## Change default subset
#' xpdb5 <- change.subset(xpdb5)
#' 
#' ## Set documentation field
#' xpdb5 <- set.doc(xpdb5)
#' ## View it
#' view.doc(xpdb5)
#' 
#' ## change the categorical limit for the dv variable
#' change.dv.cat.levels(xpdb5) <- 10
#' 
#' ## change the categorical limit for non DV variables
#' change.cat.levels(xpdb5) <- 2
#' ## or
#' xpdb5 <- change.cat.levels(xpdb5,cat.levels=2)
#' 
#' ## chnage variables from categorical to continuous
#' xpdb5 <- change.cat.cont(xpdb5,to.cat.vec=c("AGE"),to.cont.vec=c("SEX"))
#' xpdb5 <- change.cat.cont(xpdb5,change.type.vec=c("AGE","SEX"))
#' change.cat.cont(xpdb5) <- c("AGE","SEX")
#' }
#' 
#' @name   change_misc_parameters
#' @family data functions 

NULL

#' @describeIn change_misc_parameters allows interchange between categorical and continuous
#' data formats within the Xpose database. This in turn affects how plots are
#' drawn.
#' @export
change.cat.cont <-
  function(object,
           listall=TRUE,
           classic=FALSE,
           to.cat.vec=NULL,
           to.cont.vec=NULL,
           change.type.vec=NULL,
           ...){

    if(!is.null(change.type.vec)){
      for(j in change.type.vec){
        
        if(is.na(pmatch(j, names(object@Data)))) {
          cat("Couldn't find",j,"in the current database. Skipping!\n")
          next
        }

        if(is.factor(object@Data[,j])) {
          to.cont.vec <- c(to.cont.vec,j)
        } else {
          to.cat.vec <- c(to.cat.vec,j)
        }
      }
    }
    
    if(is.null(to.cont.vec) & is.null(to.cat.vec) & is.null(change.type.vec)){
      if(listall) {
        db.names(object)
      }
    }
    
    if(is.null(to.cont.vec) & is.null(to.cat.vec) & is.null(change.type.vec)){
      cat("\nPlease type the names of any CATEGORICAL variables you\n")
      cat("want to change into continuous, one per line, and end with a\n")
      cat("blank line:\n")
      cats <- scan(what=character())
    } else {
      cats <- to.cont.vec
    }

    if(length(cats)){
      for(i in cats) {
        
        if(is.na(pmatch(i, names(object@Data)))) {
          cat("Couldn't find",i,"in the current database. Skipping!\n")
          next
        }
        
        if(is.factor(object@Data[,i])){
          cat("\n  Transforming",i,"from categorical to continuous\n",sep=" ")
          object@Data[,i] <- as.numeric(levels(object@Data[,i]))[object@Data[,i]]
        } else {
          cat("  ",i," is already continuous, no changes made", sep="")
        }
      }
    }


    if(is.null(to.cont.vec) & is.null(to.cat.vec) & is.null(change.type.vec)){
      cat("Please type the names of any CONTINUOUS variables you\n")
      cat("want to change into categorical, one per line, and end with a\n")
      cat("blank line:\n")
      conts <- scan(what=character())
    } else {
      conts <- to.cat.vec
    }
    
    if(length(conts)){
      for(i in conts) {
        
        if(is.na(pmatch(i, names(object@Data)))) {
          cat("Couldn't find",i,"in current database. Skipping!\n")
          next
        }
        
        if(!is.factor(object@Data[,i])){
          cat("\n  Transforming",i,"from continuous to categorical\n",sep=" ")
          object@Data[,i] <- as.factor(object@Data[,i])
        } else {
          cat("  ",i," is already categorical, no changes made\n", sep="")
        }
      }
    }
    
    if (classic==TRUE) {
      c1<-call("assign",paste("xpdb", object@Runno, sep = ""), data, immediate=T, envir = .GlobalEnv)
      eval(c1)
      c2<-call("assign",pos = 1, ".cur.db", eval(as.name(paste("xpdb", object@Runno, sep = ""))))
      eval(c2)
    } 
    return(object)
  }

#' @describeIn change_misc_parameters allows interchange between categorical and continuous
#' data formats within the Xpose database. This in turn affects how plots are
#' drawn.
#' @export
"change.cat.cont<-" <-
  function(object,
           listall=TRUE,
           classic=FALSE,
           to.cat.vec=NULL,
           to.cont.vec=NULL,
           ...,
           value){
    object <- change.cat.cont(object,listall=listall,calssic=classic,
                              to.cat.vec=to.cat.vec,
                              to.cont.vec=to.cont.vec,
                              change.type.vec=value,
                              ...)
    return(object)
  }
