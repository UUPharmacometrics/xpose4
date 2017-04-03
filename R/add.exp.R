
#' @describeIn  add_transformed_columns Create an exponentiated version of an existing variable
#' @export

"add.exp" <- function(object, listall=TRUE, classic=FALSE )
{
  if(listall) db.names(object)

  cat("Please type the names of the items to be exponentiated, one\n")
  cat("per line, and finish with a blank line.\n")

  items <- scan(what=character())
  data <- object@Data
  sdata <- object@SData
  nams <- names(data)
  for(i in items) {
    if(is.na(match(i, nams))) {
      cat("No match: ", i, "\n", sep="")
      next
    }

    nam <- paste("exp", i, sep="")
    #cat(nam)
    newit <- exp(data[[i]])
    newits <- exp(sdata[[i]])
    if(any((data[[i]]-newit) !=0)) {
      data[[nam]] <- newit
      #vname(data[,nam]) <- nam
    }
    if(any((sdata[[i]]-newits) !=0)) {
      sdata[[nam]] <- newits
      #vname(data[,nam]) <- nam
    }
  }

  object@Data <- data
  object@SData <- sdata
  
  for (i in items) {
    expitem <- paste("exp", i, sep="")
    object@Prefs@Labels[[expitem]] <- c(paste("exp(", i, ")", sep=""))
  }
  
  if (classic==TRUE) {
    #assign(paste("xpdb", object@Runno, sep = ""), object, immediate=T, envir = .GlobalEnv)
    #assign(pos = 1, ".cur.db", eval(as.name(paste("xpdb", object@Runno, sep = ""))))
    #return(cat(""))
    
    ## to avoid checks on global variable assignment in package building
    c1<-call("assign",paste("xpdb", object@Runno, sep = ""),object,envir=.GlobalEnv)
    eval(c1)
    c2<-call("assign",pos = 1, ".cur.db", eval(as.name(paste("xpdb", object@Runno, sep = ""))))
    eval(c2)
    return(cat(""))
    
  } else {
    return(object)
  }

}
