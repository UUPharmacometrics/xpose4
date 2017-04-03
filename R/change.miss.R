#' @describeIn change_misc_parameters change the value to use as
#' 'missing'.
#' @export
"change.miss" <- function(object, classic=FALSE)
{
  data <- object
  cat("\nPlease type the number to be treated as missing data (it is currently\n")
  cat("set to",data@Prefs@Miss,") (0=exit)\n")
  ans <- readline()
  if(ans == "0") 
    return(cat(""))

  if (is.na(as.numeric(ans))) {
    ans <- NULL
    return(cat("The data value must be numeric.\n"))
    # Recall(object)
  }
  
  if (!is.na(as.numeric(ans))) {
    data@Prefs@Miss <- as.numeric(ans)
        if (classic==TRUE) {
          c1<-call("assign",paste("xpdb", object@Runno, sep = ""), data, immediate=T, envir = .GlobalEnv)
          eval(c1)
          c2<-call("assign",pos = 1, ".cur.db", eval(as.name(paste("xpdb", object@Runno, sep = ""))))
          eval(c2)
          return(cat(""))
          
        } else {
          return(data)
        }
  }

}
