#' @describeIn change_misc_parameters change settings for the number of unique data
#' values required in a variable in order to define it as continuous for the dependent variable.
#' @export
change.dv.cat.levels <- function(object,
                                   classic = FALSE,
                                   dv.cat.limit=NULL,
                                   ...){
  if(is.null(dv.cat.limit)){
    cat("\nPlease type the number of unique data values beneath which the\n")
    cat("dependent variable will be treated as categorical (it is currently\n")
    cat("set to ",object@Prefs@DV.Cat.levels,".) (0=exit):\n",sep="")
    ans <- readline()
  } else {
    ans <- dv.cat.limit
  }

  ans <- as.numeric(ans)
  if (is.na(ans)){
    cat(paste("The data value must be numeric\n",
              "Nothing has been changed\n"))
    return(object)
  }
  if(ans == 0) {
    cat(paste("Nothing has been changed\n"))
    return(object)
  }
  if (ans<0){
    cat(paste("The data value must be greater than zero\n",
              "Nothing has been changed\n"))
    return(object)
  }
  
  if (!is.na(ans)) {
    object@Prefs@DV.Cat.levels <- ans
    Data(object) <- object@Data
    if (classic==TRUE) {
      c1<-call("assign",paste("xpdb", object@Runno, sep = ""), data, immediate=T, envir = .GlobalEnv)
      eval(c1)
      c2<-call("assign",pos = 1, ".cur.db", eval(as.name(paste("xpdb", object@Runno, sep = ""))))
      eval(c2)
    }
  }
  return(object)
}

#' @describeIn change_misc_parameters change settings for the number of unique data
#' values required in a variable in order to define it as continuous for the dependent variable.
#' @export
"change.dv.cat.levels<-" <-
  function(object,
           classic=FALSE,
           ...,
           value){
    object <- change.dv.cat.levels(object,calssic=classic,dv.cat.limit=value,...)
    return(object)
  }


