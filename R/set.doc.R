#' @describeIn change_misc_parameters set the documentation field in the Xpose data object.
#' @export
set.doc<-
  function(object, classic = FALSE)
  {
    cat("Type any documentation for the new database and finish with\n")
    cat("a blank line:\n")
    doc <- scan(what = character(), sep = "\n")
    
    dat <- object
    dat@Doc <- doc
    
    if (classic==TRUE) {
      c1<-call("assign",paste("xpdb", object@Runno, sep = ""), data, immediate=T, envir = .GlobalEnv)
      eval(c1)
      c2<-call("assign",pos = 1, ".cur.db", eval(as.name(paste("xpdb", object@Runno, sep = ""))))
      eval(c2)
      return(cat(""))
    } else {
      return(dat)
    }
  }
