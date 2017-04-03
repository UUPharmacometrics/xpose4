#' @describeIn change_graphical_parameters sets preferences for loess smooths.
#' @export
"change.smooth.graph.par"  <- function(object, classic = FALSE)
{
  data <- object
  
  cat("These are the current settings for the smooth:\n\n")
  cat(paste("Use smooth:",data@Prefs@Graph.prefs$suline,sep=" "),"\n")
  cat(paste("Line color:",data@Prefs@Graph.prefs$sucol,sep=" "),"\n")
  cat(paste("Line type:", data@Prefs@Graph.prefs$sulty,sep=" "),"\n")
  cat(paste("Line width:",data@Prefs@Graph.prefs$sulwd,sep=" "),"\n")
  cat(paste("Span:",data@Prefs@Graph.prefs$suspan,sep=" "),"\n")
  cat(paste("Degree:",data@Prefs@Graph.prefs$sudegr,sep=" "),"\n")
  cat("\n")

  # gr.stngs <- xp.gr.stngs
  
  cat("Should smooths be included by default?\n\n")
  cat("(If not NULL, the value of the string specifies the y variable to\n")
  cat("use.): \n\n")
  ans <- readline()
  if(ans!="") {
    data@Prefs@Graph.prefs$suline <- ans
  }

  cat("\nSpecify a new line color or leave blank to keep unchanged.\n\n")
  cat("(Specified as an integer or a text string. A full list is obtained \n")
  cat("by the R command 'colours()'. The default is blue (col=4).): \n\n")
  ans <- readline()
  if ((ans!="") && (!is.na(as.numeric(ans)))) {
    data@Prefs@Graph.prefs$sucol <- as.numeric(ans)
  } else {
    data@Prefs@Graph.prefs$sucol <- ans
  }

  cat("\nSpecify a new line type or leave blank to keep unchanged.\n\n")
  cat("(Line types are specified as an integer (0=blank, 1=solid, \n")
  cat("2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash).) \n\n")
  ans <- readline()
  if(ans!="") {
    data@Prefs@Graph.prefs$sulty <- as.numeric(ans)
  }

  cat("\nSpecify a new line width or leave blank to keep unchanged.\n\n")
  cat("(A positive real number): \n\n")
  ans <- readline()
  if(ans!="") {
    data@Prefs@Graph.prefs$sulwd <- as.numeric(ans)
  }

  cat("\nSpecify a new smoothness parameter or leave blank to keep unchanged.\n\n")
  cat("(A positive real number between 0 and 1): \n\n")
  ans <- readline()
  if(ans!="") {
    data@Prefs@Graph.prefs$suspan <- as.numeric(ans)
  }
  
  cat("\nSpecify a new polynomial degree or leave blank to keep unchanged.\n\n")
  cat("(A positive real number between 0 and 2): \n\n")
  ans <- readline()
  if(ans!="") {
    data@Prefs@Graph.prefs$sudegr <- as.numeric(ans)
  }

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
