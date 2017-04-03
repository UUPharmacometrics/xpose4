#' @describeIn change_graphical_parameters responsible for linear regression
#' lines.
#' @export
"change.lm.graph.par"  <- function(object, classic = FALSE)
{
  data <- object
  
  cat("These are the current settings for linear regression lines:\n\n")
  cat(paste("Use linear regression line:",data@Prefs@Graph.prefs$lmline,sep=" "),"\n")
  cat(paste("Line color:",data@Prefs@Graph.prefs$lmcol,sep=" "),"\n")
  cat(paste("Line type:", data@Prefs@Graph.prefs$lmlty,sep=" "),"\n")
  cat(paste("Line width:",data@Prefs@Graph.prefs$lmlwd,sep=" "),"\n")
  cat("\n")

  # gr.stngs <- xp.gr.stngs
  
  cat("Should linear regression lines be included by default?\n\n")
  cat("(TRUE or FALSE(=NULL).): \n\n")
  ans <- readline()
  if(ans!="") {
    if (ans=="NULL") {
      ans = "FALSE"
    }
    data@Prefs@Graph.prefs$suline <- as.logical(ans)
  }

  cat("\nSpecify a new line color or leave blank to keep unchanged.\n\n")
  cat("(Specified as an integer or a text string. A full list is obtained \n")
  cat("by the R command 'colours()'. The default is blue.): \n\n")
  ans <- readline()
  if ((ans!="") && (!is.na(as.numeric(ans)))) {
    data@Prefs@Graph.prefs$lmcol <- as.numeric(ans)
  } else {
    data@Prefs@Graph.prefs$lmcol <- ans
  }

  cat("\nSpecify a new line type or leave blank to keep unchanged.\n\n")
  cat("(Line types are specified as an integer (0=blank, 1=solid, \n")
  cat("2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash).) \n\n")
  ans <- readline()
  if(ans!="") {
    data@Prefs@Graph.prefs$lmlty <- as.numeric(ans)
  }

  cat("\nSpecify a new line width or leave blank to keep unchanged.\n\n")
  cat("(A positive real number): \n\n")
  ans <- readline()
  if(ans!="") {
    data@Prefs@Graph.prefs$lmlwd <- as.numeric(ans)
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
