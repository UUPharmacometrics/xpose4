#' @describeIn change_graphical_parameters responsible for dilution
#' preferences
#' @export
"change.dil.graph.par"  <- function(object, classic = FALSE)
{
  data <- object
  
  cat("These are the current data dilution settings:\n\n")
  cat(paste("Dilution type:",data@Prefs@Graph.prefs$diltype,sep=" "),"\n")
  cat(paste("Fraction to be diluted:",data@Prefs@Graph.prefs$dilfrac,sep=" "),"\n")
  cat(paste("Dilution confidence interval:", data@Prefs@Graph.prefs$dilci,sep=" "),"\n")
  cat("\n")

  # gr.stngs <- xp.gr.stngs
  
  cat("Stratified dilution?\n\n")
  cat("(NULL specifies random dilution without stratification, and is the\n")
  cat("default. Anything else indicates that stratified dilution is to be used.): \n\n")
  ans <- readline()
  if(ans!="") {
    data@Prefs@Graph.prefs$diltype <- ans
  }
  
  cat("Specify the fraction eligible for dilution.\n\n")
  cat("(a number, between 0 and 1, defining the fraction of the data to\n") 
  cat("be diluted. The default is 0.7.): \n\n")
  ans <- readline()
  if(ans!="") {
    data@Prefs@Graph.prefs$dilfrac <- as.numeric(ans)
  }
  
  cat("Specify the confidence interval for stratified dilution.\n\n")
  cat("(a number, between 0 and 1, defining the range of the data\n") 
  cat("eligible for dilution. The default is 0.95.): \n\n")
  ans <- readline()
  if(ans!="") {
    data@Prefs@Graph.prefs$dilci <- as.numeric(ans)
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
