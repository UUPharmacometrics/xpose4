
#' Functions changing variable definitions in Xpose 4
#' 
#' These functions allow customization of Xpose's graphics settings.
#' 
#' Settings can be saved and loaded using \code{\link{export.graph.par}} and
#' \code{\link{import.graph.par}}, respectively.
#' 
#' @param object An \code{xpose.data} object.
#' @param classic A logical operator specifying whether the function should
#' assume the classic menu system. This is an internal option and need never be
#' called from the command line.
#' @return An \code{\link{xpose.data}} object (classic == FALSE) or null
#' (classic == TRUE).
#' @author Niclas Jonsson & Justin Wilkins
#' @seealso \code{\link{xpose.plot.default}},\code{\link{xpose.panel.default}},
#' \code{\link{xpose.plot.bw}},\code{\link{xpose.panel.bw}},
#' \code{\link{xpose.plot.default}},\code{\link{import.graph.par}},
#' \code{\link{export.graph.par}},\code{\link{plot.default}},
#' \code{\link{par}},\code{\link{import.graph.par}},\code{\link[lattice]{panel.abline}},
#' \code{\link[lattice]{panel.lmline}},\code{\link{lm}},\code{\link[lattice]{panel.loess}},
#' \code{\link{loess.smooth}},\code{\link{loess}},\code{\link[lattice]{panel.bwplot}},
#' \code{\link[lattice]{shingle}},\code{reorder.factor}
#' @examples
#' 
#' \dontrun{
#' ## xpdb5 is an Xpose data object
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' 
#' ## Change default miscellaneous graphic preferences
#' xpdb5 <- change.misc.graph.par(xpdb5)
#' 
#' ## Change default linear regression line preferences, creating a new 
#' ## object
#' xpdb5.a <- change.lm.graph.par(xpdb5)
#' 
#' ## Change conditioning preferences
#' xpdb5 <- change.cond.graph.par(xpdb5)
#' }
#' 
#' 
#' @name   change_graphical_parameters
#' @family data functions 
NULL

#' @describeIn change_graphical_parameters change settings for the line of
#' identity.
#' @export
change.ab.graph.par  <- function(object, classic = FALSE)
{
  data <- object
  
  cat("These are the current settings for lines of identity:\n\n")
  cat(paste("Use line of identity:",data@Prefs@Graph.prefs$abline,sep=" "),"\n")
  cat(paste("Line color:",data@Prefs@Graph.prefs$ablcol,sep=" "),"\n")
  cat(paste("Line type:", data@Prefs@Graph.prefs$abllty,sep=" "),"\n")
  cat(paste("Line width:",data@Prefs@Graph.prefs$abllwd,sep=" "),"\n")
  cat("\n")

  # gr.stngs <- xp.gr.stngs
  
  cat("Should a line of identity be included by default?\n\n")
  cat("(A 2-element list specifying whether, and if so, what\n")
  cat("type of line to automatically overlay on plots. NULL indicates\n")
  cat("no line unless overridden by a specific function (e.g. dv.vs.pred),\n")
  cat("c(1,1) indicates an x:y line, and c(1,2) indicates an x:2y line,\n")
  cat("for example. See R help on 'panel.abline' for more information.): \n\n")
  ans <- readline()
  if(ans!="") {
    data@Prefs@Graph.prefs$abline <- ans
  }

  cat("\nSpecify a new line color or leave blank to keep unchanged.\n\n")
  cat("(Specified as an integer or a text string. A full list is obtained \n")
  cat("by the R command 'colours()'. The default is black (col=1).): \n\n")
  ans <- readline()
  if ((ans!="") && (!is.na(as.numeric(ans)))) {
    data@Prefs@Graph.prefs$ablcol <- as.numeric(ans)
  } else {
    data@Prefs@Graph.prefs$ablcol <- ans
  }

  cat("\nSpecify a new line type or leave blank to keep unchanged.\n\n")
  cat("(Line types are specified as an integer (0=blank, 1=solid, \n")
  cat("2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash).) \n\n")
  ans <- readline()
  if(ans!="") {
    data@Prefs@Graph.prefs$abllty <- as.numeric(ans)
  }

  cat("\nSpecify a new line width or leave blank to keep unchanged.\n\n")
  cat("(A positive real number): \n\n")
  ans <- readline()
  if(ans!="") {
    data@Prefs@Graph.prefs$abllwd <- as.numeric(ans)
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
