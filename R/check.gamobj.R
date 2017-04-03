check.gamobj <-
  function() {
    ## another function definition
    getit <- function()
    {
      cat("\nYou have to specify the parameter name and the run number",
          "of the gam objects you want to plot. The following", 
          "gam objects are available:\n", fill = 60)
      if(.Platform$OS == "windows") {
        cat(objects(pattern="gam.xpose*",pos=1),fill=60)
      } else {
        cat(objects(pattern = "^gam.xpose",pos=1), fill = 60)
      }
      cat("\nParameter (0 to exit): ")
      ans <- readline()
      if(ans == 0){
        return(ans <- NULL)
      }
      cat("Run number (0 to exit):")
      ans1 <- readline()
      if(ans1 == 0){
        return(ans1 <- NULL)
      }
      gobjname <- paste("gam.xpose.", ans, ".", ans1, sep = "")
      if(!exists(gobjname, where = 1)) {
        cat("\n*There are no object that matches", gobjname, 
            "\n")
        gobjname <- Recall()
      }
      return(gobjname)
    }
    ##
    ## The real code starts here
    ##
    if(exists("current.gam", where = 1)) {
      
      cat("\nThe current GAM object is for",
          eval(parse(text=paste("current.gam","$pars",sep=""))),
          #current.gam$pars,
          "in run",
          #current.gam$runno,
          eval(parse(text=paste("current.gam","$runno",sep=""))),
          ".\n")
      cat("\nDo you want to proceed with this gam object? y(n) ")
      ans <- readline()
      if(ans != "y" && ans != "") {
        gobjname <- getit()
        if(!is.null(gobjname)){
          c1 <- call("assign",pos = 1, "current.gam", eval(as.name(gobjname)),immediate=T)
          eval(c1)
        }
      } else {
        gobjname <- T
      }
    }  else {
      gobjname <- getit()
      if(!is.null(gobjname)){
        c2 <- call("assign",pos=1, "current.gam", eval(as.name(gobjname)),immediate=T)
        eval(c2)
      }
    }
    return(gobjname)
  }






