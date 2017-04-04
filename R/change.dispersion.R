
change.dispersion <-
  function(first=TRUE)
  {
    value <- .cur.db@Prefs@Gam.prefs$disp
    if (first==TRUE){
      cat("Use a dispersion factor (null/true)\n")
      if (is.null(value)) {
        cat("The current value is NULL...\n")
      } else {
        cat("The current value is",value,"...\n")
      }
      cat("\nPlease type the new value ")
    }
    
    ans <- readline()
    
    if(ans == "NULL" || ans == "null") {
      .cur.db@Prefs@Gam.prefs$disp <- NULL
      c1<-call("assign",pos = 1, ".cur.db", .cur.db)
      eval(c1)
      invisible()
      return()
      
    } else {
      if(ans == "true" || ans == "TRUE") {
        .cur.db@Prefs@Gam.prefs$disp <- TRUE
        c1<-call("assign",pos = 1, ".cur.db", .cur.db)
        eval(c1)
        invisible()
        return()
      } else {
        cat("Please enter NULL or TRUE ")
        Recall(first=FALSE)
      }
    }
  }
