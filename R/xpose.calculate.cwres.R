
#' @describeIn compute.cwres This function is a wrapper around
#' the function \code{compute.cwres}.  It computes the CWRES for the model file
#' associated with the Xpose data object input to the function.  If possible it
#' also computes the CWRES for any simulated data associated with the current
#' Xpose data object.  If you have problems with this function try using
#' \code{compute.cwres} and then rereading your dataset into Xpose.
#' 
#' @export
"xpose.calculate.cwres" <-
  function(object,
           cwres.table.prefix="cwtab",
           tab.suffix="",
           sim.suffix="sim",
           est.tab.suffix=".est",
           deriv.tab.suffix=".deriv",
           old.file.convention=FALSE,
           id="ALL",
           printToOutfile=TRUE,
           onlyNonZero=FALSE,
           classic=FALSE,
           ...) {
    
    
    cwres <- compute.cwres(run.number=object@Runno,
                           tab.prefix=cwres.table.prefix,
                           est.tab.suffix=est.tab.suffix,
                           deriv.tab.suffix=deriv.tab.suffix,
                           old.file.convention=old.file.convention,
                           id=id,
                           printToOutfile=printToOutfile,
                           onlyNonZero=onlyNonZero,
                           sim.suffix="",
                           ...)

    if (!is.null(cwres)){
      object@Data$CWRES=as.vector(cwres)
      object@Prefs@Xvardef$cwres="CWRES"    
    }
    
    
    ## simulation CWRES
    if(!is.null(object@Nsim)){
      if(object@Nsim==1){
        cwres.sim <- compute.cwres(run.number=object@Runno,
                                   tab.prefix=cwres.table.prefix,
                                   est.tab.suffix=est.tab.suffix,
                                   deriv.tab.suffix=deriv.tab.suffix,
                                   old.file.convention=old.file.convention,
                                   id=id,
                                   printToOutfile=printToOutfile,
                                   onlyNonZero=onlyNonZero,
                                   sim.suffix=sim.suffix,
                                   ...)

        if (!is.null(cwres.sim)){
          object@SData$CWRES=as.vector(cwres.sim)
          object@Prefs@Xvardef$cwres="CWRES"    
        } else {
          cat("Table file needed to compute CWRES not present\n")
          cat("For simulated data\n\n")
          cat("CWRES not calculated for simulated data\n\n")
          #cat("Simulated data will be dropped from xpose object.\n\n")
        }
      } else {
        cat("CWRES cannot be calculated for table files\n")
        cat("with multiple simulations in a single file\n\n")
        cat("CWRES not calculated for simulated data\n\n")
        #cat("Simulated data will be dropped from xpose object.\n\n")
      }
    }
    

        
    if (classic==TRUE) {
      #.cur.db <- object
      c1<-call("assign",pos = 1, ".cur.db",object)
      eval(c1)
    }
    
    invisible(object)

  }

