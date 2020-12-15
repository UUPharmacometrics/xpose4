
sqrtm <- function(x) {
  xe <- eigen(x)
  xe1 <- xe$values
  if(all(xe1 >= 0)) {
    xev1 <- diag(sqrt(xe1),nrow=length(xe1))
  } else {
    i=1
    while(i<(length(xe1)+1)){
      if(xe1[i]<0){
        xe1[i]=0
      }
      i=i+1
    }
    xev1 <- diag(sqrt(xe1),nrow=length(xe1))
  }
  xval1 <- cbind(xe$vectors)
  xval1i <- solve(xval1)
  y <- xval1 %*% xev1 %*% xval1i

  ## test with
  ##foo <- matrix(c(5,-4,1,0,0,-4,6,-4,1,0,1,-4,6,-4,1,0,1,-4,6,-4,0,0,1,-4,5),nrow=5,ncol=5)
  ##foo1 <- sqrtm(foo)
  ##foo1
  ##foo1%*%foo1
  ##

}

"is.cwres.readable.file"  <- function(filename )
{
  ## If we are not dealing with R -> Splus
  if(is.null(version$language)) {
    cat("This version of Xpose uses R")
    ## if(platform() == "WIN386") {
    ##   access(filename, 4) == 0
    ## } else {
    ##   filename <- paste("'", filename, "'", sep = "")
    ##   sapply(paste("test -f", filename, "-a -r", filename), unix,
    ##          output = F) == 0
    ## }
  } else {
    return(file.exists(filename)[1])
  }    
}


read.cwres.data <-
  function(filename,
           old.file.convention=FALSE,
           est.tab.suffix=".est",
           deriv.tab.suffix=".deriv",
           ...) {
    
    tables.read <- FALSE
    
    if(old.file.convention){
      if ((is.cwres.readable.file(paste(filename,".50",sep=""))) &&
          ##(is.cwres.readable.file(paste(filename,".51",sep=""))) &&
          (is.cwres.readable.file(paste(filename,".52",sep=""))) &&
          ##(is.cwres.readable.file(paste(filename,".53",sep=""))) &&
          (is.cwres.readable.file(paste(filename,".54",sep=""))) &&
          ##(is.cwres.readable.file(paste(filename,".55",sep=""))) &&
          (is.cwres.readable.file(paste(filename,".56",sep=""))) &&
          ##(is.cwres.readable.file(paste(filename,".57",sep=""))) &&
          (is.cwres.readable.file(filename))) {

        nsim <- 1
        
        num.fields <- count.fields(filename,skip=1)
        if((length(unique(num.fields))!=1) ||
           ## These lines fix problem if num_fields matches fields first header row 
           (unique(num.fields) == 8) ||
           (unique(num.fields) == 3)) { 
          
          tmp   <- readLines(filename, n = -1)
          inds  <- grep("TABLE",tmp)
          if (length(inds)!=1){
            cat("Multiple simulations not supported\n")
            cat("using this old file convention\n")
            return(NULL)
          } else {
            data <- read.table(filename,skip=1,header=T)
          }
        } else {
          data <- read.table(filename,skip=1,header=T)
        }
        size.of.sim <- dim(data)[1]/nsim
        data[,"iteration.number"] <- sort(rep(1:nsim,size.of.sim))

        eta <- vector("list",nsim)
        theta <- vector("list",nsim)
        omega <- vector("list",nsim)
        sigma <- vector("list",nsim)
                   
        ##data <- read.table(filename,skip=1,header=TRUE)
        eta[[1]] <- read.table(paste(filename,".50",sep=""))
        ##ofv <- read.table(paste(filename,".51",sep=""))
        theta[[1]] <- read.table(paste(filename,".52",sep=""))
        ##setheta <- read.table(paste(filename,".53",sep=""))
        omega[[1]] <- read.table(paste(filename,".54",sep=""))
        ##seomega <- read.table(paste(filename,".55",sep=""))
        sigma[[1]] <- read.table(paste(filename,".56",sep=""))
        ##sesigma <- read.table(paste(filename,".57",sep="")) 

        tables.read <- TRUE
      }
    } else { # new file convention
      est.file <- paste(filename,est.tab.suffix,sep="")
      deriv.file <- paste(filename,deriv.tab.suffix,sep="")
      if ((is.cwres.readable.file(est.file)) &&
          (is.cwres.readable.file(deriv.file))) {

        nsim <- 1
        
        #########################
        ## read derivatives table
        #########################
        
        ## Check if we have unequal number of fields in the derivative file
        ## used for multiple simulations
        num.fields <- count.fields(deriv.file,skip=1)
        if((length(unique(num.fields))!=1) ||
           ## These lines fix problem if num_fields matches fields first header row 
           (unique(num.fields) == 8) ||
           (unique(num.fields) == 3)) { 
          
          tmp   <- readLines(deriv.file, n = -1)
          inds  <- grep("TABLE",tmp)
          if (length(inds)!=1){
            inds  <- inds[c(2:length(inds))]
            inds2 <- inds+1
            tempfile<- paste(deriv.file,".xptmp",sep="")
            write.table(tmp[-c(inds,inds2)],file=tempfile,
                        row.names=FALSE,quote=FALSE)
            data <- read.table(tempfile,skip=2,header=T)
            unlink(tempfile)

            ## add iteration label to data
            nsim <- length(inds)+1       
          
          } else {
            data <- read.table(deriv.file,skip=1,header=T)
          }
        } else {
          data <- read.table(deriv.file,skip=1,header=T)
        }
        size.of.sim <- dim(data)[1]/nsim
        data[,"iteration.number"] <- sort(rep(1:nsim,size.of.sim))

        
        #########################
        ## read estimated parameters table
        #########################        
        filename.extra <- est.file
        data.extra <- scan(filename.extra,
                           sep = "\n", what = character(),
                           quiet=TRUE)
        eta.pat <- "^ *ETAS"
        theta.pat <- "^ *THETAS"
        omega.pat <- "^ *OMEGAS"
        sigma.pat <- "^ *SIGMAS"

        eta.pat.line <- grep(eta.pat, data.extra)
        theta.pat.line <- grep(theta.pat, data.extra)
        omega.pat.line <- grep(omega.pat, data.extra)
        sigma.pat.line <- grep(sigma.pat, data.extra)

        pat.lines <- list(eta.pat.line,
                           theta.pat.line,
                           omega.pat.line,
                           sigma.pat.line)
        pattern.lengths <- sapply(pat.lines,length)
        if(!all(pattern.lengths==nsim)){
          cat(paste("The",est.file,"and",deriv.file,
              "files do not match in size\n"))
          return(NULL)
        }

        eta <- vector("list",nsim)
        theta <- vector("list",nsim)
        omega <- vector("list",nsim)
        sigma <- vector("list",nsim)
        for(i in 1:nsim){
          tot.eta.rows <- theta.pat.line[i] - eta.pat.line[i] - 1
          tot.theta.rows <- omega.pat.line[i] - theta.pat.line[i] - 1
          tot.omega.rows <- sigma.pat.line[i] - omega.pat.line[i] - 1
          if(i==nsim){
            tot.sigma.rows <- length(data.extra) - sigma.pat.line[i]
          } else {
            tot.sigma.rows <- eta.pat.line[i+1] - sigma.pat.line[i] - 1
          }
          eta[[i]] <- read.table(filename.extra,skip=eta.pat.line[i],nrows=tot.eta.rows)
          theta[[i]] <- read.table(filename.extra,skip=theta.pat.line[i],nrows=tot.theta.rows)
          omega[[i]] <- read.table(filename.extra,skip=omega.pat.line[i],nrows=tot.omega.rows)
          sigma[[i]] <- read.table(filename.extra,skip=sigma.pat.line[i],nrows=tot.sigma.rows)
        }
        tables.read <- TRUE
      }
    } # end new file convention
    
    if (tables.read){
      
      setClass("nm.data",
                        slots=c(data = "data.frame"
                              ,eta  = "data.frame"
                              ##,ofv = "data.frame"
                              ,theta = "data.frame"
                              ##,setheta = "data.frame"
                              ,omega = "data.frame"
                              ##,seomega = "data.frame"
                              ,sigma = "data.frame"
                              ##,sesigma = "data.frame",
                              ),
               where = .GlobalEnv
               )

      all.data <- vector("list",nsim)
      for(i in 1:nsim){
        for (j in names(eta[[i]])){
          if (!is.numeric(eta[[i]][[j]])) {
            cat(paste("Some values of ETA in the NONMEM produced file",
                      filename.extra,
                      "are non-numeric\n"))
            cat("Attempting to fix the problem\n")
            ## add an 'E' where NONMEM has dropped one
            ## i.e. if the number is  -0.2404305E-105
            ## NONMEM writes  -0.2404305-105
            exp.pat <- "([0-9]+)([+-][0-9]+)"
            repl.exp.pat <- "\\1E\\2"
            #bad.locs <- grep(exp.pat,eta[[i]][[j]])
            new.vals <- gsub(exp.pat,repl.exp.pat,eta[[i]][[j]])
            eta[[i]][j] <- as.numeric(new.vals)
         }
        }
        
        all.data[[i]] <- new("nm.data"
                             ,data=data[data$iteration.number==i,]
                             ,eta=eta[[i]]
                             ##,ofv=ofv
                             ,theta=theta[[i]]
                             ##,setheta=setheta
                             ,omega=omega[[i]]
                             ##,seomega=seomega
                             ,sigma=sigma[[i]]
                             ##,sesigma=sesigma
                             )
      }
      
      if(is.null(data$MDV)){
        cat("Assuming all dataset lines contain a measurement\n")
        cat("No MDV item defined in dataset\n")
      }

      if(is.null(data$DV)){
        dv.found <- FALSE
        cat("No DV item defined in dataset\n")

        ## use this code to det the DV to the value at the position
        ## just beofre PRED RES and WRES
        ##
        ## doesn't work right now!
        ##
        #append.list <- c("PRED","RES","WRES")
        #data.names <- names(data)
        #tmp.length <- length(data.names)
        #if (all(data.names[(tmp.length-3):(tmp.length-1)]==append.list)){
        #  data[,"DV"]=data[[data.names[(tmp.length-4)]]]
        #  dv.found <- TRUE
        #  cat(paste("Using", data.names[(tmp.length-4)],"as the DV value\n"))
        #}
        
        if(!dv.found){
          all.data <- NULL
        }
      }
      
    } else { # no tables read      
      all.data <- NULL
      cat("Some or all of the required CWRES tables are\n", 
          "missing. Please see the online help for details\n", 
          "of what is required (?compute.cwres).\n")
    }
    return(all.data)
  }



ind.cwres <-
  function(ind.data,
           H.names,
           G.names,
           OMEGA,
           SIGMA,
           IND.ETAS,
           ...){
    
    if(is.null(ind.data$MDV)){
      ind.data1 <- ind.data
    } else {
      ind.data1 <- ind.data[ind.data$MDV==0,]
    }
    
    
    if (nrow(ind.data1)!=0) {
      
      ## for testing
      ##cat(paste("ID",ind.data1[1,"ID"],"\n"))
      
      ## create the epsilon linearization matrix H
      H.EPS = as.matrix(subset(ind.data1,select=H.names))
      
      ## create the eta linearization matrix G
      G.ETA = as.matrix(subset(ind.data1,select=G.names))
      
      ## covariance matrix
      ## this is only true for NON-L2 type data
      ## L2 data will have the first term non-diagonal
      TMP <- diag(H.EPS %*% SIGMA %*% t(H.EPS))
      IND.COV = diag(TMP,nrow=length(TMP)) +
        G.ETA %*% OMEGA %*% t(G.ETA)
      
      ## FOCE residuals
      #browser()
      if(any(grepl("^IPRED$",names(ind.data1)))) EXP.F <- as.matrix(ind.data1$IPRED) - G.ETA %*% IND.ETAS
      if(any(grepl("^IPRE$",names(ind.data1)))) EXP.F <- as.matrix(ind.data1$IPRE) - G.ETA %*% IND.ETAS
      #EXP.F <- as.matrix(ind.data1$IPRE) - G.ETA %*% IND.ETAS 
      FOCE.RES <- as.matrix(ind.data1$DV) - EXP.F
      
      
      ## CWRES
      SQRT.IND.COV <- sqrtm(IND.COV)
      IND.CWRES <-  solve(SQRT.IND.COV,FOCE.RES)
      
      ## add zero values back again
      if(is.null(ind.data$MDV)){
      } else {
        CWRES <- rep(0,length(ind.data[,1]))
        ind.data2 <- cbind(ind.data,CWRES)
        ind.data2[ind.data2$MDV==0,"CWRES"] <- IND.CWRES
        IND.CWRES <- as.matrix(ind.data2["CWRES"])
      }      
    }
    else {
      CWRES <- rep(0, length(ind.data[, 1]))
      ind.data2 <- cbind(ind.data, CWRES)
      IND.CWRES <- as.matrix(ind.data2["CWRES"])
    }
    return(IND.CWRES)
  }

      


#' Compute the Conditional Weighted Residuals
#' 
#' This function computes the conditional weighted residuals (CWRES) from a
#' NONMEM run.  CWRES are an extension of the weighted residuals (WRES), but
#' are calculated based on the first-order with conditional estimation (FOCE)
#' method of linearizing a pharmacometric model (WRES are calculated based on
#' the first-order (FO) method). The function requires a NONMEM table file and
#' an extra output file that must be explicitly asked for when running NONMEM,
#' see details below.
#' 
#' The function  reads in the following two
#' files:
#' 
#' \code{paste(tab.prefix,run.number,sim.suffix,est.tab.suffix,sep="")} 
#' 
#' \code{paste(tab.prefix,run.number,sim.suffix,deriv.tab.suffix,sep="")}
#' 
#' Which might be for example:
#' 
#' \preformatted{ cwtab1.est cwtab1.deriv }
#' 
#' and (depending on the input values to the function) returns the CWRES in
#' vector form as well as creating a new table file named:
#' 
#' \code{ paste(tab.prefix,run.number,sim.suffix,sep="") }
#' 
#' Which might be for example:
#' 
#' \preformatted{ cwtab1 } 
#' 
#' @aliases compute.cwres ind.cwres read.cwres.data is.cwres.readable.file
#' sqrtm xpose.calculate.cwres
#' @param run.number The run number of the NONMEM from which the CWRES are to
#' be calculated.
#' @param tab.prefix The prefix to two NONMEM file containing the needed values
#' for the computation of the CWRES, described in the details section.
#' @param sim.suffix The suffix ,before the ".", of the NONMEM file containing
#' the needed values for the computation of the CWRES, described in the details
#' section. For example, the table files might be named \code{cwtab1sim.est}
#' and \code{cwtab1sim.deriv}, in which case \code{sim.suffix="sim"}.
#' @param est.tab.suffix The suffix, after the ".", of the NONMEM file
#' containing the estimated parameter values needed for the CWRES calculation.
#' @param deriv.tab.suffix The suffix, after the ".", of the NONMEM file
#' containing the derivatives of the model with respect to the random
#' parameters needed for the CWRES calculation.
#' @param old.file.convention For backwards compatibility.  Use this if you are
#' using the previous file convention for CWRES (table files named cwtab1,
#' cwtab1.50, cwtab1.51, ... , cwtab.58 for example).
#' @param id Can be either "ALL" or a number matching an ID label in the
#' \code{datasetname}. Value is fixed to "ALL" for
#' \code{xpose.calculate.cwres}.
#' @param printToOutfile Logical (TRUE/FALSE) indicating whether the CWRES
#' values calculated should be appended to a copy of the \code{datasetname}.
#' Only works if \code{id}="ALL".  If chosen the resulting output file will be
#' \code{datasetname}.cwres.  Value is fixed to TRUE for
#' \code{xpose.calculate.cwres}.
#' @param onlyNonZero Logical (TRUE/FALSE) indicating if the return value (the
#' CWRES values) of \code{compute.cwres} should include the zero values
#' associated with non-measurement lines in a NONMEM data file.
#' @param object An xpose.data object.
#' @param cwres.table.prefix The prefix to the NONMEM table file containing the
#' derivative of the model with respect to the etas and epsilons, described in
#' the details section.
#' @param tab.suffix The suffix to the NONMEM table file containing the
#' derivative of the model with respect to the etas and epsilons, described in
#' the details section.
#' @param classic Indicates if the function is to be used in the classic menu
#' system.
#' @param \dots Other arguments passed to basic functions in code.
#' @return \describe{\item{compute.cwres}{Returns a vector containing the values of the
#' CWRES.} \item{xpose.calculate.cwres}{ Returns an Xpose data object that
#' contains the CWRES. If simulated data is present, then the CWRES will also
#' be calculated for that data.}}
#' 
#' 
#' @section Setting up the NONMEM model file: 
#' In order for this function to
#' calculate the CWRES, NONMEM must be run while requesting certain tables and
#' files to be created.  How these files are created differs depending on if
#' you are using $PRED or ADVAN as well as the version of NONMEM you are using.
#' These procedures are known to work for NONMEM VI but may be different for
#' NONMEM V and NONMEM VII.  We have attempted to indicate where NONMEM V may be different,
#' but this has not been extensively tested!  For NONMEM VII the CWRES are calculated internally
#' so this function is rarely needed.
#' 
#' This procedure can be done automatically using Perl Speaks NONMEM (PsN) and
#' we highly recommend using PsN for this purpose.  After installing PsN just
#' type '\code{execute [modelname] -cwres}'.  See
#' \url{http://psn.sourceforge.net} for more details.  
#' 
#' There are five main insertions needed in your NONMEM control file:
#' \enumerate{ \item$ABB COMRES=X.
#' 
#' Insert this line directly after your $DATA line.  The value of X is the
#' number of ETA() terms plus the number of EPS() terms in your model.  For
#' example for a model with three ETA() terms and two EPS() terms the code
#' would look like this:
#' 
#' \preformatted{$DATA temp.csv IGNORE=@ 
#' $ABB COMRES=5 
#' $INPUT ID TIME DV MDV AMT EVID 
#' $SUB ADVAN2 TRANS2} % preformatted % item
#' 
#' \item Verbatim code.
#' 
#' \itemize{ \item Using ADVAN.
#' 
#' If you are using ADVAN routines in your model, then Verbatim code should be
#' inserted directly after the $ERROR section of your model file.  The length
#' of the code depends again on the number of ETA() terms and EPS() terms in
#' your model.  For each ETA(y) in your model there is a corresponding term
#' G(y,1) that you must assign to a COM() variable.  For each EPS(y) in your
#' model, there is a corresponding HH(y,1) term that you must assign to a COM()
#' variable.
#' 
#' For example for a model using ADVAN routines with three ETA() terms and two
#' EPS() terms the code would look like this: 
#' 
#' \preformatted{
#' "LAST 
#' "  COM(1)=G(1,1) 
#' "  COM(2)=G(2,1) 
#' "  COM(3)=G(3,1) 
#' "  COM(4)=HH(1,1) 
#' "  COM(5)=HH(2,1) 
#' } % preformatted 
#' 
#' 
#' \item Using PRED.
#' 
#' If you are using $PRED, the verbatim code should be inserted directly after
#' the $PRED section of your model file.  For each ETA(y) in your model there
#' is a corresponding term G(y,1) that you must assign to a COM() variable.
#' For each EPS(y) in your model, there is a corresponding H(y,1) term that you
#' must assign to a COM() variable. The code would look like this for three
#' ETA() terms and two EPS() terms:
#' 
#' \preformatted{
#' "LAST 
#' "  COM(1)=G(1,1) 
#' "  COM(2)=G(2,1) 
#' "  COM(3)=G(3,1) 
#' "  COM(4)=H(1,1) 
#' "  COM(5)=H(2,1) 
#' } % preformatted 
#' 
#' } % itemize
#' 
#' 
#' \item INFN routine.
#' 
#' \itemize{ \item Using ADVAN with NONMEM VI and higher.
#' 
#' If you are using ADVAN routines in your model, then an $INFN section should
#' be placed directly after the $PK section using the following code.  In this
#' example we are assuming that the model file is named something like
#' 'run1.mod', thus the prefix to these file names 'cwtab' has the same run
#' number attached to it (i.e. 'cwtab1').  This should be changed for each new
#' run number.  
#' 
#' \preformatted{$INFN
#' IF (ICALL.EQ.3) THEN
#'   OPEN(50,FILE='cwtab1.est')
#'   WRITE(50,*) 'ETAS'
#'   DO WHILE(DATA)                                                       
#'     IF (NEWIND.LE.1) WRITE (50,*) ETA                                    
#'   ENDDO                                                                
#'   WRITE(50,*) 'THETAS'
#'   WRITE(50,*) THETA
#'   WRITE(50,*) 'OMEGAS'
#'   WRITE(50,*) OMEGA(BLOCK)
#'   WRITE(50,*) 'SIGMAS'
#'   WRITE(50,*) SIGMA(BLOCK)
#' ENDIF
#' } % preformatted
#' 
#' 
#' \item Using ADVAN with NONMEM V.
#' 
#' If you are using ADVAN routines in your model, then you need to use an INFN
#' subroutine.  If we call the INFN subroutine 'myinfn.for' then the $SUBS line
#' of your model file should include the INFN option.  That is, if we are using
#' ADVAN2 and TRANS2 in our model file then the $SUBS line would look like:
#' 
#' \preformatted{$SUB ADVAN2 TRANS2 INFN=myinfn.for} % preformatted
#' 
#' The 'myinfn.for' routine for 4 thetas, 3 etas and 1 epsilon is shown below.
#' If your model has different numbers of thetas, etas and epsilons then the
#' values of NTH, NETA, and NEPS, should be changed respectively.  These vales
#' are found in the DATA statement of the subroutine.  additionally, in this
#' example we are assuming that the model file is named something like
#' 'run1.mod', thus the prefix to the output file names ('cwtab') in this
#' subroutine has the same run number attached to it (i.e. 'cwtab1').  This
#' number should be changed for each new run number (see the line beginning
#' with 'OPEN').  
#' %Please note that the 4th and 5th lines of code should be one
#' %line with the '...' removed from each line, reading: \code{COMMON /ROCM6/
#' %THETAF(40),OMEGAF(30,30),SIGMAF(30,30)}.
#' 
#' 
#' \preformatted{
#'      SUBROUTINE INFN(ICALL,THETA,DATREC,INDXS,NEWIND)
#'      DIMENSION THETA(*),DATREC(*),INDXS(*)
#'      DOUBLE PRECISION THETA
#'      COMMON /ROCM6/ THETAF(40),OMEGAF(30,30),SIGMAF(30,30)
#'      COMMON /ROCM7/ SETH(40),SEOM(30,30),SESIG(30,30)
#'      COMMON /ROCM8/ OBJECT
#'      COMMON /ROCM9/ IERE,IERC
#'      DOUBLE PRECISION THETAF, OMEGAF, SIGMAF
#'      DOUBLE PRECISION OBJECT
#'      REAL SETH,SEOM,SESIG
#'      DOUBLE PRECISION ETA(10)
#'      INTEGER J,I
#'      INTEGER IERE,IERC
#'      INTEGER MODE
#'      INTEGER NTH,NETA,NEPS
#'      DATA NTH,NETA,NEPS/4,3,1/
#'      IF (ICALL.EQ.0) THEN
#' C      open files here, if necessary
#'        OPEN(50,FILE='cwtab1.est')
#'      ENDIF
#'      IF (ICALL.EQ.3) THEN
#'        MODE=0
#'        CALL PASS(MODE)
#'        MODE=1
#'        WRITE(50,*) 'ETAS'
#' 20     CALL PASS(MODE)
#'        IF (MODE.EQ.0) GO TO 30
#'        IF (NEWIND.NE.2) THEN
#'          CALL GETETA(ETA)
#'          WRITE (50,97) (ETA(I),I=1,NETA)
#'        ENDIF
#'        GO TO 20
#' 30     CONTINUE
#'        WRITE (50,*) 'THETAS'
#'        WRITE (50,99) (THETAF(J),J=1,NTH)
#'        WRITE(50,*) 'OMEGAS'
#'        DO 7000 I=1,NETA
#' 7000     WRITE (50,99) (OMEGAF(I,J),J=1,NETA)
#'        WRITE(50,*) 'SIGMAS'
#'        DO 7999 I=1,NEPS
#' 7999     WRITE (50,99) (SIGMAF(I,J),J=1,NEPS)
#'      ENDIF
#' 99   FORMAT (20E15.7)
#' 98   FORMAT (2I8)
#' 97   FORMAT (10E15.7)
#'      RETURN
#'      END
#' } % preformatted
#' 
#' \item Using $PRED with NONMEM VI and higher.
#' 
#' If you are using $PRED, then an the following code should be placed at the
#' end of the $PRED section of the model file (together with the verbatim
#' code).  In this example we are assuming that the model file is named
#' something like 'run1.mod', thus the prefix to these file names 'cwtab' has
#' the same run number attached to it (i.e. 'cwtab1').  This should be changed
#' for each new run number.
#' 
#' \preformatted{IF (ICALL.EQ.3) THEN
#'   OPEN(50,FILE='cwtab1.est')
#'   WRITE(50,*) 'ETAS'
#'   DO WHILE(DATA)                                                       
#'     IF (NEWIND.LE.1) WRITE (50,*) ETA                                    
#'   ENDDO                                                                
#'   WRITE(50,*) 'THETAS'
#'   WRITE(50,*) THETA
#'   WRITE(50,*) 'OMEGAS'
#'   WRITE(50,*) OMEGA(BLOCK)
#'   WRITE(50,*) 'SIGMAS'
#'   WRITE(50,*) SIGMA(BLOCK)
#' ENDIF
#' } % preformatted
#' 
#' \item Using $PRED with NONMEM V.
#' 
#' If you are using $PRED with NONMEM V, then you need to add verbatim code
#' immediately after the $PRED command.  In this example we assume 4 thetas, 3
#' etas and 1 epsilon.  If your model has different numbers of thetas, etas and
#' epsilons then the values of NTH, NETA, and NEPS, should be changed
#' respectively.  These vales are found in the DATA statement below.  
#' %Please
#' %note that the 3rd and 4th lines of code should be one line with the '...'
#' %removed from each line, reading: \code{\" COMMON /ROCM6/
#' %THETAF(40),OMEGAF(30,30),SIGMAF(30,30) }.
#' 
#' \preformatted{
#' $PRED
#' "FIRST  %"
#' "     COMMON /ROCM6/ THETAF(40),OMEGAF(30,30),SIGMAF(30,30) 
#' "     COMMON /ROCM7/ SETH(40),SEOM(30,30),SESIG(30,30) 
#' "     COMMON /ROCM8/ OBJECT 
#' "     DOUBLE PRECISION THETAF, OMEGAF, SIGMAF 
#' "     DOUBLE PRECISION OBJECT 
#' "     REAL SETH,SEOM,SESIG 
#' "     INTEGER J,I 
#' "     INTEGER MODE 
#' "     INTEGER NTH,NETA,NEPS 
#' "     DATA NTH,NETA,NEPS/4,3,1/ 
#' } % preformatted

#' After this verbatim code you add all of the abbreviated code needed for the
#' $PRED routine in your model file.  After the abbreviated code more verbatim
#' code is needed.  This verbatim code should be added before the verbatim code
#' discussed above under point 2.  In the example below we are assuming that
#' the model file is named something like 'run1.mod', thus the prefix to the
#' output file names ('cwtab') has the same run number attached to it (i.e.
#' 'cwtab1').  This number should be changed for each new run number (see the
#' line beginning with 'OPEN').
#' 
#' \preformatted{
#' "     IF (ICALL.EQ.0) THEN 
#' "C    open files here, if necessary 
#' "       OPEN(50,FILE='cwtab1.est') 
#' "     ENDIF 
#' "     IF (ICALL.EQ.3) THEN 
#' "       MODE=0 
#' "       CALL PASS(MODE) 
#' "       MODE=1 
#' " 	     WRITE(50,*) 'ETAS' 
#' "20     CALL PASS(MODE) 
#' "       IF (MODE.EQ.0) GO TO 30 
#' "       IF (NEWIND.NE.2) THEN 
#' "         CALL GETETA(ETA) 
#' "         WRITE (50,97) (ETA(I),I=1,NETA) 
#' "       ENDIF 
#' "       GO TO 20 
#' "30     CONTINUE 
#' "       WRITE (50,*) 'THETAS' 
#' "       WRITE (50,99) (THETAF(J),J=1,NTH) 
#' "       WRITE (50,*) 'OMEGAS' 
#' "       DO 7000 I=1,NETA 
#' "7000     WRITE (50,99) (OMEGAF(I,J),J=1,NETA) 
#' "       WRITE (50,*) 'SIGMAS' 
#' "       DO 7999 I=1,NEPS 
#' "7999     WRITE (50,99) (SIGMAF(I,J),J=1,NEPS) 
#' "     ENDIF 
#' "99   FORMAT (20E15.7) 
#' "98   FORMAT (2I8) 
#' "97   FORMAT (10E15.7) 
#' } % preformatted
#' 
#' } % itemize % item (infn)
#' 
#' \item cwtab*.deriv table file.
#' 
#' A special table file needs to be created to print out the values contained
#' in the \code{COMRES} variables.  In addition the \code{ID, IPRED, MDV, DV,
#' PRED and RES} data items are needed for the computation of the CWRES.  The
#' following code should be added to the NONMEM model file.  In this example we
#' continue to assume that we are using a model with three ETA() terms and two
#' EPS() terms, extra terms should be added for new ETA() and EPS() terms in
#' the model file.  We also assume the model file is named something like
#' 'run1.mod', thus the prefix to these file names 'cwtab' has the same run
#' number attached to it (i.e. 'cwtab1').  This should be changed for each new
#' run number.
#' 
#' \preformatted{
#' $TABLE ID COM(1)=G11 COM(2)=G21 COM(3)=G31 COM(4)=H11 COM(5)=H21 
#'        IPRED MDV NOPRINT ONEHEADER FILE=cwtab1.deriv 
#' } % preformatted
#' 
#' 
#' \item $ESTIMATION.
#' 
#' To compute the CWRES, the NONMEM model file must use (at least) the FO
#' method with the \code{POSTHOC} step.  If the FO method is used and the
#' \code{POSTHOC} step is not included then the CWRES values will be equivalent
#' to the WRES.  The CWRES calculations are based on the FOCE approximation,
#' and consequently give an idea of the ability of the FOCE method to fit the
#' model to the data. If you are using another method of parameter estimation
#' (e.g. FOCE with interaction), the CWRES will not be calculated based on the
#' same model linearization procedure.  
#' 
#' 
#' 
#' 
#' } % Enumerate
#' @author Andrew Hooker
#' @references Hooker AC, Staatz CE, Karlsson MO. \emph{Conditional weighted
#' residuals, an improved model diagnostic for the FO/FOCE methods}. PAGE 15
#' (2006) Abstr 1001 [\url{http://www.page-meeting.org/?abstract=1001}].
#' 
#' Hooker AC, Staatz CE and Karlsson MO, Conditional weighted residuals (CWRES): 
#' a model diagnostic for the FOCE method, Pharm Res, 24(12): p. 2187-97, 2007,
#' [\url{https://doi.org/10.1007/s11095-007-9361-x}].
#' 
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## Capture CWRES from cwtab5.est and cwtab5.deriv
#' cwres <- compute.cwres(5)
#' mean(cwres)
#' var(cwres)
#' 
#' ## Capture CWRES from cwtab1.est and cwtab1.deriv, do not print out, allow zeroes
#' cwres <- compute.cwres("1", printToOutFile = FALSE,
#'   onlyNonZero = FALSE)
#' 
#' ## Capture CWRES for ID==1
#' cwres.1 <- compute.cwres("1", id=1)
#' 
#' ## xpdb5 is an Xpose data object
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory
#' xpdb5 <- xpose.data(5)
#' 
#' ## Compare WRES, CWRES
#' xpdb5 <- xpose.calculate.cwres(xpdb5)
#' cwres.wres.vs.idv(xpdb5) 
#' 
#' }
#' 
#' @export 
#' @family data functions 

compute.cwres <-
  function(run.number,
           tab.prefix="cwtab",
           sim.suffix="",
           est.tab.suffix=".est",
           deriv.tab.suffix=".deriv",
           old.file.convention=FALSE,
           id="ALL",
           printToOutfile=TRUE,
           onlyNonZero=TRUE,
           ...){


    out.file = paste(tab.prefix,run.number,sim.suffix,sep="")

    full.dataset <- read.cwres.data(out.file,
                                    old.file.convention=old.file.convention,
                                    est.tab.suffix=est.tab.suffix,
                                    deriv.tab.suffix=deriv.tab.suffix,
                                    ...)
    
    if (is.null(full.dataset)) {
      return()
    }

    num.reps <- length(full.dataset)
    tot.cwres <- c()
    for(rep in 1:num.reps){

      ## get one of the repetitions
      dataset <- full.dataset[[rep]]

      ##    dataset <- read.cwres.data("sdtab1") # only for testing
      first.only.data <- dataset@data[!duplicated(dataset@data$ID),]
      all.etas <- dataset@eta
      all.etas <- cbind(first.only.data["ID"], all.etas)
      

      ## create OMEGA, SIGMA  matricies
      OMEGA <- as.matrix(dataset@omega)
      SIGMA <- as.matrix(dataset@sigma)
      
      ## create the names of the H and G columns in the dataset
      H.names = c()
      i=1
      while(i<(length(dataset@sigma)+1)){
        H.names = c(H.names,paste("H",i,"1",sep=""))
        i=i+1
      }
      
      G.names = c()
      i=1
      while(i<(length(dataset@omega)+1)){
        G.names = c(G.names,paste("G",i,"1",sep=""))
        i=i+1
      }
      

      if(id=="ALL"){
        

        id.vals <- unique(dataset@data$ID)
        CWRES <- c()
        for(i in id.vals){
          #browser()
          #ind.data <- subset(dataset@data,ID==i)
          ind.data <- dataset@data[dataset@data$ID==i,]
          ind.etas <- t(as.matrix(all.etas[all.etas$ID==i,colnames(all.etas)!="ID"]))
          CWRESI <- ind.cwres(ind.data,
                              H.names,
                              G.names,
                              OMEGA,
                              SIGMA,
                              ind.etas,
                              ...)
          CWRES <- c(CWRES,CWRESI)
        }
        CWRES <- as.matrix(CWRES)
        
        
        
        if(printToOutfile==TRUE){
          
          ## set up out file name
          if(old.file.convention){
            filename <- paste(out.file,".cwres",sep="")
          } else {
            filename <- out.file
          }
          ## bind CWRES to data file
                                        #data.cwres <- cbind(dataset@data,CWRES)
          data.cwres <- data.frame("ID"=dataset@data$ID)
          if(!is.null(dataset@data$MDV)) data.cwres$MDV=dataset@data$MDV
          if(!is.null(dataset@data$DV)) data.cwres$DV=dataset@data$DV
          if(any(grepl("^IPRE$",names(dataset@data)))){
            if(!is.null(dataset@data$IPRE)) data.cwres$IPRE=dataset@data$IPRE
          }
          if(any(grepl("^IPRED$",names(dataset@data)))){
            if(!is.null(dataset@data$IPRED)) data.cwres$IPRED=dataset@data$IPRED
          }
          if(!is.null(dataset@data$WRES)) data.cwres$WRES=dataset@data$WRES
          if(!is.null(CWRES)) data.cwres$CWRES=CWRES
          
          ## data.cwres <- data.frame("ID"=dataset@data$ID,
          ##                                  "DV"=dataset@data$DV,
          ##                                  "MDV"=dataset@data$MDV,
          ##                                  "IPRE"=dataset@data$IPRE,
          ##                                  "WRES"=dataset@data$WRES,
          ##                                  "CWRES"=CWRES)

          ## print out dataset
          tmp <- installed.packages(priority="NA")
          if (length(grep("xpose4",tmp))>0){
            xpose.version <- tmp["xpose4","Version"]
            xpose.text <- paste("from Xpose version",xpose.version,sep=" ")
          } else {
            xpose.text <- ""
          }
          if (rep==1) {
            append.table.message <- FALSE
          } else {
            append.table.message <- TRUE
          }
          cat(paste("TABLE for CWRES computed using compute.cwres.R",xpose.text,"on",
                    format(Sys.time(), "%a %b %d, %X, %Y"),"\n"),
              file=filename,
              append=append.table.message)
          newdata <- format(data.cwres,sci=TRUE)
          suppressWarnings(
                           write.table(newdata,filename,row.names=FALSE,sep=" ",
                                       quote=FALSE,append=TRUE)
                           )
        } # end print to out file

        if(onlyNonZero==TRUE){
          if(is.null(dataset@data$MDV)){
          } else {
            ## bind the data to the data file
            data.cwres <- cbind(dataset@data,CWRES)
            tmp <- data.cwres[data.cwres$MDV==0,]
            #tmp <- subset(data.cwres,MDV==0)
            CWRES <- tmp$CWRES
          }
        }
        
      } else { ## end if ID==ALL
        data1 <- dataset@data[dataset@data$ID==id,]
        ind.etas <- t(as.matrix(all.etas[all.etas$ID==id,colnames(all.etas)!="ID"]))
        CWRES <- ind.cwres(data1,
                           H.names,
                           G.names,
                           OMEGA,
                           SIGMA,
                           ind.etas,
                           ...)

        if(onlyNonZero==TRUE){
          if(is.null(data1$MDV)){
          } else {
            ## bind the data to the data file
            data1.cwres <- cbind(data1,CWRES)
            tmp <- data1.cwres[data1.cwres$MDV==0,]
            #tmp <- subset(data1.cwres,MDV==0)
            CWRES <- tmp$CWRES
          }
        }
      } # end if ID=id
     
      tot.cwres <- c(tot.cwres,CWRES)
    }# end num.reps 
    
    return(tot.cwres)

  }



