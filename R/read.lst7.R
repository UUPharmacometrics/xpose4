
read.lst7 <- function(filename) {

  ## Function to split character strings
  string2num <- function(x)
    {
      oldopts <- options(warn = -1)
      on.exit(options(oldopts))
      nc <- nchar(x)
      tmp <- substring(x, 1:nc, 1:nc)
      spc <- tmp == " "
      st <- !spc & c(T, spc[ - nc])
      end <- !spc & c(spc[-1], T)
      as.numeric(substring(x, (1:nc)[st], (1:nc)[end]))
    }

  listfile <- scan(filename, sep = "\n", what = character(),quiet=TRUE)

  ## Find termination messages
  minimStart <- grep("#TERM:",listfile)
  minimEnd   <- grep("#TERE:",listfile)

  if(length(minimStart)>1) {
    stop("There seems to be more than one set of termination messages. This is not supported in the current version of Xpose.\n")
  } else if(length(minimStart)==0 || length(minimEnd) == 0) {
    termes <- NULL
  } else {
    termes <- listfile[(minimStart+1):(minimEnd-1)]
    termes <- substring(termes, 2)
  }


  ## Figure out the name of the raw results file
  rawfile <- paste(sub("\\.\\w*$",'',filename),".ext",sep="")
  if(!is.readable.file(rawfile)) {
    stop(paste("Could not find the raw results file (",rawfile,") for list file: ",filename,"\n"))
  } else {
    rawres <- read.table(rawfile,skip=1,header=T)
  }

  ## Extract OFV
  ofv <- rawres$OBJ[rawres$ITERATION==-1000000000]

  ## Extract parameter estimates

  ## Get lines with relevant info
  finalEstLine <- as.numeric(rawres[rawres$ITERATION==-1000000000,])

  ## Extract theta estimates
  thetas       <- finalEstLine[grep("THETA",names(rawres))]

  ## Extract omega estimates
  omindx <- grep("OMEGA",names(rawres))
  omega  <- list()
  seenOM <- NULL
  seenOM[1:100] <- 0

  for(om in omindx) {
    omnam <- names(rawres)[om]
    omcol <- as.numeric(sub("OMEGA\\.\\w*\\.",'',omnam,perl=TRUE))
    tmp1  <- sub("OMEGA\\.",'',omnam,perl=TRUE)
    omrow <- as.numeric(sub("\\.\\w*\\.$",'',tmp1,perl=TRUE))

    seenOM[omrow] <- seenOM[omrow]+1
    if(seenOM[omrow]==1) {
      omega[omrow][omcol] <- finalEstLine[om]
    } else {
      omega[[omrow]] <- c(omega[[omrow]],as.numeric(finalEstLine[om]))

    }
  }

  ## Extract sigma estimates
  siindx <- grep("SIGMA",names(rawres))
  sigma  <- list()
  seenSI <- NULL
  seenSI[1:100] <- 0

  if(length(siindx)!=0) {

    for(si in siindx) {
      sinam <- names(rawres)[si]
      sicol <- as.numeric(sub("SIGMA\\.\\w*\\.",'',sinam,perl=TRUE))
      tmp1  <- sub("SIGMA\\.",'',sinam,perl=TRUE)
      sirow <- as.numeric(sub("\\.\\w*\\.$",'',tmp1,perl=TRUE))

      seenSI[sirow] <- seenSI[sirow]+1
      if(seenSI[sirow]==1) {
        sigma[sirow][sicol] <- finalEstLine[si]
      } else {
        sigma[[sirow]] <- c(sigma[[sirow]],as.numeric(finalEstLine[si]))

      }
    }
  } else {
    sigma <- NULL
  }

  ## Extract standard error estimates

  ## Extract line with relevant info
  seEstLine <- as.numeric(rawres[rawres$ITERATION==-1000000001,])

  if(length(seEstLine)[1] == 0) {
    sethetas <- NULL
    seomega <- NULL
    sesigma <- NULL
  } else {

    ## Extract theta estimates
    sethetas       <- seEstLine[grep("THETA",names(rawres))]
    sethetas[sethetas == 1.00000E+10] <- NA
    
    ## Extract omega estimates
    omindx <- grep("OMEGA",names(rawres))
    seomega  <- list()
    seeseOM <- NULL
    seeseOM[1:100] <- 0

    for(om in omindx) {
      omnam <- names(rawres)[om]
      omcol <- as.numeric(sub("OMEGA\\.\\w*\\.",'',omnam,perl=TRUE))
      tmp1  <- sub("OMEGA\\.",'',omnam,perl=TRUE)
      omrow <- as.numeric(sub("\\.\\w*\\.$",'',tmp1,perl=TRUE))

      if(!is.na(seEstLine[om])){
          if(seEstLine[om] == 1.00000E+10) seEstLine[om] <- NA
      }
      seeseOM[omrow] <- seeseOM[omrow]+1
      if(seeseOM[omrow]==1) {
        seomega[omrow][omcol] <- seEstLine[om]
      } else {
        seomega[[omrow]] <- c(seomega[[omrow]],as.numeric(seEstLine[om]))

      }
    }

    ## Extract sigma estimates
    siindx <- grep("SIGMA",names(rawres))
    sesigma  <- list()
    seenseSI <- NULL
    seenseSI[1:100] <- 0

    if(length(siindx)!=0) {

      for(si in siindx) {
        sinam <- names(rawres)[si]
        sicol <- as.numeric(sub("SIGMA\\.\\w*\\.",'',sinam,perl=TRUE))
        tmp1  <- sub("SIGMA\\.",'',sinam,perl=TRUE)
        sirow <- as.numeric(sub("\\.\\w*\\.$",'',tmp1,perl=TRUE))

        if(!is.na(seEstLine[si])){
            if(seEstLine[si] == 1.00000E+10) seEstLine[si] <- NA
        }
        seenseSI[sirow] <- seenseSI[sirow]+1
        if(seenseSI[sirow]==1) {
          sesigma[sirow][sicol] <- seEstLine[si]
        } else {
          sesigma[[sirow]] <- c(sesigma[[sirow]],as.numeric(seEstLine[si]))

        }
      }
    } else {
      sesigma <- NULL
    }

  }


  ret.list <- list(term = termes, ofv = ofv, thetas = thetas, omega =
                   omega, sigma = sigma, sethetas = sethetas,
                   seomegas = seomega,
                   sesigmas = sesigma)
  return(ret.list)
}
