#' Create an Xpose data object
#' 
#' Creates an \code{xpose.data} object.
#' 
#' Xpose expects, by default, to find at least one the the following NONMEM 
#' tables in the working directory to be able to create an Xpose data object 
#' (using a run number of '5' as an example):
#' 
#' sdtab5: The 'standard' parameters, including IWRE, IPRE, TIME, and the NONMEM
#' default items (DV, PRED, RES and WRES) that are added when NOAPPEND is not 
#' present in the \code{$TABLE} record.
#' 
#' \code{$TABLE ID TIME IPRE IWRE NOPRINT ONEHEADER FILE=sdtab5}
#' 
#' patab5: The empirical Bayes estimates of individual model parameter values, 
#' or posthoc estimates. These are model parameters, such as CL, V2, ETA1, etc.
#' 
#' \code{$TABLE ID CL V2 KA K F1 ETA1 ETA2 ETA3 NOPRINT NOAPPEND ONEHEADER 
#' FILE=patab5 }
#' 
#' catab5: Categorical covariates, e.g. SEX, RACE.
#' 
#' \code{$TABLE ID SEX HIV GRP NOPRINT NOAPPEND ONEHEADER FILE=catab5 }
#' 
#' cotab5: Continuous covariates, e.g. WT, AGE.
#' 
#' \code{$TABLE ID WT AGE BSA HT GGT HB NOPRINT NOAPPEND ONEHEADER FILE=cotab5}
#' 
#' mutab5, mytab5, extra5, xptab5: Additional variables of any kind. These might
#' be useful if there are more covariates than can be accommodated in the 
#' covariates tables, for example, or if you have other variables that should be
#' added, e.g. CMAX, AUC.
#' 
#' The default names for table files can be changed by changing the default 
#' values to the function.  The files that Xpose looks for by default are:
#' 
#' \code{ paste(table.names, runno, tab.suffix, sep="") }
#' 
#' The default CWRES table file name is called:
#' 
#' \code{paste(cwres.name,runno,cwres.suffix,tab.suffix,sep="")}
#' 
#' If there are simulation files present then Xpose looks for the files to be 
#' named:
#' 
#' \code{paste(table.names, runno, sim.suffix, tab.suffix, sep="")} 
#' \code{paste(cwres.name,runno,sim.suffix,cwres.suffix,tab.suffix,sep="") }
#' 
#' 
#' This is basically a wrapper function for the \code{read.nm.tables}, 
#' \code{Data} and \code{SData} functions. See them for further information.
#' 
#' Also reads in the .phi file associated with the run (Individual OFVs, 
#' parameters, and variances of those parameters.)
#' 
#' @param runno Run number of the table files to read.
#' @param tab.suffix Suffix to be appended to the table file names for the 
#'   "real" data.
#' @param sim.suffix Suffix to be appended to the table file names for any 
#'   simulated data.
#' @param cwres.suffix Suffix to be appended to the table file names for any 
#'   CWRES data.
#' @param directory Where the files are located.
#' @param quiet A logical value indicating if more diagnostic messages should be
#'   printed when running this function.
#' @param table.names Default text that Xpose looks for when searching for table
#'   files.
#' @param cwres.name default text that xpose looks for when searching for CWRES 
#'   table files.
#' @param mod.prefix Start of model file name.
#' @param mod.suffix End of model file name.
#' @param phi.suffix End of .phi file name.
#' @param phi.file The name of the .phi file. If not \code{NULL} then supersedes
#'   \code{paste(mod.prefix,runno,phi.suffix,sep="")}.
#' @param nm7 \code{T/F} if table files are for NONMEM 7/6, NULL for undefined.
#' @param \dots Extra arguments passed to function.
#' @return An \code{xpose.data} object.  Default values for this object are 
#'   created from a file called 'xpose.ini'.  This file can be found in the root
#'   directory of the 'xpose4' package: 
#'   
#'   \code{system.file("xpose.ini",package="xpose4")}.  
#'   
#'   
#'   It can be modified to fit the users 
#'   wants and placed in the home folder of the user or the working directory, 
#'   to override default settings.
#' @author Niclas Jonsson, Andrew Hooker
#' @seealso \code{\link{xpose.data-class}}, \code{\link{Data}}, 
#'   \code{\link{SData}}, \code{\link{read.nm.tables}}, 
#'   \code{\link{compute.cwres}}
#' @keywords methods
#' @examples
#' # Here we create files from an example NONMEM run
#'  
#' od = setwd(tempdir()) # move to a temp directory
#' (cur.files <- dir()) # current files in temp directory
#' 
#' simprazExample(overwrite=TRUE) # write files
#' (new.files <- dir()[!(dir() %in% cur.files)])  # what files are new here?
#' 
#' xpdb <- xpose.data(1)
#' 
#' 
#' file.remove(new.files) # remove these files
#' setwd(od)  # restore working directory
#' 
#' 
#' \dontrun{
#' 
#' # We expect to find the required NONMEM run and table files for run
#' # 5 in the current working directory, and that the table files have
#' # a suffix of '.dat', e.g. sdtab5.dat
#' xpdb5 <- xpose.data(5, tab.suffix = ".dat") 
#' }
#' 
#' @export xpose.data
#' @family data functions 

xpose.data <-function(runno,
                      tab.suffix="",
                      sim.suffix="sim",
                      cwres.suffix="",
                      directory=".",
                      quiet=TRUE,
                      table.names=c("sdtab","mutab","patab","catab",
                        "cotab","mytab","extra","xptab","cwtab"),
                      cwres.name=c("cwtab"),
                      mod.prefix="run",
                      mod.suffix=".mod",
                      phi.suffix=".phi",
                      phi.file=NULL,
                      ##vpc.name="vpctab",
                      nm7=NULL,  # T/F if table files are for NM7, NULL for undefined
                      ...) {


  ##options(warn=-1) # suppress warnings

  ## make table lists
  match.pos <- match(cwres.name,table.names)
  if (!is.na(match.pos)) table.names <- table.names[-match.pos]

  ## Create the table file names to process
  myfun <- function(x,directory,runno,cwres.suffix,sim.suffix,tab.suffix) {
    filename <- paste0(x,runno,cwres.suffix,sim.suffix,tab.suffix)
    file.path(directory, filename)
  }

  tab.files   <- sapply(table.names,myfun,directory,runno,cwres.suffix="",sim.suffix="",tab.suffix)

  cwres.files <- sapply(cwres.name,myfun,directory,runno,cwres.suffix,sim.suffix="",tab.suffix)

  sim.files   <- sapply(table.names,myfun,directory,runno,cwres.suffix="",sim.suffix,tab.suffix)

  cwres.sim.files <- sapply(cwres.name,myfun,directory,runno,cwres.suffix,sim.suffix,tab.suffix)

  tab.files <- c(tab.files,cwres.files)
  sim.files <- c(sim.files,cwres.sim.files)

  ## Read the table files.
  cat("\nLooking for NONMEM table files.\n")
  tmp <- read.nm.tables(table.files=tab.files,
                        quiet=quiet,...)

  ## Fail if we can't find any.
  if(is.null(tmp)) {
    cat("Table files not read!\n")
    return(NULL)
  }

  ## check if NM.version is > 6
  if(is.null(nm7)){
    if(any(!is.na(match(c("IPRED","IWRES"),names(tmp))))){
      nm7 <- T
    } else {
      nm7 <- F
    }
  }


  ## check that classes are present
  if (!isClass("xpose.data") || !isClass("xpose.prefs")) {
    createXposeClasses(nm7=nm7)
  }
  #createXposeClasses(nm7=nm7)

  ## Create the object
  xpobj       <- new("xpose.data",
                     Runno=runno,
                     Doc=NULL,
                     Data = NULL #read.nm.tables(runno,tab.suffix=tab.suffix,
                       #quiet=TRUE)
                     )
  if(!nm7) xvardef(xpobj) <- c("iwres","IWRE")
  if(!nm7) xvardef(xpobj) <- c("ipred","IPRE")

  ## read local options
  if (is.readable.file("xpose.ini")) { ## read options in current directory
    xpobj <- xpose.read(xpobj, file="xpose.ini")
  } else if (is.readable.file(file.path(path.expand("~"),"xpose.ini"))) {     ## read local options
    xpobj <- xpose.read(xpobj, file=file.path(path.expand("~"),"xpose.ini"))
  } else if (is.readable.file(system.file("xpose.ini",package="xpose4"))) {     ## read global options
    xpobj <- xpose.read(xpobj, file=system.file("xpose.ini",package="xpose4"))
  } else{
    cat("Cannot find a valid xpose.ini file!\n")
  }
  
  #cat("test\n")

  ## read tmp data into xpose object
  Data(xpobj) <- tmp
  cat("Table files read.\n")


  ## read phi file
  ind.data <- NULL
  nsim.phi <- NULL
  if(nm7){
    phi.data <- read.phi(phi.file=phi.file,
                         phi.prefix=mod.prefix,
                         runno=runno,
                         phi.suffix=phi.suffix,
                         ##sim.suffix="sim",
                         quiet=quiet,
                         nm7=nm7,
                         directory=directory,
                         ...)
    # browser() Elins bug
    if(!is.null(phi.data)){
      ## check that phi file has right size
      if(dim(phi.data)[1]==dim(unique(xpobj@Data[xvardef("id",xpobj)]))[1]){
        xpobj@Data.firstonly <- phi.data
      }else{
        ## get the first unique ID values data from phi file
        first.phi.data <- phi.data[!duplicated(phi.data[,xvardef("id",xpobj)]),]
        sim.phi.data <- phi.data[duplicated(phi.data[,xvardef("id",xpobj)]),]

        xpobj@Data.firstonly <- first.phi.data

        nsim.phi.nrows <- dim(sim.phi.data)[1]
        first.phi.nrows <- dim(first.phi.data)[1]
        if(regexpr("\\.",as.character(nsim.phi.nrows/first.phi.nrows)) !=-1) {
          cat("The length of the Phi data and the Phi simulated data do not match!\n")
          return(xpobj)
        }
        nsim.phi <- nsim.phi.nrows/first.phi.nrows
      }
    }
  }

  ## is.readable loop
  ## check orig files + sim is.readable output & sim must match


  ## error handling for simulations!
  cat("\nLooking for NONMEM simulation table files.\n")
  gosim <- TRUE
  simct <- FALSE

  ## check if there are any simulation files
  for(i in 1:length(sim.files)) {
      if (is.readable.file(sim.files[i]))  {
          simct <- TRUE
      }
  }

  if (simct){
    for(i in 1:length(tab.files)) {
      if ((is.readable.file(tab.files[i])) && (!is.readable.file(sim.files[i])))  {
        err.mess <- paste(sim.files[i],"not found!")
        gosim <- FALSE
        break
      }
    }
  } else {
    gosim <- FALSE
  }


  if (gosim==FALSE) {

    if (!simct) {
      #cat("  Files are either not present or not named correctly\n")
      #cat("  (e.g. sdtab1a instead of sdtab1sim)\n")
    } else {
      cat("  There is not the same number of normal and \n")
      cat("  simulation table files for the current run number:\n")
      cat(paste("  ",err.mess,"\n",sep=""))
    }
    cat("No simulated table files read.\n\n")
  }

  if (gosim==TRUE) {
    simtmp <- read.nm.tables(sim.files,
                             #runno,
                             #tab.suffix=paste(sim.suffix,tab.suffix,sep=""),
                             #cwres.suffix=paste(sim.suffix,cwres.suffix,sep=""),
                             quiet=quiet,...)

    if(!is.null(tmp)) {
      SData(xpobj) <- simtmp
      cat("Simulation table files read.\n")
    } else {
      cat("There was a problem reading the simulation tables!\n")
      cat("Simulation tables not read!\n")
      return(NULL)
    }

    if(!is.null(nsim.phi)){
      ## check that there are the same number of simulations in the phi file and the table files
      if(!(xpobj@Nsim == nsim.phi)){
        cat("\nThere are not the same number of simulations\n",
            "in the table files and the phi file.\n",
            "Something is wrong with the phi file.\n",
            "It will not be used.\n",sep="")
        xpobj@Data.firstonly <- NULL
      } else {
        xpobj@SData.firstonly <- sim.phi.data
      }
    }
  }


  ## read options
  if (is.readable.file("xpose.ini")) { ## read options in current directory
    xpobj <- xpose.read(xpobj, file="xpose.ini")
  } else if (is.readable.file(file.path(path.expand("~"),"xpose.ini"))) {     ## read local options
    xpobj <- xpose.read(xpobj, file=file.path(path.expand("~"),"xpose.ini"))
  } else if (is.readable.file(system.file("xpose.ini",package="xpose4"))) {     ## read global options
    xpobj <- xpose.read(xpobj, file=system.file("xpose.ini",package="xpose4"))
  } else{
    cat("Cannot find a valid xpose.ini file!\n")
  }

  ## clean up
  if(file.exists(".sdtab.names.tmp")) file.remove(".sdtab.names.tmp")
  if(file.exists(".catab.names.tmp")) file.remove(".catab.names.tmp")
  if(file.exists(".cotab.names.tmp")) file.remove(".cotab.names.tmp")
  if(file.exists(".patab.names.tmp")) file.remove(".patab.names.tmp")

  tmp.obj <- read.vpctab(object=xpobj,
                         #inclZeroWRES=inclZeroWRES,
                         tab.suffix=tab.suffix,
                         ...)
  if(!is.null(tmp.obj)) xpobj <- tmp.obj

  if(is.null(check.vars(c("idv"),xpobj))) {
    cat("\n*********PLEASE NOTE: idv NOT IDENTIFIED************\n")
    cat("The independent variable (idv) has not been identified\n")
    cat("in the table files!  Please use the command line function\n")
    cat("'change.xvardef' (use '?change.xvardef' for help) or the classic\n")
    cat("menu system (select: Preferences/Manage variables/Change idv)\n")
    cat("to identify the name of the idv\n")
    cat("****************************************************\n")
  }



  return(xpobj)


}

