# Xpose 4
# An R-based population pharmacokinetic/
# pharmacodynamic model building aid for NONMEM.
# Copyright (C) 1998-2004 E. Niclas Jonsson and Mats Karlsson.
# Copyright (C) 2005-2008 Andrew C. Hooker, Justin J. Wilkins, 
# Mats O. Karlsson and E. Niclas Jonsson.
# Copyright (C) 2009-2010 Andrew C. Hooker, Mats O. Karlsson and 
# E. Niclas Jonsson.

# This file is a part of Xpose 4.
# Xpose 4 is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation, either version 3
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.

# You should have received a copy of the GNU Lesser General Public License
# along with this program.  A copy can be cound in the R installation
# directory under \share\licenses. If not, see http://www.gnu.org/licenses/.


#' Reading NONMEM table files
#' 
#' Reads one or more NONMEM table files, removes duplicated columns and merges
#' the data into a data.frame.
#' 
#' Reads one or more table files, removes duplicate columns and merges the
#' data. The function also checks to see if the table files are of the same
#' length (required).
#' 
#' If there are header lines in the table files (for example if your data are
#' simulated with NSUB>1), these are removed.
#' 
#' The table file names to read are constructed from the file name templates of
#' \code{table.names}. The \code{runno} and \code{tab.suffix} are appended to
#' the file name template before checking if the file is readable.
#' 
#' Xpose expects, by default, to find the following NONMEM tables in the
#' working directory to be able to create an Xpose data object (using a run
#' number of 5 as an example):
#' 
#' sdtab5: The 'standard' parameters, including IWRE, IPRE, TIME, and the
#' NONMEM default items (DV, PRED, RES and WRES) that are added when NOAPPEND
#' is not present in the \code{$TABLE} record.
#' 
#' \code{   $TABLE ID TIME IPRE IWRE NOPRINT ONEHEADER FILE=sdtab5}
#' 
#' patab5: The empirical Bayes estimates of individual model parameter values,
#' or posthoc estimates. These are model parameters, such as CL, V2, ETA1, etc.
#' 
#' \code{   $TABLE ID CL V2 KA K F1 ETA1 ETA2 ETA3 NOPRINT NOAPPEND ONEHEADER
#' FILE=patab5 }
#' 
#' catab5: Categorical covariates, e.g. SEX, RACE.
#' 
#' \code{   $TABLE ID SEX HIV GRP NOPRINT NOAPPEND ONEHEADER FILE=catab5 }
#' 
#' cotab5: Continuous covariates, e.g. WT, AGE.
#' 
#' \code{   $TABLE ID WT AGE BSA HT GGT HB NOPRINT NOAPPEND ONEHEADER FILE=cotab5}
#' 
#' mutab5, mytab5, extra5, xptab5: Additional variables of any kind. These
#' might be useful if there are more covariates than can be accommodated in the
#' covariates tables, for example, or if you have other variables that should
#' be added, e.g. CMAX, AUC.
#' 
#' @param table.files Exact names of table files to read.  If not provided then
#' the exact names are created using the other arguments to the function.
#' @param runno Run-number to identify sets of table files.
#' @param tab.suffix Table file name suffix.
#' @param table.names Vector of template table file names to read.
#' @param cwres.name Vector of CWRES table file names to read.
#' @param cwres.suffix CWRES table file name suffix.
#' @param quiet Logical value to indicate whether some warnings should be quiet
#' or not.
#' @param new_methods Should faster methods of reading tables be used (uses readr package)? 
#' @param \dots Additional arguments passed to this function
#' @return A dataframe.
#' @author Niclas Jonsson, Andrew Hooker
#' @seealso \code{\link{xpose.data-class}}, \code{\link{compute.cwres}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## We expect to find the required NONMEM run and table files for run
#' ## 5 in the current working directory, and that the table files have
#' ## a suffix of '.dat', e.g. sdtab5.dat
#' 
#' my.dataframe <- read.nm.tables(5, tab.suffix = ".dat") 
#' }
#' 
#' 
#' 

read.nm.tables <-
  function(table.files=NULL,
           runno=NULL,
           tab.suffix="",
           ##sim.suffix="sim",
           table.names=c("sdtab","mutab","patab","catab",
                         "cotab","mytab","extra","xptab"),
           cwres.name=c("cwtab"),
           cwres.suffix="",
           quiet=FALSE,
           new_methods=TRUE,
           ...) {
    
    if (is.null(table.files)){
      if(is.null(runno)) {
        cat(paste("runno must be specified if no table files provided\n"))
        return(NULL)
      }
      match.pos <- match(cwres.name,table.names)
      if (!is.na(match.pos)) table.names <- table.names[-match.pos]
      tab.files <- sapply(table.names,paste,runno,tab.suffix,sep="")
      cwres.files <- sapply(cwres.name,paste,runno,cwres.suffix,tab.suffix,sep="")
      tab.files <- c(tab.files,cwres.files)
    } else {
      tab.files <- table.files
    }
    
    ## Read in the table files
    totab      <- NULL
    totnam     <- NULL
    seen.files <- NULL
    filedim    <- NULL
    
    for(i in 1:length(tab.files)) {
      filename <- tab.files[i]
      if(!is.readable.file(filename)) {
        ##if (!quiet) {cat(filename,"not readable\n")}
        next
        
      } else {
        cat(paste("    Reading",filename,"\n"))
        
        if(new_methods){
          assign(paste0("n.",filename),read_nm_table(filename, quiet=quiet,...)) 
        } else {
                    
          ## Check which type of separator we have in our tables
          header.line = scan(file=filename,nlines=1,skip=1,what="character",sep="\n",quiet=T)
          sep.char = ""
          if(length(grep(",",header.line))!=0) sep.char = ","
          
          ## Check if we have unequal number of fields in the file
          ## used for multiple simulations
          fields.per.line <- count.fields(filename)
          fields.in.first.line <- fields.per.line[1]
          fields.in.rest <- fields.per.line[-1]
          if((length(unique(fields.in.rest))!=1) ||
             (all(fields.in.first.line==fields.in.rest))){ 
            if(!quiet) {
              cat(paste("Found different number of fields in ",filename,".\n",sep=""))
              cat("This may be due to multiple TABLE and header rows \n")
              cat("caused by running multiple simulations in NONMEM (NSIM > 1).\n")
              cat("Will try to remove these rows. It may take a while...\n")
            }
            tmp   <- readLines(filename, n = -1)
            inds  <- grep("TABLE",tmp)
            if (length(inds)!=1){
              inds  <- inds[c(2:length(inds))]
              inds2 <- inds+1
              tempfile<- paste(filename,".xptmp",sep="")
              write.table(tmp[-c(inds,inds2)],file=tempfile,
                          row.names=FALSE,quote=FALSE)
              assign(paste("n.",filename,sep=""),read.table(tempfile,skip=2,header=T,sep=sep.char))
              unlink(tempfile)
            } else {
              assign(paste("n.",filename,sep=""),read.table(filename,skip=1,header=T,sep=sep.char))
            }
          } else {
            assign(paste("n.",filename,sep=""),read.table(filename,skip=1,header=T,sep=sep.char))
          }
          
        }
        ## Remember the files seen
        ##if(is.null(seen.files)) {
        ##  seen.files <- paste("n.",filename,sep="")
        ##} else {
        seen.files <- c(seen.files,paste("n.",filename,sep=""))
        ##}
      }
    }
    
    ## Check if we found any table files
    
    if(any(is.null(seen.files))) {
      #if(tab.suffix!=sim.suffix) {
      cat("Couldn't find any table files that match run number",
          runno, "!\n")
      return(NULL)
      #} else {
      #  cat("Couldn't find any simulation table files that match run number",
      #     runno, "!\n")
      #}
    }
    
    ## Check if the files have the same length
    for(nfile in seen.files) {
      if(is.null(filedim)) {
        filedim <- nrow(get(nfile))
      } else {
        filedim <- c(filedim,nrow(get(nfile)))
      }
    }
    
    file.df <- data.frame(seen.files=seen.files,filedim=filedim)
    lngths  <- sort(unique(file.df$filedim))
    
    if(length(lngths) !=1) {
      cat("\nThe table files associated with this run number (",runno,
          ") appear\n")
      cat("to have different lengths.\n")
      cat("You will have to sort this out and try again!\n")
      return(NULL)
    }
    
    
    ## Add the tables to totab and replicate the shorter ones to match
    ## the size of the longest one
    maxlngth <- max(file.df$filedim)
    
    ##singdef <-
    ##  c("id","idlab","idv","dv","pred","ipred","iwres","wres","res")
    
    for(ii in 1:nrow(file.df)) {
      filnam <- as.character(file.df[ii,"seen.files"])
      new.df <- get(filnam)
      sz     <- file.df[ii,"filedim"]
      rl     <- maxlngth/sz
      
      if(any(is.null(totab))) {
        totab <- cbind(new.df)
        #totab <- new.df
      } else {
        totab <- cbind(totab,new.df)
      }
      
      totnam <- c(totnam,names(new.df))
      
      ## store parameters & covariates for Data.R & SData.R
      
      ##       if(!is.na(pmatch("n.patab", filnam))){ 
      ##         write(names(new.df), file=".patab.names.tmp")
      ##       } else {
      ##         if(!is.na(pmatch("n.catab", filnam))){ 
      ##           write(names(new.df), file=".catab.names.tmp")
      ##         } else {
      ##           if(!is.na(pmatch("n.cotab", filnam))){ 
      ##             write(names(new.df), file=".cotab.names.tmp")
      ##           } 
      ##         }
      ##       }
      
      if(!is.na(pmatch("n.patab", filnam))){ 
        write(names(new.df), file=".patab.names.tmp")
      } else {
        if(!is.na(pmatch("n.catab", filnam))){ 
          write(names(new.df), file=".catab.names.tmp")
        } else {
          if(!is.na(pmatch("n.cotab", filnam))){ 
            write(names(new.df), file=".cotab.names.tmp")
          } else {
            if(!is.na(pmatch("n.sdtab", filnam))){ 
              write(names(new.df), file=".sdtab.names.tmp")
            }  
          } 
        }
      }
      
      
    } 
    
    # cat(totnam, "\n")
    
    ## Delete all duplicates
    
    totab <- totab[, !duplicated(totnam)]
    return(totab)
  }

