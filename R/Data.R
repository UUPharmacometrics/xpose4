#' Extract or assign data from an xpose.data object.
#' 
#' Extracts or assigns the data from the Data or SData slots in an "xpose.data" object.
#' 
#' 
#' When using Data to assign a data.frame to the Data slot in the "xpose.data"
#' object a number of things happen:
#' 
#' Each column in the data.frame is checked and set to factor if the number of
#' unique values are less than the value of Cat.levels (see
#' \code{\link{xpose.prefs-class}}).
#' 
#' It is checked which of the predefined xpose data variables that exists in
#' the data.frame. The variable defintions that does not exist are set to NULL.
#' 
#' The column identified by the \code{dv} xpose variable definition, is checked
#' and set to factor if the number of unique values are less than or equal to
#' the DV.Cat.levels (see \code{\link{xpose.prefs-class}}).
#' 
#' Finally, each column name in the data.frame is checked for a label (see
#' \code{\link{xpose.prefs-class}}). If it is non-existent, the label is set to
#' the column name.
#' 
#' When SData is used to assign a data.frame to the SData slot it is first
#' checked that the number of rows in the SData data.frame is an even multiple
#' of the number of rown in Data. Next, each column in the SData data.frame is
#' assigned the same class as the corresponding column in the Data data.frame
#' (it is required that the columns are the same in Data and SData). Finally,
#' an extra column, "iter", is added to SData, which indicates the iteration
#' number that each row belongs to. At the same time, the Nsim slot of the
#' "xpose.data" object is set to the number of iterations (see
#' \code{\link{nsim}}).
#' 
#' @param object An "xpose.data" object
#' @param inclZeroWRES Logical value indicating whether rows with WRES==0
#' should be included in the extracted data.
#' @param onlyfirst Logical value indicating whether only the first line per
#' individual should be included in the extracted data.
#' @param subset Expression with which the extracted data should be subset (see
#' \code{\link{xsubset}})
#' @param samp An integer between 1 and object@Nsim
#' (see\code{\link{xpose.data-class}}) specifying which of the simulated data
#' sets to extract from SData.
#' @param quiet \code{TRUE or FALSE} if \code{FALSE} then some more information
#' is printed out when adding data to an Xpose object.
#' @param keep.structure \code{TRUE or FALSE} if\code{FALSE} then values are
#' converted to continuous or categorical according to the rules set up by
#' xpose using object@Prefs@Cat.levels, object@Prefs@DV.cat.levels and the
#' values in the "catab" file.
#' @param value An R data.frame.
#' @return Returns a data.frame from the Data or SData slots, excluding rows as
#' indicated by the arguments.
#' @author Niclas Jonsson
#' @seealso \code{\link{xpose.data-class}},\code{\link{xpose.prefs-class}}
#' @keywords methods
#' @examples
#' 
#' xpdb <- simpraz.xpdb
#' 
#' ## Extract data
#' my.dataframe <- Data(xpdb)
#' 
#' ## Assign data
#' Data(xpdb) <- my.dataframe
#' 
#' ## Extract simulated data
#' my.simulated.dataframe <- SData(xpdb)
#' 
#' ## Assign simulated data
#' SData(xpdb) <- my.simulated.dataframe
#' 
#' @family data functions 
#' @name data_extract_or_assign
NULL


#' @describeIn data_extract_or_assign Extract data
#' @export
Data <- function(object,
                 inclZeroWRES=FALSE,
                 onlyfirst=FALSE,
                 subset=NULL) {

  data <- object@Data
  if(!inclZeroWRES) {
    data <- data[data[,xvardef("wres",object)]!=0,]
  }

  if(onlyfirst) {
    data <- data[!duplicated(data[,xvardef("id",object)]),]
  }
  
  if(!is.null(subset)) {
    #attach(data)
    #on.exit(detach(data))
    
                                        # data <- data[eval(parse(text=subset)),]
    #browser()
    #data <- data[eval(parse(text=paste("data$", subset))),] # fix subsets 22/3/06
    data<-with(data,data[eval(parse(text=subset)),])
    

    if(dim(data)[1]==0) return(NULL)
  }

  return(data)

}


#' @describeIn data_extract_or_assign assign data
#' @export
"Data<-" <- function(object,quiet=TRUE,keep.structure=F,value) {

  if(is.null(value)) {
    return(NULL)
  }

  object@Data     <- value
  totab.nam  <- names(value)
                                        # cat(totab.nam, "\n")

  ## define the important variables in the dataset
  singdef <-
    c("id","idlab","idv","dv","pred","ipred","iwres","wres","res","cwres")

  ## Check if all variable definitions exist in the data
  for(v in singdef) {
    if(is.null(xvardef(v,object))){
      next
    }
    if(is.na(pmatch(xvardef(v,object), totab.nam))) {
      object@Prefs@Xvardef[[v]] <- NULL

      ## if DV undefined check what 4th from last in list files says
      ## must make sure no append is not called
      if(v=="dv"){
        tab.names.list <-
          c(".sdtab.names.tmp",".patab.names.tmp",".catab.names.tmp",".cotab.names.tmp")
        append.list <- c("PRED","RES","WRES")
        dv.found <- FALSE
        for(vv in tab.names.list){
          if(!dv.found){
            if (file.exists(vv)){
              tmp.list <- scan(vv, what="list", quiet=T)
              tmp.length <- length(tmp.list)
              if (all(tmp.list[(tmp.length-2):tmp.length]==append.list)){
                object@Prefs@Xvardef[[v]] <- tmp.list[[tmp.length-3]]
                dv.found <- TRUE
              }
            }
          }
        }
      }

      ## IF TIME isn't in tables is TAD?
      if(v=="idv"){
        if(!is.na(pmatch("TAD",totab.nam))){
          object@Prefs@Xvardef[[v]] <- "TAD"
        } else {
          if(!is.na(pmatch("CP",totab.nam))){
            object@Prefs@Xvardef[[v]] <- "CP"
          }
        }
      }
    }
  }

  
  ## sort out covariates and parameters
  ## look at the difference between the names in singdef
  ## and the names in *.tmp files
  if (file.exists(".patab.names.tmp")) {
    xparms <- setdiff(scan(".patab.names.tmp", what="list", quiet=T), sapply(singdef, xvardef, object))
    ##unlink("patab.names.txt")
  } else {
    xparms <- NULL
  }
  
  if (file.exists(".catab.names.tmp")) {
    xcatcovs <- setdiff(scan(".catab.names.tmp", what="list", quiet=T), sapply(singdef, xvardef, object))
    ##unlink("catab.names.txt")
  } else {
    xcatcovs <- NULL
  }
  
  if (file.exists(".cotab.names.tmp")) {
    xconcovs <- setdiff(scan(".cotab.names.tmp", what="list", quiet=T), sapply(singdef, xvardef, object))
    ##unlink("cotab.names.txt")
  } else {
    xconcovs <- NULL
  }
  
  if (!is.null(xcatcovs)) {
    if (!is.null(xconcovs)) {
      xcovs <- c(xcatcovs, xconcovs)
    } else {
      xcovs <- xcatcovs
    }
  } else {
    if (!is.null(xconcovs)) {
      xcovs <- xconcovs
    } else {
      xcovs <- NULL
    }    
  }

  ## check for categorical variables
  if(!keep.structure){
    for(v in totab.nam) {
      if (!is.na(match("TIME",v))) next
      if (!is.na(match("TAD",v))) next
      if (!is.na(match("CP",v))) next
      if (!is.na(match("PRED",v))) next
      if (!is.na(match("IPRE",v))) next
      if (!is.na(match("IPRED",v))) next
      num <- length(unique(object@Data[,v]))
      if(is.null(xvardef("dv",object))){
        if(num <= object@Prefs@Cat.levels) {
          if(!is.factor(object@Data[,v])){
            if(!quiet){
              cat("\n  Inferring that ",v," is categorical (",num," levels).\n",sep="")
              cat("  Transforming",v,"from continuous to categorical\n",sep=" ")
            }
            object@Data[,v] <- as.factor(object@Data[,v])
          }
        } else {
          if(is.factor(object@Data[,v])){
            if(!quiet){
              cat("\n  Inferring that",v,"is continuous.\n",sep=" ")
              cat("  Transforming",v,"from categorical to continuous\n",sep=" ")
            }
            object@Data[,v] <- as.numeric(levels(object@Data[,v]))[object@Data[,v]]
          }
        }
      } else {
        if(is.na(match(xvardef("dv",object), v))) {
          if(num <= object@Prefs@Cat.levels) {
            if(!is.factor(object@Data[,v])){
              if(!quiet){
                cat("\n  Inferring that ",v," is categorical (",num," levels).\n",sep="")
                cat("  Transforming",v,"from continuous to categorical\n",sep=" ")
              }
              object@Data[,v] <- as.factor(object@Data[,v])
            }
          } else {
            if(is.factor(object@Data[,v])){
              if(!quiet){
                cat("\n  Inferring that",v,"is continuous.\n",sep=" ")
                cat("  Transforming",v,"from categorical to continuous\n",sep=" ")
              }
              object@Data[,v] <- as.numeric(levels(object@Data[,v]))[object@Data[,v]]
            }
          }
        } else {
          if(num <= object@Prefs@DV.Cat.levels) {
            if(!is.factor(object@Data[,v])){
              if(!quiet){
                cat("\n  Inferring that DV is categorical (",num," levels).\n",sep="")
                cat("  Transforming DV from continuous to categorical\n")
              }
              object@Data[,v] <- as.factor(object@Data[,v])
            }
          } else {
            if(is.factor(object@Data[,v])){
              if(!quiet){
                cat("\n  Inferring that DV is continuous.\n",sep="")
                cat("  Transforming DV from categorical to continuous\n")
              }
              object@Data[,v] <- as.numeric(levels(object@Data[,v]))[object@Data[,v]]
            }
          }
        }
      }
    }
    if(is.factor(object@Data[,xvardef("dv",object)])){
      for (v in totab.nam){
        num <- length(unique(object@Data[,v]))
        if (!is.na(match("PRED",v)) | !is.na(match("IPRE",v)) | !is.na(match("IPRED",v))) {
          if(!quiet){
            cat("\n  Inferring that ",v," is categorical (",num," levels).\n",sep="")
            cat("  Transforming",v,"from continuous to categorical\n",sep=" ")
          }
          object@Data[,v] <- as.factor(object@Data[,v])
        }
      }
    } else {
      for (v in totab.nam){
        if (!is.na(match("PRED",v)) | !is.na(match("IPRE",v)) | !is.na(match("IPRED",v))) {
          if(is.factor(object@Data[,v])){
            if(!quiet){
              cat("\n  Inferring that",v,"is continuous.\n",sep=" ")
              cat("  Transforming",v,"from categorical to continuous\n",sep=" ")
            }
            object@Data[,v] <- as.numeric(levels(object@Data[,v]))[object@Data[,v]]
          }
        }
      }
    }
  }
  
  for(v in xcatcovs) {
    object@Data[,v] <- as.factor(object@Data[,v])
  }

  
  if (is.null(xparms)) {
    ep <- c()
    for(v in xvardef("parms",object)) {
      if(!is.na(pmatch(v, totab.nam))) {
        ep <- c(v,ep)
      }
    }
    object@Prefs@Xvardef[["parms"]] <- ep
  } else {
    object@Prefs@Xvardef[["parms"]] <- xparms
  }

  ep <- c()
  for(v in xvardef("tvparms",object)) {
    if(!is.na(pmatch(v, totab.nam))) {
      ep <- c(v,ep)
    }
  }
  object@Prefs@Xvardef[["tvparms"]] <- ep

  ep <- c()
  ranpar.loc <- grep("^ETA",totab.nam)
  if (length(ranpar.loc)!=0){
    ep <- totab.nam[ranpar.loc]
  }
  ##   for(v in xvardef("ranpar",object)) {
  ##     if(!is.na(pmatch(v, totab.nam))) {
  ##       ep <- c(v,ep)
  ##     }
  ##   }
  if ((is.vector(ep)) || (is.vector(ep))) {
    object@Prefs@Xvardef[["ranpar"]] <- sort(ep)
  } else {
    object@Prefs@Xvardef[["ranpar"]] <- ep
  }

  
  ##   ep <- c()
  ##   for(v in xvardef("cat.cov",object)) {
  ##     if(!is.na(pmatch(v, totab.nam))) {
  ##       ep <- c(v,ep)
  ##       object@Data[,v] <- as.factor(object@Data[,v])
  ##     }
  ##   }
  ##   object@Prefs@Xvardef[["cat.cov"]] <- ep
  
  if (is.null(xcovs)) {
    ep <- c()
    for(v in xvardef("covariates",object)) {
      if(!is.na(pmatch(v, totab.nam))) {
        ep <- c(v,ep)
      }
    }
    object@Prefs@Xvardef[["covariates"]] <- ep
  } else {
    object@Prefs@Xvardef[["covariates"]] <- xcovs
  }

  ## Fix the labels
  for(v in totab.nam) {
    if(is.null(xlabel(v,object))) {
      object@Prefs@Labels[[v]] <- v
    }
  }

  return(object)
}
