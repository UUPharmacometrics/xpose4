#' Generic table functions for Xpose 4
#' 
#' These are internal table functions relating to the Xpose summary functions.
#' 
#' These are internal Xpose functions for outputting summary tables. They are
#' not intended for direct use.
#' 
#' @aliases continuous.table categorical.table
#' @return Internal helper functions for the generic Xpose summary functions.
#' @author Niclas Jonsson, Justin Wilkins and Andrew Hooker
#' @keywords internal


categorical.table  <- function(object, 
                                 vars,
                                 onlyfirst=TRUE,
                                 subset=xsubset(object),
                                 inclZeroWRES=FALSE,
                                 miss=object@Prefs@Miss) # can be a number
{

  data <- Data(object,onlyfirst=onlyfirst,subset=subset,inclZeroWRES=inclZeroWRES)
  

  usemiss <- FALSE
  
  for (nam in vars) {
    if(any(is.na(data[[nam]]) | data[[nam]]==miss)) {
      usemiss <- TRUE
    }
  }

  if (usemiss == TRUE) {
    ret.mat <- matrix(0,ncol=5,nrow=1+length(vars))
    ret.mat[1,] <- c("","Category","N","%","Missing")
    #ret.mat[1,] <- c("","Category","N","%")  
  } else {
    ret.mat <- matrix(0,ncol=4,nrow=1+length(vars))
    ret.mat[1,] <- c("","Category","N","%")  
  }
  

  i <- 1

  for(nam in vars) {
    
    if (is.factor(data[[nam]])) {
      i <- i+1

      micov <- subset(data[[nam]], is.na(data[[nam]]) | data[[nam]]==miss)
      nomicov<- subset(data[[nam]], !is.na(data[[nam]]) & data[[nam]]!=miss)
            
      if (usemiss==TRUE) {
        ret.mat[i,] <- c(nam,
                         paste(levels(data[[nam]]),collapse="\n"),
                         paste(tabulate(data[[nam]]),collapse="\n"),
                         paste(sprintf("%.1f",
                                       100*tabulate(data[[nam]])/sum(tabulate(data[[nam]]))),
                                       collapse="\n"),
                         paste(length(micov)," (",
                             sprintf("%.1f",
                                     100*length(micov)/(length(micov)+length(nomicov))),
                             "%)",sep="")
                         )
      } else {
        ret.mat[i,] <- c(nam,
                         paste(levels(data[[nam]]),collapse="\n"),
                         paste(tabulate(data[[nam]]),collapse="\n"),
                         paste(sprintf("%.1f",
                                       100*tabulate(data[[nam]])/sum(tabulate(data[[nam]]))),
                               collapse="\n")
                         )
      }
    }
  }

  class(ret.mat) <- "char.matrix"

  return(ret.mat)
}

