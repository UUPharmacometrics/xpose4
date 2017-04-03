#' @rdname categorical.table

continuous.table  <- function(object, 
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
    ret.mat <- matrix(0,ncol=9,nrow=1+length(vars))
    ret.mat[1,] <- c("","Mean","SD","Q1","Median","Q3","Range","N","Missing")
    
  } else {
    ret.mat <- matrix(0,ncol=8,nrow=1+length(vars))
    ret.mat[1,] <- c("","Mean","SD","Q1","Median","Q3","Range","N")
    
  }
  
  i <- 1
  
  for(nam in vars) {
    i <- i+1
    
    micov <- subset(data[[nam]], is.na(data[[nam]]) | data[[nam]]==miss)
    nomicov<- subset(data[[nam]], !is.na(data[[nam]]) & data[[nam]]!=miss)
    suma <- summary(nomicov)[c(4,2,3,5,6,1)]

    if (usemiss==TRUE) {
      ret.mat[i,] <- c(nam,
                       suma[1],
                       signif(sd(nomicov), digits=4),
                       suma[2:4],
                       paste(suma[6],"-",suma[5],sep=""),
                       length(nomicov),
                       paste(length(micov)," (",
                             sprintf("%.1f",
                                     100*length(micov)/(length(micov)+length(nomicov))),
                             "%)",sep="")
                       )

    } else {
      ret.mat[i,] <- c(nam,
                       suma[1],
                       signif(sd(nomicov), digits=4),
                       suma[2:4],
                       paste(suma[6],"-",suma[5],sep=""),
                       length(nomicov)
                       )

    }
  }

  class(ret.mat) <- "char.matrix"

  return(ret.mat)
}



