#' @rdname addid

create.rand <- function(data,object,frac,seed=NULL) {

  if(!is.null(seed)) {
    set.seed(seed)
  } else {
    seed <- "noseed"
  }

  if(missing(frac)) frac <- object@Prefs@Graph.prefs$dilfrac

  facnam <- paste("R",seed,sep="")
  ids    <- unique(data[,xvardef("id",object)])
  
  tmp    <- data.frame(ID=ids,R=sample(c(0,1),size=length(ids),replace=TRUE,
                         prob=c(frac,1-frac)))  

  names(tmp)        <- c(xvardef("id",object),facnam)
  newdata           <- merge(data,tmp,by=xvardef("id",object))
  newdata[,facnam]  <- as.factor(newdata[,facnam])

  xlabel(object) <- c(facnam,facnam)
  data[,facnam] <- newdata[,facnam]
  
  names(data)[length(names(data))] <- facnam
  invisible()
  return(data)
}
