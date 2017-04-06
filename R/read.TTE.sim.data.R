


#' Read (repeated) time-to-event simulation data files.
#' 
#' @param sim.file Name of the simulated file.
#' @param subset subset to extract.
#' @param headers headers in file.
#' @param xpose.table.file xpose table files.
#' @param \dots Extra arguments passed to function.
#' @author Andrew C. Hooker
#' @export read.TTE.sim.data
#' @family data functions 
read.TTE.sim.data <-
  function(sim.file,
           subset=NULL,
           headers=c("REP", "ID", "DV", "TIME", "FLAG2","DOSE"),
           xpose.table.file=FALSE,
           ...)
{
  if(xpose.table.file){
    sim.data <- read.nm.tables(sim.file,quiet=T,...)
  } else {
    sim.data <- read.table(sim.file,skip=0,header=F)
    names(sim.data) <- headers
  }
  d.sim.sub <- sim.data[eval(parse(text=paste("sim.data$", subset))),]

  ## add a unique ID identifier
  rle.result <- rle(d.sim.sub$ID)
  rle.result$values <- 1:length(rle.result$values)
  new.id <- inverse.rle(rle.result)
  d.sim.sub$new.id <- new.id

  ## old way to add a unique ID identifier
  ## d.sim.sub$new.id <- max(unique(d.sim.sub$ID))*(d.sim.sub$REP-1)+d.sim.sub$ID

  tmp <- d.sim.sub
  tmp2 <- tmp[!duplicated(tmp$new.id, fromLast = TRUE),]
  d.sim <- tmp2
  return(d.sim)
}
