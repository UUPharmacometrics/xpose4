

#' Default function for calculating dispersion in \code{\link{xpose.gam}}.
#'
#' @inheritParams xpose.gam
#' @param gamdata the data used for a GAM
#' 
#' @return a list including the dispersion
#' @export
#' @family GAM functions 
#' @importFrom splines ns
#'
xp.get.disp <-function(gamdata,
                       parnam,
                       covnams,
                       family="gaussian",
                       ...)
{
  ##
  ## Run the null gam
  ##
  form <- as.formula(paste(parnam,"~1"))
  gam.null <- gam(form,data=gamdata)
  
  
  ## check that categorical covariates have more than one factor
  for(cov in covnams){
    if(is.factor(gamdata[, cov]) && nlevels(gamdata[,cov])==1){
      covnams=covnams[covnams!=cov]
    }
  }
  
  sel=rep(FALSE,times=length(covnams))
  names(sel)=covnams
  sel2=rep(FALSE,times=length(covnams))
  names(sel2)=covnams
  
  for(i in covnams) {
    ##
    ## Run the gam with the covariate entering linearly
    ##
    form <- as.formula(paste(parnam,"~",i))
    gam1 <- gam(form,data=gamdata)
    
    ##
    ## If we are dealing with a continous covariate
    ## run the gam on the non-linear function as well
    ## and comapre it to the linear fit
    if(!is.factor(gamdata[, i])) {
      form <- as.formula(paste(parnam," ~ ns(",i,", df=2)"))
      gam2 <- gam(form,data=gamdata)
      p    <- anova(gam1, gam2, test = "F")$Pr[2]
      
      ##
      ## If the non-linear was the better one, compare it to the null
      ## gam else compare the ninear to the null gam
      ##
      if(p < 0.05) {
        p <- anova(gam.null, gam2, test = "F")$Pr[2]	 
        if(p < 0.05){
          sel2[i] <- T
        }
      } else {
        p <- anova(gam.null, gam1, test = "F")$Pr[2]
        if(p < 0.05){
          sel[i] <- T
        }
      }
      ##
      ## If we are delaing with a factor, comapre it to the null gam
      ##
    } else {
      p <- anova(gam.null, gam1, test = "F")$Pr[2]
      if(p < 0.05){
        sel[i] <- T
      }
    }
  }
  
  
  ##
  ## Assemble the formula to use in the dispersion getting gam
  ##
  
  form <- NULL
  if(any(sel)) {
    form <- paste(names(sel)[sel], collapse = "+")
  }
  if(any(sel2)) {
    ncov <- names(sel2)[sel2]
    for(i in 1:length(ncov))
      ncov[i] <- paste("ns(",ncov[i],",df=2)",sep="")
    
    if(!any(is.null(form)))
      form <- paste(form,"+",paste(ncov,collapse="+"))
    else
      form <- paste(ncov,collapse="+")
  }
  
  if(is.null(form))
    gamform <- as.formula(paste(parnam,"~ 1"))
  else
    gamform <- as.formula(paste(parnam,"~",form))
  
  ##
  ## Run the dispersion getting GAM
  ##
  
  if(family == "gaussian") {
    gam3 <- gam(gamform,data=gamdata)
  } else {
    gam3 <- gam(gamform,data=gamdata,family=quasi(link=identity,variance="mu^2"))
  }
  
  disp <- summary(gam3)$dispersion
  
  ret.list <- list(covs = form,
                   formula = gamform,
                   dispersion = disp)
  return(ret.list)
}
