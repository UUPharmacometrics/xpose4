#' Stepwise GAM search for covariates on a parameter (Xpose 4)
#' 
#' Function takes an Xpose object and performs a generalized additive model
#' (GAM) stepwise search for influential covariates on a single model parameter.
#' 
#' 
#' @param object An xpose.data object.
#' @param parnam ONE (and only one) model parameter name.
#' @param covnams Covariate names to test on parameter.
#' @param trace TRUE if you want GAM output to screen.
#' @param scope Scope of the GAM search.
#' @param disp If dispersion should be used in the GAM object.
#' @param start.mod Starting model.
#' @param family Assumption for the parameter distribution.
#' @param wts.data Weights on the least squares fitting of parameter vs.
#' covariate. Often one can use the variances of the individual parameter
#' values as weights. This data frame must have column with name ID and any
#' subset variable as well as the variable defined by the \code{wts.col}.
#' @param wts.col Which column in the \code{wts.data} to use.
#' @param steppit TRUE for stepwise search, false for no search.
#' @param subset Subset on data.
#' @param onlyfirst TRUE if only the first row of each individual's data is to
#' be used.
#' @param medianNorm Normalize to the median of parameter and covariates.
#' @param nmods Number of models to examine.
#' @param smoother1 Smoother for each model.
#' @param smoother2 Smoother for each model.
#' @param smoother3 Smoother for each model.
#' @param smoother4 Smoother for each model.
#' @param arg1 Argument for model 1.
#' @param arg2 Argument for model 2.
#' @param arg3 Argument for model 3.
#' @param arg4 Argument for model 4.
#' @param excl1 Covariate exclusion from model 1.
#' @param excl2 Covariate exclusion from model 2.
#' @param excl3 Covariate exclusion from model 3.
#' @param excl4 Covariate exclusion from model 4.
#' @param extra Extra exclusion criteria.
#' @param \dots Used to pass arguments to more basic functions.
#' @return Returned is a \code{\link[gam]{step.gam}} object
#' @author E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
#' @seealso \code{\link[gam]{step.gam}}
#' @examples
#' ## Run a GAM using the example xpose database 
#' gam_ka <- xpose.gam(simpraz.xpdb, parnam="KA")
#' 
#' ## Summarize GAM
#' xp.summary(gam_ka)
#' 
#' ## GAM residuals of base model vs. covariates
#' xp.plot(gam_ka)
#' 
#' ## An Akaike plot of the results
#' xp.akaike.plot(gam_ka)
#' 
#' ## Studentized residuals
#' xp.ind.stud.res(gam_ka)
#' 
#' ## Individual influence on GAM fit
#' xp.ind.inf.fit(gam_ka)
#' 
#' ## Individual influence on GAM terms
#' xp.ind.inf.terms(gam_ka)
#' 
#' ## Individual parameters to GAM fit
#' xp.cook(gam_ka)
#' 
#' @export xpose.gam
#' @family GAM functions 

xpose.gam <-
  function(object,
           parnam = xvardef("parms", object)[1],
           covnams = xvardef("covariates", object),
           trace = TRUE,
           scope = NULL,
           disp = object@Prefs@Gam.prefs$disp,
           start.mod=object@Prefs@Gam.prefs$start.mod,
           family="gaussian",
           wts.data =object@Data.firstonly,
           wts.col= NULL,#object@Prefs@Gam.prefs$wts,
           ## must have ID and any subset variable 
           ## well as a variable defined by wts.col
           steppit=object@Prefs@Gam.prefs$steppit,
           subset=xsubset(object),
           onlyfirst=object@Prefs@Gam.prefs$onlyfirst,
           medianNorm=object@Prefs@Gam.prefs$medianNorm,
           ## for the scope function
           nmods=object@Prefs@Gam.prefs$nmods,
           smoother1=object@Prefs@Gam.prefs$smoother1,
           smoother2=object@Prefs@Gam.prefs$smoother2,
           smoother3=object@Prefs@Gam.prefs$smoother3,
           smoother4=object@Prefs@Gam.prefs$smoother4,
           arg1=object@Prefs@Gam.prefs$arg1,
           arg2=object@Prefs@Gam.prefs$arg2,
           arg3=object@Prefs@Gam.prefs$arg3,
           arg4=object@Prefs@Gam.prefs$arg4,
           excl1=object@Prefs@Gam.prefs$excl1,
           excl2=object@Prefs@Gam.prefs$excl2,
           excl3=object@Prefs@Gam.prefs$excl3,
           excl4=object@Prefs@Gam.prefs$excl4,
           extra=object@Prefs@Gam.prefs$extra,
           ...) {
    
    ##
    ## Check the data
    ##
    for (i in xvardef("parms", object)) {
      if(is.null(i)) {
        cat("Parameters are not properly set in the database!\n")
        return()
      }
    }
    for (i in xvardef("covariates", object)) {
      if(is.null(i)) {
        cat("Covariates are not properly set in the database!\n")
        return()
      }
    }
    
    if(length(parnam)>1) {
      cat(
        "You have specified more than on parameter but you can only\n"
      )
      cat(
        "run the GAM on one parameter at a time.\n"
      )
      return()
    }
    
    ##Get data
    gamdata <- Data(object,subset=subset,onlyfirst=onlyfirst,...)
    if(any(is.null(gamdata))) return("The subset expression is invalid.")
    
    
    ## subset weights
    if(!is.null(wts.col)){
      if(!is.null(subset)){
        wts.data <- subset(wts.data,eval(parse(text=subset)))
      }
      ##check that ids are equal
      if(!all(gamdata$ID==wts.data$ID)){
        cat("Weights and data set do not have same ID values.\n")
        return()
      }
      ## add other weighting options
      #browser()
      #str(wts.data)
      #se*wpop/(wpop-se)
      
      
      ## assign weight column
      wts <- wts.data[,wts.col]
    } else {
      wts <- NULL
    }
    
    
    ##
    ## Normalize to median if requested
    ##
    if(medianNorm==TRUE) {
      for(i in covnams) {
        if(is.factor(gamdata[,i])) {
          next
        } else {
          gamdata[,i] <- gamdata[,i] - median(gamdata[, i])
        }
      }
      for(i in parnam) {
        if(is.factor(gamdata[,i])) {
          next
        } else {
          gamdata[,i] <- gamdata[,i] - median(gamdata[, i])
        }
      }
    }
    
    ## Check the length of the wts
    if(!is.null(wts)) {
      if(length(wts) != length(gamdata[[1]])) {
        cat("Weights and data have unequal length!\n")
        return()
      } else {
        ## looks good
      }
    }
    c1 <- call("assign","wts",wts,pos=1)
    eval(c1)
    c2 <- call("assign","gamdata",gamdata,pos=1)
    eval(c2)
    c3 <- call("assign","covnams",covnams,pos=1)
    eval(c3)
    
    ##
    ## Set starting model
    ##
    if(is.null(start.mod)) {
      form <- as.formula(paste(parnam,"~1"))
    } else {
      form <- start.mod
    }
    
    ##
    ## Check to see if the dispersion should be estimated
    ##
    if(!is.null(disp)) {
      disp1 <- xp.get.disp(gamdata=gamdata,parnam=parnam,covnams=covnams,family=family)
      disp2 <- disp1$dispersion
    }
    
    
    ##
    ## Nonice way to get the proportional error model (doesn't work right now)
    ##
    if(!any(is.null(wts))) {
      ##if(family=="quasi")
      ## bam.start <- gam(form,data=gamdata,weights=wts,
      ##                  family=quasi(link=identity,var="mu^2"))
      #else
      bam.start <- gam(form,weights=wts,data=gamdata)
    } else {
      ##if(family=="quasi")
      ##bam.start <- gam(form,data=gamdata,family=quasi(link=identity,var="mu^2"))
      ##else
      bam.start <- gam(form,data=gamdata)
    }
    
    ##
    ## Set the keep function
    ##
    "bam.keep.old.gam" <-
      function(object, AIC){
        list(df.resid = object$df.resid,
             deviance = object$deviance,
             term = as.character(object$formula)[3],
             labs = labels(object),
             AIC = AIC)
      }
    
    
      "bam.keep" <-
        function(object){
          list(df.resid = object$df.resid,
               deviance = object$deviance,
               term = as.character(object$formula)[3],
               labs = labels(object),
               AIC = object$aic)
        }
    
    
    ##
    ## Set the scope
    ##
    if(any(is.null(scope))){
      #scope <- xp.check.scope(object,covnams=covnams)
      scope <-xp.scope3(object,
                        covnam=covnams,
                        nmods=nmods,
                        smoother1=smoother1,
                        smoother2=smoother2,
                        smoother3=smoother3,
                        smoother4=smoother4,
                        arg1=arg1,
                        arg2=arg2,
                        arg3=arg3,
                        arg4=arg4,
                        excl1=excl1,
                        excl2=excl2,
                        excl3=excl3,
                        excl4=excl4,
                        extra=extra)
    }
    
    
    ##
    ## Run the GAM
    ##
    if(!is.null(steppit)){
      if(is.null(disp)){
        if(packageVersion("gam") >= "1.9.1"){
          nose1.parm <- step.gam(bam.start, trace = trace, scope, keep = bam.keep)
        } else {
          nose1.parm <- step.gam(bam.start, trace = trace, scope, keep = bam.keep.old.gam)
        }
      } else {
        if(packageVersion("gam") >= "1.9.1"){
          nose1.parm <- step.gam(bam.start, trace = trace, scope, keep = bam.keep,scale=disp2)
        } else {
          nose1.parm <- step.gam(bam.start, trace = trace, scope, keep = bam.keep.old.gam,scale=disp2)
        }
      }
    } else {
      nose1.parm <- bam.start
    }
    
    
    
    ## add to gam object
    if(!is.null(disp)) {
      nose1.parm$dispersion <- disp2
    } else {
      nose1.parm$dispersion <- disp
    }
    
    if(!is.null(steppit)){
      nose1.parm$steppit <- steppit
    }
    
    nose1.parm$subset <- subset
    nose1.parm$onlyfirst <- onlyfirst
    nose1.parm$medianNorm <- medianNorm
    nose1.parm$pars <- parnam
    nose1.parm$runno <- object@Runno
    
    
    remove("gamdata",pos=1)
    remove("wts",pos=1)
    remove("covnams",pos=1)
    return(nose1.parm)
  }





