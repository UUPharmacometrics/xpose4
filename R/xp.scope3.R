#' Define a scope for the gam.  Used as default input to the \code{scope} argument in 
#' \code{xpose.gam} 
#' 
#' @inheritParams xpose.gam
#' @param covnam Covariate names to test.
#' 
#' @examples 
#' 
#' xp.scope3(simpraz.xpdb)
#' 
#' @export
#' @family GAM functions 

xp.scope3 <-
  function(object,
           covnam=xvardef("covariates", object),
           nmods = 3,
           smoother1 = 0,
           arg1 = NULL,
           smoother2 = 1,
           arg2 = NULL,
           smoother3 = "ns",
           arg3 = "df=2",
           smoother4 = "ns",
           arg4 = "df=3", 
           excl1 = NULL,
           excl2 = NULL,
           excl3 = NULL,
           excl4 = NULL,
           extra = NULL,
           subset=xsubset(object),
           ...)
  {
    ##Get data
    data <- Data(object,subset=subset)
    if(any(is.null(data))) return("The subset expression is invalid.")    
    
    ## parnam <- xvardef("parms", object)
    ## covnams <- xvardef("covariates", object)
    step.list <- as.list(covnam)
    names(step.list) <- covnam
    
    if(is.null(extra)) extra <- list()	## Set up the smoother lists
    
    for(cov in covnam) {
      ## Check that the covariate is not excluded completely
      if(!is.na(match(cov, excl1)) && !is.na(match(cov, excl2)) && !
         is.na(match(cov, excl3)) && !is.na(match(cov, excl4))) 
      {
        stop("A covariate cannot be excluded from all levels in the GAM scope.\n")
      }
      
      ## check that categorical covariates have more than one factor
      if(is.factor(data[, cov]) && nlevels(data[,cov])==1){
        step.list=step.list[names(step.list)!=cov]
      } else
      { # create the scope
        tmp.scope <- character(0)
        for(i in 1:nmods) {
          excl <- eval(parse(text = paste("excl", i, sep = "")))
          if(!is.na(match(cov, excl)))
            next
          if(is.factor(data[, cov]) && i == 1) {
            tmp.scope <- c(tmp.scope, "1")
            next
          }	else if(is.factor(data[, cov]) && i == 2) {
            tmp.scope <- c(tmp.scope, cov)
            next
          }  else if(is.factor(data[, cov]) && i > 2) {
            next
          }
          ## Check if we have any specific covariate settings for smoother
          if(!is.null(extra[[cov]][[paste("sm", i, sep = "")]])) 
          {
            sm <- extra[[cov]][[paste("sm", i, sep = "")]]
          } else {
            sm <- eval(parse(text = paste("smoother", i, 
                                          sep = "")))
          }
          if(sm == 0) {
            tmp.scope <- c(tmp.scope, "1")
          }	else if(sm == 1) {
            tmp.scope <- c(tmp.scope, cov)
          }  else {
            ## Check if we have any specific settings for  arg
            if(!is.null(extra[[cov]][[paste("arg", i, sep
                                            = "")]])) {
              if(extra[[cov]][[paste("arg", i, sep = "")]] == 
                 "") {
                arg <- NULL
              } else {
                arg <- extra[[cov]][[paste("arg", i, sep = 
                                             "")]]
              }
            } else {
              arg <- eval(parse(text = paste("arg", i, sep
                                             = "")))
            }
            if(is.na(pmatch("I((",sm)))
              tmp.scope <- c(tmp.scope, paste(sm, "(", cov, 
                                              if(is.null(arg)) ")" else paste(",", arg, ")", 
                                                                              sep = "")))
            else
              tmp.scope <- c(tmp.scope,sm)
            
          }
        }
        tmp.scope <- eval(parse(text = paste("~", paste(tmp.scope, collapse = "+"
        ))))
        step.list[[cov]] <- tmp.scope
      }
    }
    step.list
  }
