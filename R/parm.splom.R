#' Plot scatterplot matrices of parameters, random parameters or covariates
#' 
#' These functions plot scatterplot matrices of parameters, random parameters
#' and covariates.
#' 
#' The parameters or covariates in the Xpose data object, as specified in
#' \code{object@Prefs@Xvardef$parms}, \code{object@Prefs@Xvardef$ranpar} or
#' \code{object@Prefs@Xvardef$covariates}, are plotted together as scatterplot
#' matrices. 
#' 
#' A wide array of extra options controlling scatterplot matrices are
#' available. See \code{\link{xpose.plot.splom}} for details.
#' 
#' To control the appearance of the labels and names in the scatterplot matrix
#' plots you can try \code{varname.cex=0.5} and \code{axis.text.cex=0.5} (this
#' changes the tick labels and the variable names to be half as large as
#' normal).
#' 
#' @param object An xpose.data object.
#' @param main A string giving the plot title or \code{NULL} if none.
#' @param varnames A vector of strings containing labels for the variables in
#' the scatterplot matrix.
#' @param onlyfirst Logical value indicating if only the first row per
#' individual is included in the plot.
#' @param lmline logical variable specifying whether a linear regression line
#' should be superimposed over an \code{\link[lattice]{xyplot}}. \code{NULL} ~
#' FALSE. (\code{y~x})
#' @param smooth A \code{NULL} value indicates that no superposed line should
#' be added to the graph. If \code{TRUE} then a smooth of the data will be
#' superimposed.
#' @param \dots Other arguments passed to \code{\link{xpose.plot.histogram}}.
#' @return Delivers a scatterplot matrix.
#' @author Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{xpose.plot.splom}}, \code{\link{xpose.panel.splom}},
#' \code{\link[lattice]{splom}}, \code{\link{xpose.data-class}},
#' \code{\link{xpose.prefs-class}}
#' @examples
#' 
#' ## Here we load the example xpose database 
#' xpdb <- simpraz.xpdb
#' 
#' ## A scatterplot matrix of parameters, grouped by sex
#' parm.splom(xpdb, groups="SEX")
#' 
#' ## A scatterplot matrix of ETAs, grouped by sex
#' ranpar.splom(xpdb, groups="SEX")
#' 
#' ## Covariate scatterplots, with text customization
#' cov.splom(xpdb, varname.cex=0.4, axis.text.cex=0.4, smooth=NULL, cex=0.4)
#' 
#' @name par_cov_splom
#' @family specific functions 
NULL

#' @describeIn  par_cov_splom A scatterplot matrix of parameters
#' @export 
parm.splom <- 
  function(object,  
           main = 
             xpose.multiple.plot.title(object=object,
                                       plot.text = "Scatterplot matrix of parameters",
                                       ...),
           varnames  = NULL,
           #xlb = NULL,
           #ylb = NULL,
           onlyfirst=TRUE,
           #inclZeroWRES=FALSE,
           #subset=xsubset(object),
           smooth = TRUE,
           lmline = NULL,
           #groups = NULL,
           #main.cex=NULL,
           ...) {
    
    if(any(is.null(xvardef("parms",object)))) {
      return(cat("Parameters are not defined in the current database!\n"))
    }
    
    if(is.null(varnames)) {
      varnames <- c()
      for (i in xvardef("parms", object)) {
        varnames <- c(varnames, xlabel(i, object))
      }
    }
    
    xplot <- xpose.plot.splom(xvardef("parms", object),
                              object,
                              varnames=varnames,
                              main = main,
                              onlyfirst = onlyfirst,
                              #inclZeroWRES = inclZeroWRES,
                              #subset = subset,
                              #groups = groups,
                              smooth = smooth,
                              lmline = lmline,
                              #ylb = ylb,
                              #xlb = xlb,
                              ...)
    
    return(xplot)
    
  }
