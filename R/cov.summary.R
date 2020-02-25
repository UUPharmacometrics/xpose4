#' Summarize individual parameter values and covariates
#' 
#' These functions produce tables, printed to the screen, summarizing the
#' individual parameter values or covariates from a dataset in Xpose 4.
#' 
#' 
#' @param object An xpose.data object.
#' @param onlyfirst Logical value indicating if only the first row per
#' individual is included in the plot.
#' @param inclZeroWRES Logical value indicating whether rows with WRES=0 are
#' included in the plot. The default is FALSE.
#' @param out.file Where the results should be output to.  Can be ".screen",
#' ".ask", ".graph" or a filename in quotes.
#' @param subset A string giving the subset expression to be applied to the
#' data before plotting. See \code{\link{xsubset}}.
#' @param main The title of the plot.  If \code{"Default"} then a default title
#' is plotted. Otherwise the value should be a string like \code{"my title"} or
#' \code{NULL} for no plot title.  
#' @param fill The color to fill the boxes in the table if the table is printed
#' to ".graph"
#' @param values.to.use Which values should be summarized
#' @param value.name The name of the values
#' @param max.plots.per.page Maximum plots per page.
#' @param \dots Other arguments passed to \code{Data} and \code{SData}.
#' @return Returned is the matrix of values from the table. \code{parm.summary}
#' and \code{cov.summary} produce summaries of parameters and covariates,
#' respectively. \code{parm.summary} produces less attractive output but
#' supports mirror functionality.
#' 
#' \code{parm.summary} and \code{cov.summary} utilize
#' \code{\link[Hmisc]{print.char.matrix}} to print the information to the
#' screen.
#' @author Andrew Hooker & Justin Wilkins
#' @seealso \code{\link{Data}}, \code{\link{SData}},
#' \code{\link{xpose.data-class}}, \code{\link[Hmisc]{print.char.matrix}}
#' @keywords methods
#' @examples
#' 
#' parm.summary(simpraz.xpdb)
#' 
#' @name  par_cov_summary
#' @family data functions 
NULL

#' @describeIn  par_cov_summary Covariate summary
#' @export
"cov.summary" <- function(object,
                          onlyfirst=TRUE,
                          subset=xsubset(object),
                          inclZeroWRES=FALSE,
                          out.file=".screen", # can be ".ask" ".graph" or a file name,
                          #out.file.sep=",",
                          main="Default",
                          fill = "gray",
                          values.to.use=xvardef("covariates",object),
                          value.name="Covariate",
                          ...){

  
  return(parm.summary(object,
                     onlyfirst=onlyfirst,
                     subset=subset,
                     out.file=out.file,
                     main=main,
                     fill=fill,
                     values.to.use=values.to.use,
                     value.name=value.name,
                     inclZeroWRES=inclZeroWRES,
                     ...) )

}
