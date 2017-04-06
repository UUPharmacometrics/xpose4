#' Print an Xpose multiple plot object.
#' 
#' Print an Xpose multiple plot object, which is the output from the function
#' \code{\link{xpose.multiple.plot}}.
#' 
#' Print method for a plot class.
#' 
#' @param x Output object from the function \code{\link{xpose.multiple.plot}}.
#' @param \dots Additional options passed to function.
#' @author Niclas Jonsson and Andrew C. Hooker
#' @seealso \code{\link{xpose.multiple.plot}}.
#' @method print xpose.multiple.plot
#' @export 
print.xpose.multiple.plot <- function(x,...) {
  
  xpose.multiple.plot.default(x@plotList,
                              plotTitle=x@plotTitle,
                              prompt=x@prompt,
                              new.first.window=x@new.first.window,
                              max.plots.per.page=x@max.plots.per.page,
                              title=x@title,               
                              mirror=x@mirror,
                              bql.layout=x@bql.layout,
                              ...)
  
  invisible()
  
}


setMethod("show","xpose.multiple.plot",function(object) print(x=object))

# #' Show an Xpose multiple plot object.
# #'
# #' Show an Xpose multiple plot object, which is the output from the function
# #' \code{\link{xpose.multiple.plot}}.
# #'
# #' Show method for a plot class.
# #'
# #' @param x Output object from the function \code{\link{xpose.multiple.plot}}.
# #' @param \dots Additional options passed to function.
# #' @author Niclas Jonsson and Andrew C. Hooker
# #' @seealso \code{\link{xpose.multiple.plot}}.
# #' @method show xpose.multiple.plot
# #' @export
# show.xpose.multiple.plot <- function(x,...){
#   xpose.multiple.plot.default(x@plotList,
#                               plotTitle=x@plotTitle,
#                               prompt=x@prompt,
#                               new.first.window=x@new.first.window,
#                               max.plots.per.page=x@max.plots.per.page,
#                               title=x@title,
#                               mirror=x@mirror,
#                               bql.layout=x@bql.layout,
#                               ...)
# 
#   invisible()
# }

