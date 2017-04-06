
#' Structured goodness of fit diagnostics.
#' 
#' This is a template function for creating structured goodness of fit
#' diagnostics using the functions in the Xpose specific library.
#' 
#' The \code{gof} function is provided as a template to facilitate the
#' (structured) use of the functions in the Xpose specific library. Xpose
#' specific is extensively descibed in the 'Xpose Bestiary'.
#' 
#' The function can be renamed so that multiple scripts can be used in
#' paralell.
#' 
#' The function is set up to make it easy to display plots on screen as well as
#' to save them in files. In the latter case, plots are save in a sub-directory
#' called 'Plots'.
#' 
#' The arguments \code{structural}, \code{residual}, \code{covariate},
#' \code{iiv}, \code{iov} and \code{all} are just "switches" to different parts
#' of the code (if-blocks). These blocks can be removed or the default values
#' of the arguments changed to better suit the needs of the user.
#' 
#' It is also possible to add tracing information to the produced plots. This
#' is done via the \code{myTrace} argument. A non-NULL value should be a
#' function that returns a \code{panel.text} object. The default is the
#' \code{xpPage} function that will put a string concatenated from the device
#' name, function name, working directory and date, in small, faint grey, font
#' at the bottom of each graph page. Note that the user need to add
#' \code{page=myTrace} as an argument to the Xpose functions for this to have
#' an effect.
#' 
#' The function calls a support function called \code{gofSetup}, which is
#' responsible for setting up the graphics device and determining the file
#' names for saved graphs.
#' 
#' @aliases gof gofSetup xpPage
#' @param runno The run number fo Xpose to identify the appropriate files to
#' read. In addition \code{runno} is used to construct the file name to save
#' plots in. \code{runno} can also be \code{NULL} for cases in which the
#' function is used for non-Xpose based code.
#' @param save Logical. \code{TRUE} if the plot(s) is to be saved in a file.
#' \code{FALSE} if the plot(s) is to be displayed on screen. The plot(s) will
#' be saved in a file named with the function name followed by the word 'run',
#' the run number, an order number followed by a file name extension
#' appropriate for the selected \code{saveType}. For example 'gofrun1-01.pdf'
#' for the first plot file created by a script called \code{gof} based on
#' output from run 1 and \code{saveType='pdf'}.
#' @param onefile Logical. \code{TRUE} if plots are to be save in a single file
#' and \code{FALSE} if each plot should be saved as a separate file. In the
#' latter case, each file will be have an incremented order number (01-99).
#' @param saveType The type of graphics file to produce if \code{save=TRUE}.
#' Allowed values are 'pdf' (default), 'wmf' (only Windows) and 'png'.
#' @param pageWidth The width of the graphics device in inches.
#' @param pageHeight The height of the graphics device in inches.
#' @param structural Logical. \code{TRUE} if the code in the structural model
#' section (see below) should be executed and \code{FALSE} if not.
#' @param residual Logical. \code{TRUE} if the code in the residual model
#' section (see below) should be executed and \code{FALSE} if not.
#' @param covariate Logical. \code{TRUE} if the code in the covariate model
#' section (see below) should be executed and \code{FALSE} if not.
#' @param iiv Logical. \code{TRUE} if the code in the IIV model section (see
#' below) should be executed and \code{FALSE} if not.
#' @param iov Logical. \code{TRUE} if the code in the IOV model section (see
#' below) should be executed and \code{FALSE} if not.
#' @param all Logical. \code{TRUE} if the code in all sections (see below)
#' should be executed.
#' @param myTrace \code{NULL} or the name of a function. The value of
#' \code{myTrace} can used with the lattice \code{page=} argument to annotate
#' plots for tracability.
#' @return Does not return anything unless the user specify a return value.
#' @author E. Niclas Jonsson, Mats Karlsson and Andrew Hooker
#' @seealso \code{\link[xpose4]{xpose4-package}}
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' ## This is an example of how the function may be setup by a user.
#' 
#' library(xpose4)
#' mygof <- gof
#' fix(mygof)
#' 
#' myggof <- function (runno = NULL, save = FALSE, onefile = FALSE, saveType = "pdf", 
#'          pageWidth = 7.6, pageHeight = 4.9, structural = TRUE, residual = TRUE, 
#'          covariate = FALSE, iiv = FALSE, iov = FALSE, all = FALSE, myTrace=xpPage) {
#' 
#'          gofSetup(runno, save, onefile, saveType, pageWidth, pageHeight)
#'          xpdb <- xpose.data(runno)
#' 
#'          if (structural || all) {
#'           xplot <- dv.vs.pred.ipred(xpdb, page = myPage)
#'           print(xplot)
#'          }
#'          if (residual || all) {
#'           xplot <- absval.wres.vs.pred(xpdb, page = myPage)
#'           print(xplot)
#'          }
#'          if (covariate || all) {
#'          }
#'          if (iiv || all) {
#'          }
#'          if (iov || all) {
#'          }
#'          if (save) dev.off()
#'     invisible()
#'   }
#' 
#' ## The function can then be execute, e.g.:
#' mygof(1)
#' 
#' }
#' 
#' @export gof
#' @family generic functions 
#' @family specific functions 
gof <- function(runno=NULL,save=FALSE,onefile=FALSE,
                saveType="pdf",pageWidth=7.6,pageHeight=4.9,
                structural = TRUE,residual=TRUE,covariate=FALSE,
                iiv=FALSE,iov=FALSE,all=FALSE,myTrace=xpPage) {

  ## This is a template function for creating goodness of fit plots using
  ## Xpose specific. Type ?gof at the R-prompt for more help.

#########################################################
## Start the graphics device and define save-filenames ##
## (probably no need to change this).                  ##
#########################################################
  gofSetup(runno,save,onefile,saveType,pageWidth,pageHeight)
 
#############################################
### Set up the data and create the graphs ###
#############################################

## Note! With lattice it is necessary to issue print(plotobject) to
## make the plot appear on the graphics device., e.g.
## xplot <- xyplot(DV~TIME,data)
## print(xplot)

  ## Read the data and do any modifications
  xpdb <- xpose.data(runno)

  ## Create structural model diagnostics
  if(structural || all) {

    xplot <- dv.vs.pred.ipred(xpdb,page=myTrace)
    print(xplot)
  }

  ## Create residual model diagnostics
  if(residual || all) {

    xplot <- absval.wres.vs.pred(xpdb,page=NULL)
    print(xplot)

  }

  ## Create covariate model diagnostics
  if(covariate || all) {

  }

  ## Create iiv model diagnostics
  if(iiv || all) {

  }

  ## Create iov model diagnostics
  if(iov || all) {

  }


#######################
### Finishing tasks ###
#######################

  ## Turn off device if a file device, i.e. save=TRUE
  if(save) dev.off()
  invisible()
}

