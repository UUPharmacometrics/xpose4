% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gof.R
\name{gof}
\alias{gof}
\alias{gofSetup}
\alias{xpPage}
\title{Structured goodness of fit diagnostics.}
\usage{
gof(
  runno = NULL,
  save = FALSE,
  onefile = FALSE,
  saveType = "pdf",
  pageWidth = 7.6,
  pageHeight = 4.9,
  structural = TRUE,
  residual = TRUE,
  covariate = FALSE,
  iiv = FALSE,
  iov = FALSE,
  all = FALSE,
  myTrace = xpPage
)
}
\arguments{
\item{runno}{The run number fo Xpose to identify the appropriate files to
read. In addition \code{runno} is used to construct the file name to save
plots in. \code{runno} can also be \code{NULL} for cases in which the
function is used for non-Xpose based code.}

\item{save}{Logical. \code{TRUE} if the plot(s) is to be saved in a file.
\code{FALSE} if the plot(s) is to be displayed on screen. The plot(s) will
be saved in a file named with the function name followed by the word 'run',
the run number, an order number followed by a file name extension
appropriate for the selected \code{saveType}. For example 'gofrun1-01.pdf'
for the first plot file created by a script called \code{gof} based on
output from run 1 and \code{saveType='pdf'}.}

\item{onefile}{Logical. \code{TRUE} if plots are to be save in a single file
and \code{FALSE} if each plot should be saved as a separate file. In the
latter case, each file will be have an incremented order number (01-99).}

\item{saveType}{The type of graphics file to produce if \code{save=TRUE}.
Allowed values are 'pdf' (default), 'wmf' (only Windows) and 'png'.}

\item{pageWidth}{The width of the graphics device in inches.}

\item{pageHeight}{The height of the graphics device in inches.}

\item{structural}{Logical. \code{TRUE} if the code in the structural model
section (see below) should be executed and \code{FALSE} if not.}

\item{residual}{Logical. \code{TRUE} if the code in the residual model
section (see below) should be executed and \code{FALSE} if not.}

\item{covariate}{Logical. \code{TRUE} if the code in the covariate model
section (see below) should be executed and \code{FALSE} if not.}

\item{iiv}{Logical. \code{TRUE} if the code in the IIV model section (see
below) should be executed and \code{FALSE} if not.}

\item{iov}{Logical. \code{TRUE} if the code in the IOV model section (see
below) should be executed and \code{FALSE} if not.}

\item{all}{Logical. \code{TRUE} if the code in all sections (see below)
should be executed.}

\item{myTrace}{\code{NULL} or the name of a function. The value of
\code{myTrace} can used with the lattice \code{page=} argument to annotate
plots for traceability.}
}
\value{
Does not return anything unless the user specify a return value.
}
\description{
This is a template function for creating structured goodness of fit
diagnostics using the functions in the Xpose specific library.
}
\details{
The \code{gof} function is provided as a template to facilitate the
(structured) use of the functions in the Xpose specific library. Xpose
specific is extensively described in the 'Xpose Bestiary'.

The function can be renamed so that multiple scripts can be used in
parallel.

The function is set up to make it easy to display plots on screen as well as
to save them in files. In the latter case, plots are save in a sub-directory
called 'Plots'.

The arguments \code{structural}, \code{residual}, \code{covariate},
\code{iiv}, \code{iov} and \code{all} are just "switches" to different parts
of the code (if-blocks). These blocks can be removed or the default values
of the arguments changed to better suit the needs of the user.

It is also possible to add tracing information to the produced plots. This
is done via the \code{myTrace} argument. A non-NULL value should be a
function that returns a \code{panel.text} object. The default is the
\code{xpPage} function that will put a string concatenated from the device
name, function name, working directory and date, in small, faint grey, font
at the bottom of each graph page. Note that the user need to add
\code{page=myTrace} as an argument to the Xpose functions for this to have
an effect.

The function calls a support function called \code{gofSetup}, which is
responsible for setting up the graphics device and determining the file
names for saved graphs.
}
\examples{

\dontrun{
## This is an example of how the function may be setup by a user.

library(xpose4)
mygof <- gof
fix(mygof)

myggof <- function (runno = NULL, save = FALSE, onefile = FALSE, saveType = "pdf", 
         pageWidth = 7.6, pageHeight = 4.9, structural = TRUE, residual = TRUE, 
         covariate = FALSE, iiv = FALSE, iov = FALSE, all = FALSE, myTrace=xpPage) {

         gofSetup(runno, save, onefile, saveType, pageWidth, pageHeight)
         xpdb <- xpose.data(runno)

         if (structural || all) {
          xplot <- dv.vs.pred.ipred(xpdb, page = myPage)
          print(xplot)
         }
         if (residual || all) {
          xplot <- absval.wres.vs.pred(xpdb, page = myPage)
          print(xplot)
         }
         if (covariate || all) {
         }
         if (iiv || all) {
         }
         if (iov || all) {
         }
         if (save) dev.off()
    invisible()
  }

## The function can then be execute, e.g.:
mygof(1)

}

}
\seealso{
\code{\link[xpose4]{xpose4-package}}

Other generic functions: 
\code{\link{xpose.multiple.plot}},
\code{\link{xpose4-package}}

Other specific functions: 
\code{\link{absval.cwres.vs.cov.bw}()},
\code{\link{absval.cwres.vs.pred}()},
\code{\link{absval.cwres.vs.pred.by.cov}()},
\code{\link{absval.iwres.cwres.vs.ipred.pred}()},
\code{\link{absval.iwres.vs.cov.bw}()},
\code{\link{absval.iwres.vs.idv}()},
\code{\link{absval.iwres.vs.ipred}()},
\code{\link{absval.iwres.vs.ipred.by.cov}()},
\code{\link{absval.iwres.vs.pred}()},
\code{\link{absval.wres.vs.cov.bw}()},
\code{\link{absval.wres.vs.idv}()},
\code{\link{absval.wres.vs.pred}()},
\code{\link{absval.wres.vs.pred.by.cov}()},
\code{\link{absval_delta_vs_cov_model_comp}},
\code{\link{addit.gof}()},
\code{\link{autocorr.cwres}()},
\code{\link{autocorr.iwres}()},
\code{\link{autocorr.wres}()},
\code{\link{basic.gof}()},
\code{\link{basic.model.comp}()},
\code{\link{cat.dv.vs.idv.sb}()},
\code{\link{cat.pc}()},
\code{\link{cov.splom}()},
\code{\link{cwres.dist.hist}()},
\code{\link{cwres.dist.qq}()},
\code{\link{cwres.vs.cov}()},
\code{\link{cwres.vs.idv}()},
\code{\link{cwres.vs.idv.bw}()},
\code{\link{cwres.vs.pred}()},
\code{\link{cwres.vs.pred.bw}()},
\code{\link{cwres.wres.vs.idv}()},
\code{\link{cwres.wres.vs.pred}()},
\code{\link{dOFV.vs.cov}()},
\code{\link{dOFV.vs.id}()},
\code{\link{dOFV1.vs.dOFV2}()},
\code{\link{data.checkout}()},
\code{\link{dv.preds.vs.idv}()},
\code{\link{dv.vs.idv}()},
\code{\link{dv.vs.ipred}()},
\code{\link{dv.vs.ipred.by.cov}()},
\code{\link{dv.vs.ipred.by.idv}()},
\code{\link{dv.vs.pred}()},
\code{\link{dv.vs.pred.by.cov}()},
\code{\link{dv.vs.pred.by.idv}()},
\code{\link{dv.vs.pred.ipred}()},
\code{\link{ind.plots}()},
\code{\link{ind.plots.cwres.hist}()},
\code{\link{ind.plots.cwres.qq}()},
\code{\link{ipred.vs.idv}()},
\code{\link{iwres.dist.hist}()},
\code{\link{iwres.dist.qq}()},
\code{\link{iwres.vs.idv}()},
\code{\link{kaplan.plot}()},
\code{\link{par_cov_hist}},
\code{\link{par_cov_qq}},
\code{\link{parm.vs.cov}()},
\code{\link{parm.vs.parm}()},
\code{\link{pred.vs.idv}()},
\code{\link{ranpar.vs.cov}()},
\code{\link{runsum}()},
\code{\link{wres.dist.hist}()},
\code{\link{wres.dist.qq}()},
\code{\link{wres.vs.idv}()},
\code{\link{wres.vs.idv.bw}()},
\code{\link{wres.vs.pred}()},
\code{\link{wres.vs.pred.bw}()},
\code{\link{xpose.VPC}()},
\code{\link{xpose.VPC.both}()},
\code{\link{xpose.VPC.categorical}()},
\code{\link{xpose4-package}}
}
\author{
E. Niclas Jonsson, Mats Karlsson and Andrew Hooker
}
\concept{generic functions}
\concept{specific functions}
\keyword{methods}
