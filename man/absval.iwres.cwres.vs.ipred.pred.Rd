% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/absval.iwres.cwres.vs.ipred.pred.R,
%   R/absval.iwres.wres.vs.ipred.pred.R
\name{absval.iwres.cwres.vs.ipred.pred}
\alias{absval.iwres.cwres.vs.ipred.pred}
\alias{absval.iwres.wres.vs.ipred.pred}
\title{Absolute population weighted residuals vs population predictions, and
absolute individual weighted residuals vs individual predictions, for Xpose
4}
\usage{
absval.iwres.cwres.vs.ipred.pred(object, main = "Default", ...)

absval.iwres.wres.vs.ipred.pred(object, main = "Default", ...)
}
\arguments{
\item{object}{An xpose.data object.}

\item{main}{The title of the plot.  If \code{"Default"} then a default title
is plotted. Otherwise the value should be a string like \code{"my title"} or
\code{NULL} for no plot title.}

\item{\dots}{Other arguments passed to \code{link{xpose.plot.default}}.}
}
\value{
Returns a compound plot.
}
\description{
This is a matrix plot of absolute population weighted residuals (|CWRES|) vs
population predictions (PRED) and absolute individual weighted residuals
(|IWRES|) vs individual predictions (IPRED), a specific function in Xpose 4.
It is a wrapper encapsulating arguments to the \code{absval.cwres.vs.pred}
and \code{absval.iwres.vs.ipred} functions.
}
\details{
The plots created by the \code{absval.wres.vs.pred} and
\code{absval.iwres.vs.ipred} functions are presented side by side for
comparison.

A wide array of extra options controlling xyplots are available. See
\code{\link{xpose.plot.default}} for details.
}
\section{Functions}{
\itemize{
\item \code{absval.iwres.wres.vs.ipred.pred()}: absolute population weighted residuals (|WRES|) vs
population predictions (PRED) and absolute individual weighted residuals
(|IWRES|) vs individual predictions (IPRED)

}}
\examples{

## Here we load the example xpose database 
xpdb <- simpraz.xpdb

## A vanilla plot
absval.iwres.wres.vs.ipred.pred(xpdb)
absval.iwres.cwres.vs.ipred.pred(xpdb)

## Custom colours and symbols
absval.iwres.cwres.vs.ipred.pred(xpdb, cex=0.6, pch=8, col=1)

}
\seealso{
\code{\link{absval.wres.vs.pred}},
\code{\link{absval.iwres.vs.ipred}}, \code{\link{xpose.plot.default}},
\code{\link{xpose.panel.default}}, \code{\link[lattice]{xyplot}},
\code{\link{xpose.prefs-class}}, \code{\link{xpose.data-class}}

Other specific functions: 
\code{\link{absval.cwres.vs.cov.bw}()},
\code{\link{absval.cwres.vs.pred}()},
\code{\link{absval.cwres.vs.pred.by.cov}()},
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
\code{\link{gof}()},
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

Other specific functions: 
\code{\link{absval.cwres.vs.cov.bw}()},
\code{\link{absval.cwres.vs.pred}()},
\code{\link{absval.cwres.vs.pred.by.cov}()},
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
\code{\link{gof}()},
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
E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins
}
\concept{specific functions}
