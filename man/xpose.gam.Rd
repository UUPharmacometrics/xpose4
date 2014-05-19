% Xpose 4
% An R-based population pharmacokinetic/
% pharmacodynamic model building aid for NONMEM.
% Copyright (C) 1998-2004 E. Niclas Jonsson and Mats Karlsson.
% Copyright (C) 2005-2008 Andrew C. Hooker, Justin J. Wilkins, 
% Mats O. Karlsson and E. Niclas Jonsson.
% Copyright (C) 2009-2010 Andrew C. Hooker, Mats O. Karlsson and 
% E. Niclas Jonsson.

% This file is a part of Xpose 4.
% Xpose 4 is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public License
% as published by the Free Software Foundation, either version 3
% of the License, or (at your option) any later version.

% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public License
% along with this program.  A copy can be cound in the R installation
% directory under \share\licenses. If not, see http://www.gnu.org/licenses/.

\name{xpose.gam}
\alias{xpose.gam}
\alias{xp.get.disp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Stepwise GAM search for covariates on a parameter (Xpose 4)}
\description{
Function takes an Xpose object and performs a generalized additive model
(GAM) stepwise search for covariates on a single model parameter.
}
\usage{
xpose.gam(object,
           parnam = xvardef("parms", object)[1],
           covnams = xvardef("covariates", object),
           trace = TRUE,
           scope = NULL, 
	         disp = object@Prefs@Gam.prefs$disp,
           start.mod = object@Prefs@Gam.prefs$start.mod,
           family = "gaussian",
           wts.data =object@Data.firstonly,
           wts.col=NULL,
           steppit = object@Prefs@Gam.prefs$steppit,
           subset = xsubset(object),
           onlyfirst = object@Prefs@Gam.prefs$onlyfirst,
           medianNorm = object@Prefs@Gam.prefs$medianNorm,
           nmods = object@Prefs@Gam.prefs$nmods,
           smoother1 = object@Prefs@Gam.prefs$smoother1,
           smoother2 = object@Prefs@Gam.prefs$smoother2,
           smoother3 = object@Prefs@Gam.prefs$smoother3,
           smoother4 = object@Prefs@Gam.prefs$smoother4,
           arg1 = object@Prefs@Gam.prefs$arg1,
           arg2 = object@Prefs@Gam.prefs$arg2,
           arg3 = object@Prefs@Gam.prefs$arg3,
           arg4 = object@Prefs@Gam.prefs$arg4,
           excl1 = object@Prefs@Gam.prefs$excl1,
           excl2 = object@Prefs@Gam.prefs$excl2,
           excl3 = object@Prefs@Gam.prefs$excl3,
           excl4 = object@Prefs@Gam.prefs$excl4,
           extra = object@Prefs@Gam.prefs$extra,
           ...)
           
xp.get.disp(gamdata,
           parnam,
           covnams,
           family = "gaussian",
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An xpose.data object. }
  \item{parnam}{ONE (and only one) model parameter name.}
  \item{covnams}{Covariate names to test on parameter.}
  \item{trace}{TRUE if you want GAM output to screen.}
  \item{scope}{Scope of the GAM search.}
  \item{disp}{If dispersion should be used in the GAM object.}
  \item{start.mod}{Starting model.}
  \item{family}{Assumption for the parameter distribution. }
  \item{wts.data}{Weights on the least squares fitting of parameter
    vs. covariate. Often one can use the variances of the
    individual parameter values as weights. This data frame must have
    column with name ID and any subset variable as well as the variable
    defined by the \code{wts.col}.}
  \item{wts.col}{Which column in the \code{wts.data} to use.}
  \item{steppit}{TRUE for stepwise search, false for no search.}
  \item{subset}{Subset on data.}
  \item{onlyfirst}{TRUE if only the first row of each individual's data is
    to be used. }
  \item{medianNorm}{Normalize to the median of parameter and covariates.}
  \item{nmods}{Number of models to examine.}
  \item{smoother1}{Smoother for each model.}
  \item{smoother2}{Smoother for each model.}
  \item{smoother3}{Smoother for each model.}
  \item{smoother4}{Smoother for each model.}
  \item{arg1}{Argument for model 1.}
  \item{arg2}{Argument for model 2. }
  \item{arg3}{Argument for model 3. }
  \item{arg4}{Argument for model 4. }
  \item{excl1}{Covariate exclusion from model 1. }
  \item{excl2}{Covariate exclusion from model 2.}
  \item{excl3}{Covariate exclusion from model 3.}
  \item{excl4}{Covariate exclusion from model 4.}
  \item{extra}{Extra exclusion criteria.}
  \item{gamdata}{Data for the GAM. A data frame.}
  \item{\dots}{Used to pass arguments to more basic functions.}
}
\details{
  \code{xpose.gam} performs a stepwise GAM search for influential covariates. 
  \code{xp.get.disp} is a helper function for calculating dispersion, and
  is not intended to be used by itself.
}
\value{
  Returned is a \code{\link[gam]{step.gam}} object
}
\author{E. Niclas Jonsson, Mats Karlsson, Andrew Hooker & Justin Wilkins}

\seealso{
\code{\link{xp.gam}},
\code{\link[gam]{step.gam}}
}
\examples{
\dontrun{
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb <- xpose.data(5)

## Here we load the example xpose database 
data(simpraz.xpdb)
xpdb <- simpraz.xpdb

xpose.gam(xpdb, parnam="CL", covnams = xvardef("covariates", xpdb))
}}
\keyword{methods}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

