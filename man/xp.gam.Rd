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

\name{gam.functions}
\alias{xp.gam}
\alias{check.gamobj}
\alias{xp.akaike.plot}
\alias{xp.check.scope}
\alias{xp.cook}
\alias{xp.ind.stud.res}
\alias{xp.ind.inf.fit}
\alias{xp.ind.inf.terms}
\alias{xp.plot}
\alias{xp.scope3}
\alias{xp.summary}
\title{GAM functions for Xpose 4}
\description{
  These are functions for running the generalized additive model within
  Xpose 4, summarizing its results, and plotting them.
}
\usage{
xp.gam(object,
       parnam=xvardef("parms", object),
       covnams = xvardef("covariates", object),
       wts.col=NULL,
       ask.for.input=TRUE,
       overwrite=TRUE, 
       ...)

check.gamobj()

xp.akaike.plot(title = NULL,
           xlb = "Akaike value",
           ylb = "Models",
           gamobj=NULL,
           ...)
           
xp.check.scope(object,
           covnam = xvardef("covariates", object),
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

xp.cook(gam.object)
           
xp.ind.inf.fit(plot.ids = TRUE,
           idscex = 0.7,
           ptscex = 0.7,
           title = NULL,
           recur = FALSE,
           xlb = NULL,
           ylb = NULL,
           gamobj=NULL,
           ...)
           
xp.ind.inf.terms(xlb = NULL,
           ylb = NULL,
           plot.ids = TRUE,
           idscex = 0.7,
           ptscex = 0.7,
           prompt = TRUE,
           gamobj=NULL,
           ...)
           
xp.ind.stud.res(title = NULL,
           recur = FALSE,
           xlb = NULL,
           ylb = NULL,
           gamobj=NULL)
           
xp.plot(plot.ids = TRUE,
           idscex = 0.7,
           ptscex = 0.7,
           prompt = TRUE,
           gamobj=NULL,
           ...)
           
xp.summary(gamobj=NULL)

}
\arguments{
  \item{object}{An xpose.data object. }
  \item{title}{A text string indicating plot title. If \code{NULL}, left blank.}
  \item{xlb}{A text string indicating x-axis legend. If \code{NULL}, left blank.}
  \item{ylb}{A text string indicating y-axis legend. If \code{NULL}, left blank.}
  \item{covnam}{A list of covariate variables to use in the GAM search.}
  \item{wts.col}{Column in the object@Data.firstonly to use as weights
    on the parnam values.}
  \item{nmods}{The number of modelfits to use when setting GAM scope. The default 
    is 3.}
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
  \item{extra}{Scope parameter for the GAM.}
  \item{gam.object}{A GAM object (see \code{\link[gam]{gam}}.}
  \item{plot.ids}{Logical, specifies whether or not ID numbers should be displayed.}
  \item{idscex}{ID label size.}
  \item{ptscex}{Point size.}
  \item{recur}{If dispersion should be used in the GAM object.}
  \item{prompt}{Specifies whether or not the user should be prompted to press RETURN
    between plot pages. Default is TRUE.}
  \item{parnam}{The parameter to run the GAM on.}
  \item{covnams}{The covariates to test on the parnam}
  \item{ask.for.input}{Should the program ask for input from the user?
    Can be TRUE or FALSE.}
  \item{overwrite}{Should we overwrite the gam object stored in memory
    if it exists already.  Can be TRUE or FALSE.}
  \item{gamobj}{A GAM object to use in the plot. IF null then the user
    is asked to choose from a list of GAM objects in memory.}
  \item{\dots}{Other arguments passed to the GAM functions.}
}
\details{
  Forthcoming.
}
\value{
  A GAM fit.
}
\author{Niclas Jonsson & Andrew Hooker}
\seealso{
\code{\link[gam]{gam}},
\code{\link[lattice]{dotplot}}
}
\examples{
\dontrun{
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb <- xpose.data(5)

## Run a GAM
xp.gam(xpdb)

## Summarize GAM
xp.summary()

## An Akaike plot of the results
xp.akaike.plot()

## Studentized residuals
xp.ind.stud.res()

## Individual influence on GAM fit
xp.ind.inf.fit(plot.ids=xpdb@Prefs@Gam.prefs$plot.ids)

## Individual influence on GAM terms
xp.ind.inf.terms(plot.ids=xpdb@Prefs@Gam.prefs$plot.ids)
}}
\keyword{methods}
