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
\name{xp.boot.par.est.corr}
\alias{xp.boot.par.est.corr}
\title{
  Correlations between covariate coefficients
}
\description{
  This function creates a plot showing the correlations in estimates for covariate
  coeffecients, obtained from the first step (univariate testing) in
  each scm performed in the bootscm.
}
\usage{
xp.boot.par.est.corr(bootgam.obj = NULL, sd.norm = TRUE,  by.cov.type =
FALSE, cov.plot = NULL, ask.covs = FALSE, dotpch=19, col = rgb(.2, .2,
.9, .75), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bootgam.obj}{
    The object created using bootscm.import(), which hold the data for
    plotting.
  }
  \item{sd.norm}{ Perform normalization of the covariate coefficients
    (default is TRUE). When TRUE, the estimated covariate coefficients
    will be multiplied by the standard deviation of the specific
    covariate (both for continuous and categorical covariates). }

  \item{by.cov.type}{
    Split the plot for continuous and dichotomous covariates. Default is FALSE.
  }

  \item{cov.plot}{ A character vector which lists the covariates to
    include in the plot. If none are specified (NULL), all covariate
    coefficients will be included in the plot.  }

  \item{ask.covs}{
    Ask the user which covariates to include in the plot. Default is FALSE.
  }
  \item{dotpch}{
    The character used for plotting.
  }
  \item{col}{
    The colors used for plotting.
  }
  \item{\dots}{
    Additional plotting arguments may be passed to this function.
  }
}
\value{
  No value returned.
}
\author{
  Ron Keizer
}
\examples{
\dontrun{
xp.boot.par.est.corr(current.bootscm, sd.norm = TRUE,
                          cov.plot = c("CLSEX", "VSEX", "CLWT"))

}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~bootscm }
