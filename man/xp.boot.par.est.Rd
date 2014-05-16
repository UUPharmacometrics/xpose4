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
\name{xp.boot.par.est}
\alias{xp.boot.par.est}
\title{
Compare parameter estimates for covariate coefficients
}
\description{
  This function creates a plot of the estimates for covariate
  coefficients, obtained from the first step (univariate testing) in
  each scm performed in the bootscm. When normalized for their standard
  deviation, these plots can be used to compare the strength of the
  covariate relationship. Coloring is based on the covariate being
  included in the final model (blue) not being included (red).

  Optionally, estimated bias is plotted in the graph (as text). Bias is
  also shown by the difference in mean of parameter estimates when the
  covariate is included (blue diamond), as opposed to the mean of all
  parameter estimates (grey diamond)

  Note: For dichotomous covariates, the default PsN implementation is to
  use the most common covariate value as base, while the effect of the
  other value, is estimated by a theta. Xpose (bootscm.import) however
  recalculates the estimated parameters, to the parametrization in which
  the lowest value of the dichotomous covariate is the base (e.g. 0),
  and the estimated THETA denotes the proportional change, when the
  covariate has the other value (e.g. 1).
}
\usage{
xp.boot.par.est(bootgam.obj = NULL, sd.norm = TRUE, by.cov.type = FALSE,
abs.values = FALSE, show.data = TRUE, show.means = TRUE, show.bias =
TRUE, dotpch = c(1,19), labels= NULL, pch.mean="|", xlab=NULL,
ylab=NULL, col = c(rgb(.8, .5, .5), rgb(.2,
.2, .7), rgb(.2,.2,.7), rgb(.6,.6,.6)), ...)
}
\arguments{
  \item{bootgam.obj}{
    The object created using bootscm.import(), which hold the data for
    plotting.}
  \item{sd.norm}{ Perform normalization of the covariate coefficients
    (default is TRUE). When TRUE, the estimated covariate coefficients
    will be multiplied by the standard deviation of the specific
    covariate (both for continuous and categorical covariates).
  }
  \item{by.cov.type}{
    Split the plot for continuous and dichotomous covariates. 
    Default is FALSE.
  }
  \item{abs.values}{
    Show the covariate coefficient in absolute values. Default is FALSE.
  }
  \item{show.data}{
    Show the actual covariate coefficients in the plot. Default is TRUE.
  }
  \item{show.means}{
    Show the means of included covariates (blue) and all covariates
    (grey) in the plot. Default is TRUE.
  }
  \item{show.bias}{
    Show estimated bias as text in the plot. Default is TRUE.
  }
  \item{dotpch}{
    The character used for plotting.
  }
  \item{labels}{Custom labels for the parameter-covariate relationships,
    (character vector)}
  \item{xlab}{Custom x-axis label}
  \item{ylab}{Custom y-axis label}
  \item{pch.mean}{
    The character used for plotting the mean.
  }
  \item{col}{
    The color scheme.
  }
  \item{\dots}{
    Additional plotting arguments may be passed to this function.
  }
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{
  No value returned.
}
\author{
  Ron Keizer
}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
  xp.boot.par.est()
}
\keyword{ ~bootscm }
