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
\name{xp.incl.index.cov}
\alias{xp.incl.index.cov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot of inclusion index of covariates.
}
\description{
  Covariate inclusion indices show the correlation in inclusion of a
  covariate in the final model in a bootgam or bootscm.
}
\usage{
xp.incl.index.cov(bootgam.obj = NULL, boot.type = NULL, main = NULL, xlb
= "Index", ylb = "Covariate", add.ci = FALSE, incl.range = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bootgam.obj}{
    The bootgam or bootscm object.
  }
  \item{boot.type}{
    Either "bootgam" or "bootscm". Default is NULL, which means the user
    will be asked to make a choice.
  }
  \item{main}{
    Plot title.
  }
  \item{xlb}{
    Label for the x-axis.
  }
  \item{ylb}{
    Label for th y-axis.
  }
  \item{add.ci}{
    Add a confidence interval to the plotted data.
  }
  \item{incl.range}{
    Included range
  }

  \item{\dots}{
    Additional plotting information.
  }
}
\value{
  A lattice plot object is returned.
}
\author{
  Ron Keizer
}
\keyword{ ~bootgam }
\keyword{ ~bootscm }
