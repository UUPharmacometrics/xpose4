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
\name{xp.distr.mod.size}
\alias{xp.distr.mod.size}
\title{
  Plot of model size distribution for a bootgam or bootscm
}
\description{
  This function creates a kernel smoothed plot of the number of
  covariates included in the final model in each gam/scm in the
  bootgam/bootscm procedure.
}
\usage{
xp.distr.mod.size(bootgam.obj = NULL, boot.type = NULL, 
                  main = NULL, bw = 0.5, xlb = NULL, ...)
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
  \item{bw}{
    The smoothing bandwidth to be used for the kernel.
  }
  \item{xlb}{
    The x-axis label.
  }
  \item{\dots}{
    Additional plotting parameter may be passed to this function.
  }
}
\value{
  A lattice plot object will be returned.
}
\author{
  Ron Keizer
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~bootgam }
\keyword{ ~bootscm }
