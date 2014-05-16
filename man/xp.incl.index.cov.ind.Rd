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
\name{xp.incl.index.cov.ind}
\alias{xp.incl.index.cov.ind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Individual inclusion index
}

\description{ This function will generate a plot of individual inclusion indexes for a
  specific covariate, which can be used to identify influential
  individuals for inclusion of that covariate. The index for an individual is calculated as
  the observed number of inclusions of that individual when the specific
  covariate was included minus the expected number of inclusions (based
  on the total bootstrap inclusions), divided by expected.}

\usage{
xp.incl.index.cov.ind(bootgam.obj = NULL, boot.type=NULL, cov.name = NULL, 
                      main = NULL, ylb = "ID", 
                      xlb = "Individual inclusion index", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{bootgam.obj}{
    A bootgam or bootscm object.
  }
  \item{boot.type}{
    Either "bootgam" or "bootscm". Default is NULL, which means the user
    will be asked to make a choice.
  }
  \item{cov.name}{
    The name of the covariate for which to create the plot.
  }
  \item{main}{
    The title of the plot.
  }
  \item{xlb}{
    The label for the x-axis.
  }
  \item{ylb}{
    The label for the y-axis.
  }
  \item{\dots}{
    Additional plotting parameters.
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
