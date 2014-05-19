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

\name{classic}
\alias{add.modify.db.items.menu}
\alias{another.menu}
\alias{change.dispersion}
\alias{change.graphical.par}
\alias{change.medianNorm}
\alias{change.onlyfirst}
\alias{change.steppit}
\alias{change.use.ids}
\alias{change.xp.obj}
\alias{gam.change.weights}
\alias{covariate.model.menu}
\alias{cwres.menu}
\alias{data.checkout.menu}
\alias{documentation.menu}
\alias{gam.menu}
\alias{gam.settings.menu}
\alias{gof.menu}
\alias{list.gam.settings}
\alias{main.menu}
\alias{manage.db}
\alias{model.comparison.menu}
\alias{model.comparison.covariates.menu}
\alias{parameters.menu}
\alias{preferences.menu}
\alias{residual.diagnostics.menu}
\alias{struct.gof.menu}
\alias{vpc.npc.menu}
\alias{runsum.print}
\alias{structural.diagnostics.menu}
\alias{table.file.read.settings.menu}
%\alias{xpose4}
\title{Classic menu functions for Xpose 4}
\description{
  These functions drive the classic Xpose menu system.
}
\usage{
xpose4()
}

\details{
  The classic Xpose menu system is invoked by using the command \code{xpose4()}.
  With the exception of \code{xpose4}, these are internal Xpose functions, 
  not intended for direct use.
}
\value{
  Functions for the classic Xpose menu system.
}
\author{Andrew Hooker}

\examples{
\dontrun{
xpose4()
}
}
\keyword{methods}
