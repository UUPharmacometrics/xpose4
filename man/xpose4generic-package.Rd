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

\name{xpose4generic-package}
\alias{xpose4generic-package}
\title{Xpose generic multi-purpose functions}
\docType{package}
\description{
  Generic xpose functions 
}
\details{ 
These funtions can be invoked by the user but require quite detailed input to generate the desired output.  
used by \link{xpose4specific-package} to create output.

\itemize{
\item \code{\link{add.grid.table}}
\item \code{\link{add.grid.text}}
\item \code{\link{addid}}
\item \code{\link{categorical.table}}
\item \code{\link{compute.cwres}}
\item \code{\link{computePI}}
\item \code{\link{continuous.table}}
\item \code{\link{create.mirror}}
\item \code{\link{create.rand}}
\item \code{\link{create.strat.rand}}
\item \code{\link{create.xpose.plot.classes}}
\item \code{\link{eq.xpose}}
\item \code{\link{find.right.table}}
\item \code{\link{get.polygon.regions}}
\item \code{\link{get.refrunno}}
\item \code{\link{ind.cwres}}
\item \code{\link{is.cwres.readable.file}}
\item \code{\link{print.xpose.multiple.plot}}
\item \code{\link{read.cwres.data}}
\item \code{\link{setup.PPI}}
\item \code{\link{sqrtm}}
\item \code{\link{xpose.calculate.cwres}}
\item \code{\link{xpose.create.label}}
\item \code{\link{xpose.create.title}}
\item \code{\link{xpose.create.title.hist}}
\item \code{\link{xpose.create.title.text}}
\item \code{\link{xpose.dev.new}}
\item \code{\link{xpose.logTicks}}
\item \code{\link{xpose.multiple.plot}}
\item \code{\link{xpose.multiple.plot.default}}
\item \code{\link{xpose.multiple.plot.title}}
\item \code{\link{xpose.panel.bw}}
\item \code{\link{xpose.panel.default}}
\item \code{\link{xpose.panel.histogram}}
\item \code{\link{xpose.panel.qq}}
\item \code{\link{xpose.panel.splom}}
\item \code{\link{xpose.plot.bw}}
\item \code{\link{xpose.plot.default}}
\item \code{\link{xpose.plot.histogram}}
\item \code{\link{xpose.plot.qq}}
\item \code{\link{xpose.plot.splom}}
\item \code{\link{xpose.stack}}
\item \code{\link{xpose.string.print}}
\item \code{\link{xpose.xscale.components.log10}}
\item \code{\link{xpose.yscale.components.log10}}
}
}

\author{E. Niclas Jonsson, Mats Karlsson, Justin Wilkins and Andrew Hooker}
\keyword{methods}
\keyword{package}
