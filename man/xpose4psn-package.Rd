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

\name{xpose4psn-package}
\alias{xpose4psn-package}
\title{Xpose PsN post-processing functions}
\docType{package}
\description{
Xpose functions for post-processing PsN output.
}
\details{ 

These functions are the interface between Xpose and PsN, i.e. they do not post-process NONMEM output but rather PsN output.

\itemize{

\item bootstrap
\itemize{
\item \code{\link{boot.hist}}
}

\item boot_scm
\itemize{
\item \code{\link{bootscm.import}}
\item \code{\link{bootgam.print}}
\item \code{\link{xp.inc.prob}}
\item \code{\link{xp.inc.prob.comb.2}}
\item \code{\link{xp.distr.mod.size}}
\item \code{\link{xp.inc.stab.cov}}
\item \code{\link{xp.incl.index.cov}}
\item \code{\link{xp.incl.index.cov.ind}}
\item \code{\link{xp.incl.index.cov.comp}}
\item \code{\link{xp.boot.par.est}}
\item \code{\link{xp.boot.par.est.corr}}
\item \code{\link{xp.dofv.plot}}
}

\item randtest
\itemize{
\item \code{\link{randtest.hist}}
}

\item vpc and npc
\itemize{
\item \code{\link{cat.pc}}
\item \code{\link{kaplan.plot}}
\item \code{\link{npc.coverage}}
\item \code{\link{xpose.VPC}}
\item \code{\link{xpose.VPC.both}}
\item \code{\link{xpose.VPC.categorical}}
}

}
}

\author{E. Niclas Jonsson, Mats Karlsson, Justin Wilkins and Andrew Hooker}
\keyword{methods}
\keyword{package}
