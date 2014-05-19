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

\name{xpose4specific-package}
\alias{xpose4specific-package}
\title{Xpose specific functions for a single purpose}
\docType{package}
\description{
  A list of specific functions for tasks to be performed using an Xpose database
}
\details{ 
These functions are single purpose functions that generate specific output given only the Xpose database as input. The behavior can, to some extent, be influenced by the user.

\itemize{
\item \code{\link{absval.cwres.vs.cov.bw}}
\item \code{\link{absval.cwres.vs.pred}}
\item \code{\link{absval.cwres.vs.pred.by.cov}}
\item \code{\link{absval.dcwres.vs.cov.model.comp}}
\item \code{\link{absval.dipred.vs.cov.model.comp}}
\item \code{\link{absval.diwres.vs.cov.model.comp}}
\item \code{\link{absval.dpred.vs.cov.model.comp}}
\item \code{\link{absval.dwres.vs.cov.model.comp}}
\item \code{\link{absval.iwres.cwres.vs.ipred.pred}}
\item \code{\link{absval.iwres.vs.cov.bw}}
\item \code{\link{absval.iwres.vs.idv}}
\item \code{\link{absval.iwres.vs.ipred}}
\item \code{\link{absval.iwres.vs.ipred.by.cov}}
\item \code{\link{absval.iwres.vs.pred}}
\item \code{\link{absval.iwres.wres.vs.ipred.pred}}
\item \code{\link{absval.wres.vs.cov.bw}}
\item \code{\link{absval.wres.vs.idv}}
\item \code{\link{absval.wres.vs.pred}}
\item \code{\link{absval.wres.vs.pred.by.cov}}
\item \code{\link{add.model.comp}}
\item \code{\link{addit.gof}}
\item \code{\link{autocorr.cwres}}
\item \code{\link{autocorr.iwres}}
\item \code{\link{autocorr.wres}}
\item \code{\link{basic.gof}}
\item \code{\link{basic.model.comp}}
\item \code{\link{bootgam.print}}
\item \code{\link{cat.dv.vs.idv.sb}}
\item \code{\link{cat.pc}}
\item \code{\link{check.gamobj}}
\item \code{\link{cov.hist}}
\item \code{\link{cov.qq}}
\item \code{\link{cov.splom}}
\item \code{\link{cov.summary}}
\item \code{\link{cwres.dist.hist}}
\item \code{\link{cwres.dist.qq}}
\item \code{\link{cwres.vs.cov}}
\item \code{\link{cwres.vs.idv}}
\item \code{\link{cwres.vs.idv.bw}}
\item \code{\link{cwres.vs.pred}}
\item \code{\link{cwres.vs.pred.bw}}
\item \code{\link{cwres.wres.vs.idv}}
\item \code{\link{cwres.wres.vs.pred}}
\item \code{\link{data.checkout}}
\item \code{\link{dOFV.vs.cov}}
\item \code{\link{dOFV.vs.id}}
\item \code{\link{dOFV1.vs.dOFV2}}
\item \code{\link{dv.preds.vs.idv}}
\item \code{\link{dv.vs.idv}}
\item \code{\link{dv.vs.ipred}}
\item \code{\link{dv.vs.ipred.by.cov}}
\item \code{\link{dv.vs.ipred.by.idv}}
\item \code{\link{dv.vs.pred}}
\item \code{\link{dv.vs.pred.by.cov}}
\item \code{\link{dv.vs.pred.by.idv}}
\item \code{\link{dv.vs.pred.ipred}}
\item \code{\link{gof}}
\item \code{\link{gofSetup}}
\item \code{\link{ind.plots}}
\item \code{\link{ind.plots.cwres.hist}}
\item \code{\link{ind.plots.cwres.qq}}
\item \code{\link{ind.plots.wres.hist}}
\item \code{\link{ind.plots.wres.qq}}
\item \code{\link{ipred.vs.idv}}
\item \code{\link{iwres.dist.hist}}
\item \code{\link{iwres.dist.qq}}
\item \code{\link{iwres.vs.idv}}
\item \code{\link{kaplan.plot}}
%\item \code{\link{npc.coverage}}
\item \code{\link{parm.hist}}
\item \code{\link{parm.qq}}
\item \code{\link{parm.splom}}
\item \code{\link{parm.summary}}
\item \code{\link{parm.vs.cov}}
\item \code{\link{parm.vs.parm}}
\item \code{\link{pred.vs.idv}}
\item \code{\link{ranpar.hist}}
\item \code{\link{ranpar.qq}}
\item \code{\link{ranpar.splom}}
\item \code{\link{ranpar.vs.cov}}
\item \code{\link{runsum}}
\item \code{\link{tabulate.parameters}}
\item \code{\link{wres.dist.hist}}
\item \code{\link{wres.dist.qq}}
\item \code{\link{wres.vs.cov}}
\item \code{\link{wres.vs.idv}}
\item \code{\link{wres.vs.idv.bw}}
\item \code{\link{wres.vs.pred}}
\item \code{\link{wres.vs.pred.bw}}
\item \code{\link{xp.akaike.plot}}
\item \code{\link{xp.boot.par.est}}
\item \code{\link{xp.boot.par.est.corr}}
\item \code{\link{xp.check.scope}}
\item \code{\link{xp.cook}}
\item \code{\link{xp.distr.mod.size}}
\item \code{\link{xp.dofv.plot}}
\item \code{\link{xp.gam}}
\item \code{\link{xp.get.disp}}
\item \code{\link{xp.inc.prob}}
\item \code{\link{xp.inc.prob.comb.2}}
\item \code{\link{xp.inc.stab.cov}}
\item \code{\link{xp.incl.index.cov}}
\item \code{\link{xp.incl.index.cov.comp}}
\item \code{\link{xp.incl.index.cov.ind}}
\item \code{\link{xp.ind.inf.fit}}
\item \code{\link{xp.ind.inf.terms}}
\item \code{\link{xp.ind.stud.res}}
\item \code{\link{xp.plot}}
\item \code{\link{xp.scope3}}
\item \code{\link{xp.summary}}
\item \code{\link{xpose.gam}}
\item \code{\link{xpose.license.citation}}
\item \code{\link{xpose.VPC}}
\item \code{\link{xpose.VPC.both}}
\item \code{\link{xpose.VPC.categorical}}
\item \code{\link{xpPage}}
}
}
\author{E. Niclas Jonsson, Mats Karlsson, Justin Wilkins and Andrew Hooker}
\keyword{methods}
\keyword{package}
