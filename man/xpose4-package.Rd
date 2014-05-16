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

\name{xpose4-package}
\alias{xpose4-package}
\title{Xpose 4 Package}
\docType{package}
\description{
 Xpose is an R-based model building aid for population analysis using
  NONMEM. It facilitates data set checkout, exploration and
  visualization, model diagnostics, candidate covariate identification
  and model comparison.
}
\details{ Xpose is an R library for post-processing of NONMEM output. It takes one or more standard NONMEM table files as input and generates graphs or other analyses. It is assumed that each NONMEM run can be uniquely identified by a run number (see section below for how to generate the appropriate input to Xpose). Xpose is implemented using the lattice graphics library.

The Xpose library can be divided up into five sub-sections: 

\code{\link{xpose4data-package}} Contains functions for managing the input data and manipulating the Xpose database.

\code{\link{xpose4generic-package}} Contains generic wrapper functions around the lattice functions. These funtions can be invoked by the user but require quite detailed instructions to generate the desired output.

\code{\link{xpose4specific-package}} These functions are single purpose functions that generate specific output given only the Xpose database as input. The behavior can to some extent be influenced by the user.

\code{\link{xpose4classic-package}}  Historically, Xpose has had a text based menu interface to make it simple for the user to invoke the Xpose specific functions. This interface is called Xpose Classic. Given the limitations a text based interface imposes, Xpose Classic is not very flexible but may still be useful for quick assessment of a model.

\code{\link{xpose4special-package}} These functions are the interface between Xpose and PsN, i.e. they do not post-process NONMEM output but rather PsN output.


 }

\section{How to make NONMEM generate input to Xpose}{ Xpose recognizes
NONMEM runs, and files associated to a particular run, though the run
number.  This is a number that is used in the name of NONMEM model
files, output files and table files.  The fundamental input to Xpose
is one or more NONMEM table file.  These table files should be named
as below followed by the run number, for example xptab1 for run number
1.  Xpose looks for files according to the following pattern, where *
is your run number:

\bold{sdtab*}  Standard table file, containing ID, IDV, DV, PRED, IPRED, WRES, IWRES, RES, IRES, etc.

\bold{patab*}	Parameter table, containing model parameters - THETAs, ETAs and EPSes

\bold{catab*}	Categorical covariates, e.g. SEX, RACE

\bold{cotab*}	Continuous covariates, e.g. WT, AGE

\bold{extra*, mutab*, mytab*, xptab*, cwtab*}	Other variables you might need to have available to Xpose

\bold{run*.mod}	Model specification file

\bold{run*.lst}	NONMEM output

Strictly, only one table file is needed for xpose (for example sdtab* or xptab*). 
However, using patab*, cotab*, catab* influence the way that Xpose interprets the data and are recommended to get  full benefit from Xpose.

You can use code similar to the following to generate the tables you need. 
NONMEM automatically appends DV, PRED, WRES and RES unless NOAPPEND is specified. 
Don't forget to leave at least one blank line at the end of the model specification file.

\code{$TABLE ID TIME IPRED IWRES EVID MDV NOPRINT ONEHEADER FILE=sdtab1}
\code{$TABLE ID CL V2 KA K SLP KENZ NOPRINT ONEHEADER FILE=patab1}
\code{$TABLE ID WT HT AGE BMI PKG NOPRINT ONEHEADER FILE=cotab1}
\code{$TABLE ID SEX SMOK ALC NOPRINT ONEHEADER FILE=catab1}
}

\section{Running Xpose}{

  Xpose can be run in two different ways, using the classic menu system
  and by using the command line in R.   
  
  \subsection{Running Xpose from the classic menu system}{ 
  Xpose can be run
  through a series of menu selections.  This method of running Xpose
  is useful for the beginning "Xposer" and for learning about the
  functionality available in Xpose.  The menu system does not allow
  for as much customization as the command line interface, nor does it
  allow for scripting.
  
  To run the classic Xpose interface, at the R command prompt,
  type:

  \code{library(xpose4)}
  \code{xpose4()}

  And follow the menu prompts.
}
\subsection{Running Xpose from the command line}{
  Each function in 
  Xpose  is  independently available from the command
  line, once the library is loaded. For example, you might do the
  following: 
  
  \code{library(xpose4)}
  \code{xpdb <- xpose.data(5)}
  \code{basic.gof(xpdb)} 
}
}
 
\section{Getting Help}{
  
  All Xpose 4.0 functions are documented within the R help system.
  For example, if you want to know more about DV vs PRED plots and their
  options, type at the R command line after loading Xpose:

  \code{?dv.vs.pred}

  You can also use the interactive help system in most R-interfaces: choose Help from the menu system, 
  then click on packages, then xpose4.
  
  The most interesting may be the specific functions \code{\link{xpose4specific-package}}.
  These are functions that are ready to use just by telling the functions
  which xpose database to use (i.e. \code{xpdb} after running the
  command \code{xpdb <- xpose.data(run.number)}).  
}
\references{\href{http://xpose.sf.net}{Xpose Homepage}, \href{http://psn.sf.net}{PsN Homepage}}
\author{E. Niclas Jonsson, Mats Karlsson, Justin Wilkins and Andrew Hooker}
\keyword{methods}
\keyword{package}
