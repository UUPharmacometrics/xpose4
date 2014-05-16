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

\name{xpose4data-package}
\alias{xpose4data-package}
\title{Xpose 4 data}
\docType{package}
\description{
  A package used to read in NONMEM table files to Xpose and to create
  an Xpose database
}
\details{ 
  Xpose can be run in two different ways, using the classic menu system
  and by using the command line in R. 
}
\section{Running Xpose with the classic menu system}{
  Xpose can be run
  through a series of menu selections.  This method of running Xpose
  is useful for the beginning "Xposer" and for learning about the
  functionality available in Xpose.  The menu system does not allow
  for as much customization as the command line interface, nor does it
  allow for scripting.
  
  To run the classic Xpose interface, at the R command prompt,
  type:

  \code{library(xpose4) #read in the xopse library}\cr
  \code{xpose4() #Run the menu system}

  And follow the menu prompts.
}

\section{Running Xpose from the command line}{
  Each function in the
  Xpose bundle is now independently available from the command
  line, once libraries are loaded. For example, you might do the
  following: 
  
  \code{library(xpose4) #read in the xopse library}\cr
  \code{run.number <- 5 # run number of xpose table files}\cr
  \code{xpdb <- xpose.data(run.number) # run number of xpose table
    files}\cr
  \code{basic.gof(xpdb) # Basic goodness-of-fit plots} 
}

  
%   key functionality

%   -mirror
%   -vpc
%   -cwres

%   -nonmem naming convention

\section{Getting Help}{
  
  All Xpose 4.0 functions are documented within the R help system.
  For example, if you want to know more about DV vs PRED plots and their
  options, type at the R command line after loading Xpose:

  \code{?dv.vs.pred}

  You can also use the html help, in the R-gui go to (Help -> Html help)
  then click on packages, then xpose4.

  If you don't know the names of the Xpose function you would like to
  use then you can browse the four different libraries that make up the
  Xpose 4 distribution.  These libraries are:
  
  \code{\link[xpose4data:xpose4data-package]{xpose4data}} - 
  Functions to to read NONMEM table files and create the Xpose data
  structure
  
  \code{\link[xpose4generic:xpose4generic-package]{xpose4generic}} -  
  Generic functions to create plots using the Xpose data structure.
  
  
  \code{\link[xpose4specific:xpose4specific-package]{xpose4specific}}
  - 
  Specific functions to create plots using the Xpose data
  structure.  Generally what you want to use.
  
  \code{\link[xpose4classic:xpose4classic-package]{xpose4classic}}
  - 
  Functions for running the classic menu system in Xpose.

  
  \code{\link[xpose4specific:xpose4specific-package]{xpose4specific}}
  may be the most helpful as it
  contains functions that are ready to use just by telling the functions
  which xpose database to use (i.e. \code{xpdb} after running the
  command \code{xpdb <- xpose.data(run.number)}).
  
   
}
\author{E. Niclas Jonsson, Mats Karlsson, Justin Wilkins and Andrew Hooker}
\keyword{methods}
\keyword{package}
