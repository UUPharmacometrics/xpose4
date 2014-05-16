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

\name{xpose.data}
\alias{xpose.data}
\title{Creates an Xpose data object}
\description{
  Creates an \code{xpose.data} object.
}
\usage{
           xpose.data(runno,
                      tab.suffix="",
                      sim.suffix="sim",
                      cwres.suffix="",
                      directory="",
                      quiet=TRUE,
                      table.names=c("sdtab","mutab","patab","catab",
                        "cotab","mytab","extra","xptab","cwtab"),
                      cwres.name=c("cwtab"),
                      mod.prefix="run",
                      mod.suffix=".mod",
                      phi.suffix=".phi",
                      phi.file=NULL,
                      nm7=NULL,  
                      ...)
}
\arguments{
  \item{runno}{Run number of the table files to read. }
  \item{tab.suffix}{Suffix to be appended to the table file names for
    the "real" data.}
  \item{sim.suffix}{Suffix to be appended to the table file names for
    any simulated data.}
  \item{cwres.suffix}{Suffix to be appended to the table file names for
    any CWRES data.}
  \item{directory}{Where the files are located.}
  \item{quiet}{A logical value indicating if more diagnostic messages
    should be printed when running this function.}
  \item{table.names}{Default text that Xpose looks for when searching
    for table files.}
  \item{cwres.name}{default text that xpose looks for when searching for
    CWRES table files.}
  \item{mod.prefix}{Start of model file name.}
  \item{mod.suffix}{End of model file name.}
  \item{phi.suffix}{End of \.phi file name.}
  \item{phi.file}{The name of the \.phi file. If not \code{NULL} then
    supercedes \code{paste(mod.prefix,runno,phi.suffix,sep="")}.}
  \item{nm7}{\code{T/F} if table files are for NONMEM 7/6, NULL for undefined.}
  \item{\dots}{Extra arguments passed to function.}
}
\details{
  Xpose expects, by default, to find the following NONMEM tables in the working
  directory to be able to create an Xpose data object (using a run number of 5
  as an example):
  
  sdtab5: The 'standard' parameters, including IWRE, IPRE, TIME, and the 
          NONMEM default items (DV, PRED, RES and WRES) that are added when 
          NOAPPEND is not present in the \$TABLE record.
          
          \code{\$TABLE ID TIME IPRE IWRE 
                  NOPRINT ONEHEADER FILE=sdtab5}
  
  patab5: The empirical Bayes estimates of individual model parameter values,
          or posthoc estimates. These are model parameters, such as CL, V2,
          ETA1, etc.
          
          \code{\$TABLE ID CL V2 KA K F1 ETA1 ETA2 ETA3
                  NOPRINT NOAPPEND ONEHEADER FILE=patab5 }
       
  catab5: Categorical covariates, e.g. SEX, RACE.
  
          \code{\$TABLE ID SEX HIV GRP
                  NOPRINT NOAPPEND ONEHEADER FILE=catab5 }
  
  cotab5: Continuous covariates, e.g. WT, AGE.
  
          \code{\$TABLE ID WT AGE BSA HT GGT HB
                  NOPRINT NOAPPEND ONEHEADER FILE=cotab5 }
  
  mutab5, mytab5, extra5, xptab5: Additional variables of any kind. These might
          be useful if there are more covariates than can be accommodated in
          the covariates tables, for example, or if you have other variables
          that should be added, e.g. CMAX, AUC.
          

  The default names for table files can be changed by changing the
  dafault values to the function.  The files that Xpose looks for by
  default are:
  \preformatted{
      paste(table.names, runno, tab.suffix, sep="")
  }

  The default CWRES table file name is called:
  \preformatted{
      paste(cwres.name,runno,cwres.suffix,tab.suffix,sep="")
  }

  If there are simulation files present then Xpose looks for the files
  to be named:
  \preformatted{
    paste(table.names, runno, sim.suffix, tab.suffix, sep="")
    paste(cwres.name,runno,sim.suffix,cwres.suffix,tab.suffix,sep="")
  }
  
  This is basically a wrapper function for the \code{read.nm.tables},
  \code{Data} and \code{SData} functions. See them for further
  information.

  Also reads in the \.phi file associated with the run (Individual OFVs,
  parameters, and variances of those parameters.)
}
\value{
  An \code{xpose.data} object.
}
\author{Niclas Jonsson, Andrew Hooker}
\seealso{
\code{\link{xpose.data-class}},
\code{\link{Data}},
\code{\link{SData}},
\code{\link{read.nm.tables}},
\code{\link{compute.cwres}}}
\examples{

## We expect to find the required NONMEM run and table files for run
## 1 in the current working directory, using default Xpose format
## Here we create files from an example NONMEM run
simprazExample(overwrite=FALSE)
xpdb <- xpose.data(1) 

\dontrun{
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory, and that the table files have
## a suffix of '.dat', e.g. sdtab5.dat

xpdb5 <- xpose.data(5, tab.suffix = ".dat") 
}
}
\keyword{methods}
