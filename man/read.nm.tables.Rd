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

\name{read.nm.tables}
\alias{read.nm.tables}
\title{Reading NONMEM table files}
\description{
Reads one or more NONMEM table files, removes duplicated columns and
merges the data into a data.frame.
}
\usage{
read.nm.tables(table.files = NULL,
               runno = NULL,
               tab.suffix = "",
               table.names = c("sdtab", "mutab", "patab",
                               "catab", "cotab", "mytab",
                               "extra", "xptab"),
               cwres.name = c("cwtab"),
               cwres.suffix = "",
               quiet = FALSE,
               ...)
}
\arguments{
  \item{table.files}{Exact names of table files to read.  If not
    provided then the exact names are created using the other arguments
    to the function.}
  \item{runno}{Run-number to identify sets of table files.}
  \item{tab.suffix}{Table file name suffix.}
  %\item{sim.suffix}{Simulation table file name suffix.}
  \item{table.names}{Vector of template table file names to read.}
  \item{cwres.name}{Vector of CWRES table file names to read.}
  \item{cwres.suffix}{CWRES table file name suffix.}
  \item{quiet}{Logical value to indicate whether some warnings should be quiet
    or not.}
  \item{\dots}{Additional arguments passed to this function}
}
\details{
  Reads one or more table files, removes duplicate columns and merges
  the data. The function also checks to see if the table files are of the
  same length (required).

  If there are header lines in the table files (for example if your data
  are simulated with NSUB>1), these are removed.

  The table file names to read are constructed from the file name
  templates of \code{table.names}. The \code{runno} and
  \code{tab.suffix} are appended to the file name template before
  checking if the file is readable.
  
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
}
\value{
  A data.frame.
}
\author{Niclas Jonsson, Andrew Hooker}
\seealso{
\code{\link{xpose.data-class}},
\code{\link{compute.cwres}}
}
\examples{
\dontrun{
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory, and that the table files have
## a suffix of '.dat', e.g. sdtab5.dat

my.dataframe <- read.nm.tables(5, tab.suffix = ".dat") 
}

}
\keyword{methods}
