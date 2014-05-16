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

\name{data.checkout}
\alias{data.checkout}
\title{Check through the source dataset to detect problems}
\description{
  This function graphically "checks out" the dataset to identify errors 
  or inconsistencies.
}
\usage{
data.checkout(obj=NULL, 
           datafile = ".ask.", 
           hlin = -99,
           dotcol = "black",
           dotpch = 16,
           dotcex = 1,
           idlab="ID",
           csv=NULL,
           main="Default",
           ...)
}
\arguments{
  \item{obj}{NULL or an xpose.data object.}
  \item{datafile}{A data file, suitable for import by \code{\link{read.table}}.}
  \item{hlin}{An integer, specifying the line number on which the column
              headers appear.}
  \item{dotcol}{Colour for the dots in the dotplot. If obj is an xpose
    data object then the default is to use 
    the same value as defined for box-and-whisker plots.}  
  \item{dotpch}{Plotting character for the dots in the dotplot. If obj
    is an xpose data object then the default is to use  
    the same value as defined for box-and-whisker plots.} 
  \item{dotcex}{Relative scaling for the dots in the dotplot.
    If obj is an xpose
    data object then the default is to use 
    the same value as defined for box-and-whisker plots.}
  \item{idlab}{The ID column label in the dataset.  Input as a text
    string.}
  \item{csv}{Is the data file in CSV format (comma separated values)?
    If the value is \code{NULL} then the user is asked at the command
    line.  If supplied to the function the value can be \code{TRUE/FALSE}.}
  \item{main}{The title to the plot. "default" means that Xpose creates
    a title.}
  \item{\dots}{Other arguments passed to \code{link[lattice]{dotplot}}.}           
}
\details{
  This function creates a series of \code{dotplots}, one for each variable
  in the dataset, aginst individual ID. Outliers and clusters may easily be
  detected in this manner. 
}
\value{
  A stack of dotplots.
}
\author{Niclas Jonsson, Andrew Hooker & Justin Wilkins}

\seealso{
     \code{\link[lattice]{dotplot}},
     \code{\link{xpose.prefs-class}},
     \code{\link{read.table}}
}
\examples{
\dontrun{
## We expect to find the required NONMEM run, table and data files for run
## 5 in the current working directory 
xpdb5 <- xpose.data(5)

data.checkout(xpdb5, datafile = "mydata.dta")
data.checkout(datafile = "mydata.dta")
}
}
\keyword{methods}
