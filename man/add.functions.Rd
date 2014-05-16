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

\name{add.functions}
\alias{add.absval}
\alias{add.dichot}
\alias{add.exp}
\alias{add.log}
\alias{add.tad}
\title{Column-transformation functions for Xpose 4}
\description{
  These functions transform existing Xpose 4 data columns, adding new columns.
}
\usage{
add.absval(object, 
           listall = TRUE, 
           classic = FALSE) 

add.dichot(object, 
           listall = TRUE, 
           classic = FALSE)
        
add.exp(object, 
        listall = TRUE, 
        classic = FALSE)
        
add.log(object, 
        listall = TRUE, 
        classic = FALSE)    
        
add.tad(object,  
        classic = FALSE)

}
\arguments{
  \item{object}{An \code{xpose.data} object. }
  \item{listall}{A logical operator specifying whether the items in the database should be listed. }
  \item{classic}{A logical operator specifying whether the function should assume the classic menu system. 
                 This is an internal option and need never be called from the command line.}
}
\details{
  These functions may be used to create new data columns within the Xpose
  data object by transforming existing ones. \code{add.absval} creates a column
  reflecting the absolute value of a column, \code{add.dichot} creates a 
  categorical data column based upon a continuous variable, \code{add.exp} 
  creates an exponentiated version of an existing variable, \code{add.log}
  provides log transformation, and \code{add.tad} creates a time-after-dose
  (TAD) data item based upon the dose and time variables in the dataset. 
}
\value{
  An \code{\link{xpose.data}} object (classic == FALSE) or null (classic == TRUE).
}
\author{Niclas Jonsson & Justin Wilkins}
\seealso{
  \code{\link{xpose.data}}
}
\examples{
\dontrun{
## xpdb5 is an Xpose data object
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)

## Create a column containing the absolute values of data in another 
## column
add.absval(xpdb5)

## Create a categorical data column based on a continuous data column, 
## and do not list variables 
add.dichot(xpdb5, listall = FALSE)

## Create a column containing the exponentiated values of data in 
## another column
add.exp(xpdb5)

## Create a column containing log-transformations of data in another 
## column
add.log(xpdb5)

## Create a time-after-dose column
add.tad(xpdb5)
}
}
\keyword{methods}
