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

\name{Data}
\alias{Data}
\alias{SData}
\alias{Data<-}
\alias{SData<-}
\title{Extract data from an xpose.data object.}
\description{
  Extracts the data from the Data or SData slots in an "xpose.data" object.
}
\usage{
Data(object,
     inclZeroWRES = FALSE,
     onlyfirst = FALSE,
     subset = NULL)

SData(object,
      inclZeroWRES = FALSE,
      onlyfirst = FALSE,
      subset = NULL,
      samp = NULL)

Data(object,quiet=TRUE,keep.structure=F)  <- value
SData(object) <- value
}
\arguments{
  \item{object}{An "xpose.data" object}
  \item{inclZeroWRES}{Logical value indicating whether rows with WRES==0
    should be included in the extracted data.}
  \item{onlyfirst}{Logical value indicating whether only the first line
    per individual should be included in the extracted data.}
  \item{subset}{Expression with which the extracted data should be
    subset (see \code{\link{xsubset}})}
  \item{samp}{An integer between 1 and object@Nsim
    (see\code{\link{xpose.data-class}}) specifying which of the
    simulated data sets to extract from SData.}
  \item{quiet}{\code{TRUE or FALSE} if\code{FALSE} then some more
    information is printed out when adding data to an Xpose object.}
  \item{keep.structure}{\code{TRUE or FALSE} if\code{FALSE} then values
    are converted to continuous or categorical according to the rules
    set up by xpose using object@Prefs@Cat.levels,
    object@Prefs@DV.cat.levels and the values in the "catab" file.}
  \item{value}{An R data.frame.}
}
\details{

  When using Data to assign a data.frame to the Data slot in the
  "xpose.data" object a number of things happen:

  Each column in the data.frame is checked and set to factor if
  the number of unique values are less than the value of
  Cat.levels (see \code{\link{xpose.prefs-class}}).

  It is checked which of the predefined xpose data
  variables that exists in the data.frame. The variable defintions that
  does not exist are set to NULL.

  The column identified by the \code{dv} xpose variable definition, is
  checked and set to factor if the number of unique values are less than
  or equal to the DV.Cat.levels (see \code{\link{xpose.prefs-class}}).

  Finally, each column name in the data.frame is checked for a label
  (see \code{\link{xpose.prefs-class}}). If it is non-existent, the
  label is set to the column name.


  When SData is used to assign a data.frame to the SData slot it is
  first checked that the number of rows in the SData data.frame is an
  even multiple of the number of rown in Data. Next, each column in the
  SData data.frame is assigned the same class as the corresponding
  column in the Data data.frame (it is required that the columns are the
  same in Data and SData). Finally, an extra column, "iter", is added to SData,
  which indicates the iteration number that each row belongs to. At the
  same time, the Nsim slot of the "xpose.data" object is set to the
  number of iterations (see \code{\link{nsim}}).

}
\value{
  Returns a data.frame from the Data or SData slots, excluding rows as
  indicated by the arguments.
}
\author{Niclas Jonsson }
\examples{
\dontrun{
## xpdb5 is an Xpose data object
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)

## Extract data
my.dataframe <- Data(xpdb5, subset="TIME < 24")

## Assign data
Data(xpdb5) <- my.dataframe

## Extract simulated data
my.simulated.dataframe <- SData(xpdb5)

## Assign simulated data
SData(xpdb5) <- my.simulated.dataframe
}
}
\seealso{\code{\link{xpose.data-class}},\code{\link{xpose.prefs-class}} }
\keyword{methods}
