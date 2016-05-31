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

\name{xpose.data-class}
\docType{class}
\alias{xpose.data-class}
\alias{numeric_or_NULL-class}
\alias{data.frame_or_NULL-class}

\title{Class xpose.data}
\description{The xpose.data class is the fundamental data object in
  Xpose 4. It contains the data and preferences used in the creation of
  the Xpose plots and analyses.}
\section{Objects from the Class}{
  Objects are most easily created by the \code{xpose.data} function, whcih
  reads the appropriate NONMEM table files and populates the slots of
  the object.
}
\section{Slots}{
  \describe{
    \item{\code{Runno:}}{Object of class \code{"character or numeric"}
      The run number identifying the NONMEM run from which the data in
      the object comes from.}
    \item{\code{Data}:}{Object of class \code{"data.frame or NULL"} This
    slot holds the (merged) data, in the form of a regular data.frame,
    from the NONMEM table files generated from the NONMEM run. It can be
    accessed and set using the \code{\link{Data}} function.}
    \item{\code{SData}:}{Object of class \code{"data.frame or NULL"} A
      data.frame of any data simulated from the model. It can be
    accessed and set using the \code{\link{SData}} function.The number of
      rows should be an even multiple of the rows in Data and the column
    names should be exactly the same as in Data, except that one extra
    column should be present ("iter"). The extra column consists of integers
    that identifies the iteration the row belongs to.}
    \item{\code{Nsim}:}{Object of class \code{"numeric or NULL"} A
      number that indicates the number of simulated data sets in SData.}
    \item{\code{Prefs}:}{Object of class \code{"xpose.prefs"} An object
      that contains a number of data and graphical preferences for the
      xpose.data object.}
    \item{\code{Doc}:}{Object of class \code{"character or NULL"}
      A short piece of text describing the model.}
  }
}
\section{Methods}{
  None yet.
}
\author{Niclas Jonsson }
\note{The is a validation function for the object called \code{test.xpose.data}.}
\seealso{
  \code{\link{xpose.data}},
  \code{\link{Data}},
  \code{\link{SData}}
  \code{\link{read.nm.tables}}, \code{\link{xpose.prefs-class}}
}
\keyword{classes}

