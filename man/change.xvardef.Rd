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

\name{change.xvardef}
\Rdversion{1.1}
\alias{change.xvardef}
\alias{change.xvardef<-}

\title{
Change Xpose variable definitions.
}
\description{
  These functions allow for the changing of Xpose variable definitions
  like "idv" and "dv". These variable definitions are used to refer to
  columns of the observed data in a generic way, so that generic
  plotting functions can be created.
}
\usage{
change.xvardef(object,var = ".ask", def = ".ask", listall = TRUE,
               classic = FALSE, check.var = FALSE, ...)

change.xvardef(object,
               var,
               listall=FALSE,
               classic=FALSE,
               check.var=FALSE,
               ...) <- value

}
\arguments{
  \item{object}{
    An \code{xpose.data} object.
  }
  \item{var}{
    The Xpose variable you would like to change or add to the current
    object. A one-element character vector (e.g. \code{"idv"}).  If
    \code{".ask"} the user will be prompted to input a value.
  }
  \item{def}{
    A vector of column names from NONMEM table files
    (\code{names(object@Data)}) that should be associated with this
    variable (e.g. \code{c("TIME")}).  Mutiple values are allowed. If
    \code{".ask"} the user will be prompted to input values.
  }
  \item{value}{
    Same as \code{def} defined above. 
  }
  \item{listall}{
    Should the function list the database values?
  }
  \item{classic}{
    Is the function being used from the classic interface.  This is an
    internal option.
  }
  \item{check.var}{
    Should the variables be checked against the current variables in the object?
  }
  \item{\dots}{
    Items passed to functions within this function.
  }
}

\section{Additional arguments}{
  \strong{The default xpose variables are:\cr}
  \describe{
    \item{id}{Individual identifier column in dataset}
    \item{idlab}{values used for plotting ID values on data points in
      plots}
    \item{occ}{The occation variable}
    \item{dv}{The dv variable}
    \item{pred}{The pred variable}
    \item{ipred}{The ipred variable}
    \item{wres}{The wres variable}
    \item{cwres}{The cwres variable}
    \item{res}{The res variable}
    \item{parms}{The parameters in the database}
    \item{covariates}{The covariates in the database}
    \item{ranpar}{The random parameters in the database}
  }
}
\value{
If called from the the command line then this function returns an xpose
database.  If called from the classic interface this function updates
the current xpose database (.cur.db).
}
\author{
Andrew Hooker
}

\section{Acknowledgements}{
  Thanks to Sebastien Bihorel for input which helped create this function.
}

\seealso{
  \code{\link{xvardef}},
  \code{\link{xpose.data}}
}
\examples{
\dontrun{
# Change the "id" variable to point to "PRED" in the xpose object
xpdb <- change.xvardef(xpdb,var="id",def="PRED")

# Check the value of the "id" variable
xvardef("id",xpdb)

# Change the "idv" variable
change.xvardef(xpdb,var="idv") <- "TIME"

# Change the covariate scope
change.xvardef(xpdb,var="covariates") <- c("SEX","AGE","WT")

# Use the interactive capabilities of the function
xpdb <- change.xvardef(xpdb)
}
}
\keyword{methods}
