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

\name{summary}
\alias{parm.summary}
\alias{cov.summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summarize individual parameter values and covariates}
\description{
  These functions produce tables, printed to the screen, summarizing the
  individual parameter values or covariates from a dataset in Xpose 4.
}
\usage{
cov.summary(object,
            onlyfirst=TRUE,
            subset=xsubset(object),
            inclZeroWRES=FALSE,
            out.file=".screen", 
            main="Default",
            fill = "gray",
            values.to.use=xvardef("covariates",object),
            value.name="Covariate",
            ...)
                          
parm.summary(object,
            onlyfirst=TRUE,
            subset=xsubset(object),
            inclZeroWRES=FALSE,
            out.file=".screen",
            main="Default",
            fill = "gray",
            values.to.use=xvardef("parms",object),
            value.name="Parameter",
            max.plots.per.page=1,
            ...)
              
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An xpose.data object.}
  \item{onlyfirst}{Logical value indicating if only the first row
    per individual is included in the plot. }
  \item{inclZeroWRES}{Logical value indicating whether rows with WRES=0
    are included in the plot. The default is FALSE. }
  \item{out.file}{Where the results should be output to.  Can be
    ".screen", ".ask", ".graph" or a filename in quotes.}
  \item{subset}{A string giving the subset expression to be applied to
    the data before plotting. See \code{\link{xsubset}}.}
  \item{main}{The title of the plot.  If \code{"Default"} then a default
    title is plotted. Otherwise the value should be a string like
    \code{"my title"} or \code{NULL} for no plot title.  For
    \code{"Default"} the function
    \code{\link{xpose.multiple.plot.title}} is used.}
  \item{fill}{The color to fill the boxes in the table if the table is
    printed to ".graph"}
  \item{values.to.use}{Which values should be summarized}
  \item{value.name}{The name of the values}
  \item{max.plots.per.page}{Maximum plots per page.}
  \item{\dots}{Other arguments passed to \code{Data} and \code{SData}.}
}

\value{
  Returned is the matrix of values from the table. \code{parm.summary} and
  \code{cov.summary} produce summaries of parameters and covariates,
  respectively. \code{parm.summary} produces less attractive output but supports
  mirror functionality.

   \code{parm.summary} and  \code{cov.summary} utilize
   \code{\link[Hmisc]{print.char.matrix}} to print the information to
   the screen.
}
\author{Andrew Hooker & Justin Wilkins}

\seealso{
  \code{\link{Data}},
  \code{\link{SData}},
  \code{\link{xpose.data-class}},
  \code{\link[Hmisc]{print.char.matrix}}
}
\examples{
\dontrun{
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)
}

## Here we load the example xpose database from the xpose4data package
data(simpraz.xpdb)
xpdb <- simpraz.xpdb

parm.summary(xpdb)

cov.summary(xpdb)
}
\keyword{methods}
