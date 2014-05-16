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

\name{xpose.multiple.plot.default}
\alias{xpose.multiple.plot.default}
\title{Xpose 4 generic function for plotting multiple lattice objects on
one page}
\description{ 
  Function takes a list of \pkg{lattice} plot objects and prints them in
  a multiple plot layout with a title.
}
\usage{
xpose.multiple.plot.default(plotList,
           plotTitle=NULL,
           prompt=FALSE,
           new.first.window=FALSE,
           max.plots.per.page=4, # absolute max is 9
           title    = list(
             title.x = unit(0.5, "npc"),
             title.y = unit(0.5, "npc"),
             title.gp= gpar(cex=1.2,fontface="bold"),
             title.just = c("center","center")
             ),
           mirror=FALSE,
           bql.layout=FALSE,
           page.numbers=TRUE,
           ...)

}

\arguments{
  \item{plotList}{A list of lattice plot objects such that plot object i
    can be called with \code{plotList[[i]]}}
  \item{plotTitle}{The title used for the multiple plot layout}
  \item{prompt}{If more than one page is needed do you want a prompt at
    the command line before the next page is printed}
  \item{new.first.window}{Should the first page of this plot be in the
    already opened window or should a new windowq be created}
  \item{max.plots.per.page}{Maximum number of plots per page in the
    mulltiple layout}
  %\item{title.size}{Amount, in a range of 0-1, of how much space the
  %  title should take up in the plot)}
  \item{title}{Look of title using \pkg{grid}.}
  \item{mirror}{if the list contains mirror plots}
  \item{bql.layout}{should we use layout optimized for BQL
    measurements?}
  \item{page.numbers}{Should we add page numbers to multiple page plots?}
  \item{\dots}{Other arguments passed to the code in this function}
}
\details{
  \strong{Additional arguments:}
  \describe{
    \item{title.x}{Where the title should be placed in the title \pkg{grid} region}
    \item{title.y}{Where the title should be placed in the title \pkg{grid} region}
    \item{title.just}{how the title should be justified}
    \item{title.gp}{The par parameters for the title (see \pkg{grid})}
  }
}
\value{
  returns nothing
}
%\references{ ~put references to the literature/web site here ~ }
\author{Andrew Hooker}
%\note{ 
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{
  \pkg{grid},
  \code{\link{basic.gof}},
  \code{\link{parm.vs.parm}},
  \code{\link{parm.vs.cov}},
}
%\examples{
%}
\keyword{methods}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
