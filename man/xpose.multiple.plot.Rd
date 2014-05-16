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
\name{xpose.multiple.plot}
\alias{xpose.multiple.plot}

\title{
Create and object with class "xpose.multiple.plot".
}
\description{
Create and object with class "xpose.multiple.plot".
}
\usage{
xpose.multiple.plot(plotList,
           plotTitle=NULL,
           nm7 = TRUE,
           prompt=FALSE,
           new.first.window=FALSE,
           max.plots.per.page=4,                   
           title    = list(
             title.x = unit(0.5, "npc"),
             title.y = unit(0.5, "npc"),
             title.gp= gpar(cex=1.2,fontface="bold"),#,font=2),
             title.just = c("center","center")
             ),
           mirror=FALSE,
           bql.layout=FALSE,
           ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plotList}{
A list of lattice plots.
}
  \item{plotTitle}{
Main title for plots.
}
  \item{nm7}{
\code{TRUE} if we are using NONMEM 7
}
  \item{prompt}{
When printing should we prompt for each new page in plot?
}
  \item{new.first.window}{
\code{TRUE} or \code{FALSE}.
}
  \item{max.plots.per.page}{
A number.  Max value is 9.
}
  \item{title}{
Title properties.
}
  \item{mirror}{
Are there mirror plots in plot list?
}
  \item{bql.layout}{
Should we use layout optimized for plots with BQL (below limit of quantification) measurements?
}
  \item{\dots}{
Additional options passed to function.
}
}
\value{
An object of class "xpose.multiple.plot".
}
\author{
Niclas Jonsson and Andrew C. Hooker
}

\seealso{
  \code{\link{print.xpose.multiple.plot}},
  \code{\link{xpose.multiple.plot.default}} 
}
\examples{
\dontrun{
## See code for cwres.vs.cov() by typing

library(xpose4specific)
cwres.vs.cov

}
}

\keyword{ methods }

