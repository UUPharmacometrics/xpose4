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

\name{change.graphical.parameters}
\alias{change.misc.graph.par}
\alias{change.ab.graph.par}
\alias{change.lm.graph.par}
\alias{change.smooth.graph.par}
\alias{change.label.par}
\alias{change.bw.graph.par}
\alias{change.dil.graph.par}
\alias{change.pi.graph.par}
\alias{change.cond.graph.par}
\title{Functions changing variable definitions in Xpose 4}
\description{
  These functions allow customization of Xpose's graphics settings.
}
\usage{                                   
change.ab.graph.par(object, 
                    classic = FALSE) 
                
change.bw.graph.par(object, 
                    classic = FALSE)
                
change.cond.graph.par(object, 
                      classic = FALSE)
                
change.dil.graph.par(object, 
                     classic = FALSE)
                
change.label.par(object,
                 classic = FALSE)
                                      
change.lm.graph.par(object, 
                    classic = FALSE) 
                
change.misc.graph.par(object, 
                      classic = FALSE) 
                
change.pi.graph.par(object, 
                    classic = FALSE)

change.smooth.graph.par(object,  
                        classic = FALSE)     
}
\arguments{
  \item{object}{An \code{xpose.data} object. }
  \item{classic}{A logical operator specifying whether the function should 
                 assume the classic menu system. This is an internal option 
                 and need never be called from the command line.}
}
\details{
  These functions are used to customize graphics settings, the way Xpose
  draws its graphs. \code{change.misc.graph.par} sets basic graphics parameters,
  including plot type, point type and size, colour, line type, and line width.
  \code{change.ab.graph.par} is used to change settings for the line of identity,
  \code{change.lm.graph.par} is responsible for linear regression lines, 
  \code{change.smooth.graph.par} sets preferences for loess smooths, \code{change.bw.graph.par}
  sets preferences for box-and-whisker plots, and \code{change.label.par},
  \code{change.dil.graph.par} and \code{change.pi.graph.par} functions are responsible 
  for labelling, dilution and prediction interval plotting preferences, respectively. 
  \code{change.cond.graph.par} sets preferences for conditioning.
  
  Settings can be saved and loaded using \code{\link{export.graph.par}} and 
  \code{\link{import.graph.par}}, respectively.
}
\value{
  An \code{\link{xpose.data}} object (classic == FALSE) or null (classic == TRUE).
}
\author{Niclas Jonsson & Justin Wilkins}
\seealso{\code{\link{xpose.plot.default}},\code{\link{xpose.panel.default}},
         \code{\link{xpose.plot.bw}},\code{\link{xpose.panel.bw}},
         \code{\link{xpose.plot.default}},\code{\link{import.graph.par}},
         \code{\link{export.graph.par}},\code{\link{plot.default}},
         \code{\link{par}},\code{\link{import.graph.par}},\code{\link[lattice]{panel.abline}},
         \code{\link[lattice]{panel.lmline}},\code{\link{lm}},\code{\link[lattice]{panel.loess}},
         \code{\link{loess.smooth}},\code{\link{loess}},\code{\link[lattice]{panel.bwplot}},
         \code{\link[lattice]{shingle}},\code{reorder.factor}
}
\examples{
\dontrun{
## xpdb5 is an Xpose data object
## We expect to find the required NONMEM run and table files for run
## 5 in the current working directory
xpdb5 <- xpose.data(5)

## Change default miscellaneous graphic preferences
xpdb5 <- change.misc.graph.par(xpdb5)

## Change default linear regression line preferences, creating a new 
## object
xpdb5.a <- change.lm.graph.par(xpdb5)

## Change conditioning preferences
xpdb5 <- change.cond.graph.par(xpdb5)
}
}
\keyword{methods}
