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
\name{dOFV.vs.id}
\alias{dOFV.vs.id}
\title{
Change in Objective function value vs. removal of individuals.
}
\description{
A plot showing the most and least influential individuals in
determinning a drop in OFV between two models.
}
\usage{
dOFV.vs.id(xpdb1,
             xpdb2,
             sig.drop=-3.84,
             decrease.label.number=3,
             increase.label.number=3,
             id.lab.cex=0.6,
             id.lab.pos=2,
             type="o",
             xlb="Number of subjects removed",
             ylb=expression(paste(Delta,"OFV")),
             main="Default",
             sig.line.col = "red",
             sig.line.lty = "dotted",
             tot.line.col = "grey",
             tot.line.lty = "dashed",
             key=list(columns = 1,
                    lines = list(pch = c(super.sym$pch[1:2],NA,NA),
                    type=list("o","o","l","l"),
                    col = c(super.sym$col[1:2],sig.line.col,tot.line.col),
                    lty = c(super.sym$lty[1:2],sig.line.lty,tot.line.lty)
                    ),
                    text = list(c(
                    expression(paste(Delta, OFV[i] < 0)),
                    expression(paste(Delta, OFV[i] > 0)),
                    expression(paste("Significant  ",Delta, OFV)),
                    expression(paste("Total  ",Delta, OFV))
                    )),
                    corner=c(0.95,0.5),border=T
             ),
             ...)

}
\arguments{
  \item{xpdb1}{
Xpose data object for first NONMEM run ("new" run)
}
  \item{xpdb2}{
Xpose data object for Second NONMEM run ("reference" run)
}
  \item{sig.drop}{
What is a significant drop of OFV?
}
  \item{decrease.label.number}{
How many points should bw labeled with ID values for those IDs with a
drop in iOFV?
}
  \item{increase.label.number}{
How many points should bw labeled with ID values for those IDs with an
increase in iOFV? 
}
  \item{id.lab.cex}{
Size of ID labels.
}
  \item{id.lab.pos}{
ID label position.
}
  \item{type}{
Type of lines.
}
  \item{xlb}{
X-axis label.
}
  \item{ylb}{
Y-axis label.
}
  \item{main}{
Title of plot.
}
  \item{sig.line.col}{
Significant OFV drop line color.
}
  \item{sig.line.lty}{
Significant OFV drop line type.
}
  \item{tot.line.col}{
Total OFV drop line color.
}
  \item{tot.line.lty}{
Total OFV drop line type.
}
  \item{key}{
Legend for plot.
}
  \item{\dots}{
Additional arguments to function.
}
}
\author{
Andrew C. Hooker
}
\examples{
\dontrun{
library(xpose4)

## first make sure that the iofv values are read into xpose
cur.dir <- getwd()
setwd(paste(cur.dir,"/LAG_TIME",sep=""))
xpdb1 <- xpose.data(1)
setwd(paste(cur.dir,"/TRANSIT_MODEL",sep=""))
xpdb2 <- xpose.data(1)
setwd(cur.dir)

## then make the plot
dOFV.vs.id(xpdb1,xpdb2)
}
}

\keyword{ methods }

