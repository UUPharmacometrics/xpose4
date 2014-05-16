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

\name{xpose.create.title}
\alias{xpose.create.title}
\alias{xpose.create.label}
\alias{xpose.create.title.hist}
\alias{xpose.multiple.plot.title}

\title{Functions to create labels for plots}
\description{
  Functions to create labels for plots
}
\usage{
xpose.create.title(x,y,object,subset=NULL,funx=NULL,funy=NULL,
                               no.runno=FALSE,...)

xpose.create.label(x, object, fun, logx,
                   autocorr.x = FALSE,
                   autocorr.y = FALSE, ...)

xpose.create.title.hist(x, object, subset, ...)

xpose.multiple.plot.title(object, plot.text,
                          subset = xsubset(object),
                          main = "Default",
                          no.runno = FALSE,
                          ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Column name for x-variable}
  \item{y}{Column name for y variable}
  \item{object}{Xpose data object}
  \item{subset}{Subset used for plot}
  \item{fun}{Function applied to data}
  \item{funx}{Function applied to x data}
  \item{funy}{Function applied to y data}
  \item{no.runno}{should we include a run number in the label}
  \item{logx}{is the data log trasformed?}
  \item{autocorr.x}{Are we looking at an autocorrelation plot?}
  \item{autocorr.y}{Are we looking at an autocorrelation plot?}
  \item{plot.text}{Text of plot title}
  \item{main}{If \code{"Default"} then \code{plot.text} is the default
    title with the run number and subset expression added to the
    title. If the value is anything other than \code{"Default"} then
    that value is used as the plot title.}
  \item{\dots}{additional arguments passed to the function.}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  Plot titles and labels.
}
\author{Andrew Hooker}
\keyword{methods}% __ONLY ONE__ keyword per line
